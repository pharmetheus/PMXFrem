#' Filter a dataset based on IGNORE/ACCEPT list statements in a NONMEM model file.
#'
#' This function reads a NONMEM model file, extracts any list-based IGNORE or ACCEPT
#' conditions from the $DATA record (e.g., IGNORE=(SEX.EQ.1)), and applies them
#' to a provided data frame.
#'
#' NOTE: This function does NOT handle single-character IGNORE statements (e.g.,
#' IGNORE=@, IGNORE=C). If such a statement is detected, a warning will be issued
#' unless suppressed.
#'
#' @param baseModelFile The path to the NONMEM model file (.mod).
#' @param wideData A "wide" format data frame to be filtered.
#' @param quiet Logical. If TRUE, suppresses the warning for single-character
#'   IGNORE statements. Defaults to FALSE.
#'
#' @param rawData Optional data frame with the data values as written in the
#'   file (every column character), the same number of rows as `wideData` and
#'   at least its columns. See Details. Defaults to `NULL`.
#'
#' @details Following the NONMEM 7.5 help (`$DATA`, `IGNORE=(list)`): with
#'   `=`, `==`, `/=`, `.EQ.` and `.NE.` the value in the data record and the
#'   value in the list are compared **as character strings**, so a data value
#'   written `1.0` does not satisfy `.EQ.1`; with `.EQN.`, `.NEN.` and the
#'   ordering operators (`>`, `.GT.`, `>=`, `.GE.`, `<`, `.LT.`, `<=`, `.LE.`)
#'   they are compared numerically. A record that satisfies at least one
#'   condition of the list is dropped (`IGNORE`) or accepted (`ACCEPT`).
#'
#'   Text comparison needs the values as written. The callers that read a data
#'   file pass them in `rawData`. When `rawData` is `NULL` (the data came as a
#'   data frame) the text is `as.character()` of the values, an approximation:
#'   a numeric `1.0` reads `"1"`, and so does `1`.
#'
#' @return A data frame containing the data after all applicable IGNORE/ACCEPT
#'   conditions have been applied.
#'
#' @family Data Assembly Internal
#' @concept data_assembly
#' @keywords internal
filterDataFromModel <- function(baseModelFile, wideData, quiet = FALSE, rawData = NULL) {
  # --- 1. Find and clean the $DATA record from the model file ---
  dataRecord <- findrecord(baseModelFile, record = "\\$DATA", quiet = TRUE)
  if (is.null(dataRecord)) {
    # If no $DATA record, return the data frame as-is
    return(wideData)
  }

  fullRecordString <- paste(dataRecord, collapse = " ")
  fullRecordString <- gsub(";.*", "", fullRecordString)
  fullRecordString <- gsub("\\s+", " ", fullRecordString)

  # --- 2. Check for and warn about unsupported single-character IGNOREs ---
  # This looks for an IGNORE that is NOT followed by a parenthesis. It is not
  # applied here, so warn when it could remove data records. Per the NONMEM
  # help ($DATA), IGNORE=c skips records whose first character is c, and
  # IGNORE=@ those whose first non-blank character is a letter or @. A numeric
  # first column without NA (written as letters) starts with a digit, sign or
  # point, so then only the header goes - unless c itself is one of those.
  char_ignore_match <- regmatches(
    fullRecordString,
    regexec("IGNORE\\s*=\\s*['\"]?([^[:space:](])['\"]?", fullRecordString, ignore.case = TRUE)
  )[[1]]

  if (length(char_ignore_match) > 0 && !quiet) {
    ignoreChar <- char_ignore_match[2]
    firstCol <- wideData[[1]]
    headerOnly <- is.numeric(firstCol) && !anyNA(firstCol) && !grepl("^[0-9.+-]$", ignoreChar)
    if (!headerOnly) {
      warning(warningCondition(
        paste0(
          "A single-character IGNORE statement was found in the $DATA record.\n",
          "If this filter is used for anything else than managing the header line, ",
          "you will have to pre-filter the data or use IGNORE=(list)."
        ),
        class = "pmxfrem_single_char_ignore", call = NULL
      ))
    }
  }

  # --- 3. Apply Post-Parsing Filter (List-based IGNORE/ACCEPT) ---

  # Regex now makes the '=' optional: IGNORE(list) or IGNORE=(list)
  accept_matches <- regmatches(fullRecordString, gregexpr("ACCEPT\\s*=?\\s*\\((.*?)\\)", fullRecordString, ignore.case = TRUE))[[1]]
  ignore_matches <- regmatches(fullRecordString, gregexpr("IGNORE\\s*=?\\s*\\((.*?)\\)", fullRecordString, ignore.case = TRUE))[[1]]

  if (length(accept_matches) > 0 && length(ignore_matches) > 0) {
    stop("An ACCEPT=(list) and IGNORE=(list) cannot be used in the same $DATA record.", call. = FALSE)
  }

  if (length(accept_matches) == 0 && length(ignore_matches) == 0) {
    return(wideData)
  }

  # The text of the data as written, for the operators that compare strings.
  textData <- .filterTextData(wideData, rawData)

  matches <- if (length(accept_matches) > 0) accept_matches else ignore_matches
  kind <- if (length(accept_matches) > 0) "ACCEPT" else "IGNORE"
  conditions <- unlist(lapply(matches, function(m) {
    .splitFilterList(regmatches(m, regexec("\\((.*)\\)", m))[[1]][2])
  }))
  conditions <- lapply(conditions, .parseFilterCondition)

  # A record satisfying at least one condition of any list is dropped (IGNORE)
  # or accepted (ACCEPT): the conditions are joined by an implied .OR. (NONMEM
  # 7.5 help, $DATA). Several lists of one kind are joined the same way; the
  # help says so for IGNORE and is silent for ACCEPT.
  hit <- rep(FALSE, nrow(wideData))
  for (cond in conditions) {
    if (!cond$label %in% names(wideData)) {
      stop(
        "The ", kind, " condition '", cond$text, "' in $DATA refers to '", cond$label,
        "', which is not a column of the data.",
        call. = FALSE
      )
    }
    hit <- hit | .evalFilterCondition(cond, textData[[cond$label]])
  }

  if (!quiet) {
    message(
      "Applying ", kind, " condition: ",
      paste(vapply(conditions, `[[`, "", "text"), collapse = ", ")
    )
  }

  keep <- if (kind == "ACCEPT") hit else !hit
  wideData[keep, , drop = FALSE]
}

# The data values as character strings. `rawData` (the file's values as
# written, same shape and column names as `wideData`) wins; otherwise the
# values of `wideData` are converted with as.character(), which is how a
# number reads when the original text is unknown (1.0 reads "1").
.filterTextData <- function(wideData, rawData) {
  if (is.null(rawData)) {
    return(as.data.frame(lapply(wideData, as.character), stringsAsFactors = FALSE, optional = TRUE))
  }
  if (nrow(rawData) != nrow(wideData)) {
    stop("`rawData` must have the same number of rows as `wideData`.", call. = FALSE)
  }
  if (!all(names(wideData) %in% names(rawData))) {
    stop("`rawData` must have the columns of `wideData`.", call. = FALSE)
  }
  as.data.frame(lapply(rawData[names(wideData)], as.character), stringsAsFactors = FALSE, optional = TRUE)
}

# Read a data file with every column as the text written in it, for
# filterDataFromModel(rawData =). `...` goes to data.table::fread().
.readDataAsText <- function(path, ...) {
  as.data.frame(
    data.table::fread(path, header = TRUE, colClasses = "character", data.table = FALSE, ...),
    stringsAsFactors = FALSE, optional = TRUE
  )
}

# Split the inside of an IGNORE/ACCEPT list at commas, except in quotes (help:
# "Quotes are required if a value contains special characters ... However, a
# value may not contain spaces or commas" - quoting is still honoured here).
.splitFilterList <- function(listString) {
  chars <- strsplit(listString, "")[[1]]
  parts <- character()
  current <- ""
  quote <- ""
  for (ch in chars) {
    if (nzchar(quote)) {
      if (ch == quote) quote <- ""
      current <- paste0(current, ch)
    } else if (ch %in% c("'", "\"")) {
      quote <- ch
      current <- paste0(current, ch)
    } else if (ch == ",") {
      parts <- c(parts, current)
      current <- ""
    } else {
      current <- paste0(current, ch)
    }
  }
  trimws(c(parts, current))
}

# label, operator, value. The help says the operator defaults to "=" when
# omitted; that form cannot be told from a label and a value, so it is not
# supported here. A condition is label + operator + value (help: "label=value",
# "label.EQ.value", ... "/=", "<", ...).
.parseFilterCondition <- function(text) {
  ops <- c(
    "\\.EQN\\.", "\\.NEN\\.", "\\.EQ\\.", "\\.NE\\.", "\\.GE\\.", "\\.GT\\.", "\\.LE\\.", "\\.LT\\.",
    "==", "/=", "<=", ">=", "=", "<", ">"
  )
  pattern <- paste0("^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*(", paste(ops, collapse = "|"), ")\\s*(.+?)\\s*$")
  m <- regmatches(text, regexec(pattern, text, ignore.case = TRUE))[[1]]
  if (length(m) == 0) {
    stop(
      "Cannot parse the IGNORE/ACCEPT condition '", text, "': expected label, operator and value, ",
      "e.g. BLQ.EQ.1.",
      call. = FALSE
    )
  }
  value <- m[4]
  if (grepl("^(\".*\"|'.*')$", value)) value <- substr(value, 2, nchar(value) - 1)
  op <- toupper(m[3])
  # NONMEM 7.5 help, $DATA, IGNORE=(list): with "=", "==", "/=", ".EQ." and
  # ".NE." the data value and the list value are compared as character
  # strings; "Otherwise, they are converted to numeric and compared
  # numerically (This is the case with .NEN. and .EQN.)".
  type <- switch(op,
    "=" = ,
    "==" = ,
    ".EQ." = "eq",
    "/=" = ,
    ".NE." = "ne",
    ".EQN." = "eqn",
    ".NEN." = "nen",
    ">" = ,
    ".GT." = "gt",
    ">=" = ,
    ".GE." = "ge",
    "<" = ,
    ".LT." = "lt",
    "<=" = ,
    ".LE." = "le"
  )
  list(label = m[2], type = type, value = value, text = trimws(text))
}

# TRUE where the condition holds; a value that cannot be compared (NA, or
# not a number where a number is needed) does not satisfy it.
.evalFilterCondition <- function(cond, dataText) {
  dataText <- trimws(dataText)
  if (cond$type %in% c("eq", "ne")) {
    res <- if (cond$type == "eq") dataText == cond$value else dataText != cond$value
  } else {
    x <- suppressWarnings(as.numeric(dataText))
    v <- suppressWarnings(as.numeric(cond$value))
    res <- switch(cond$type,
      eqn = x == v,
      nen = x != v,
      gt = x > v,
      ge = x >= v,
      lt = x < v,
      le = x <= v
    )
  }
  res & !is.na(res)
}
