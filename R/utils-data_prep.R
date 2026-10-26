#' Prepare and Validate FFEM Data Against a Model's $INPUT Record
#'
#' Reads an FFEM dataset and a NONMEM model file, and aligns the dataset's
#' column names with those specified in the model's $INPUT record. It validates
#' that the column count matches and that all specified covariates are present
#' in the $INPUT record. This function is robust to multi-line, abbreviated,
#' and commented $INPUT records.
#'
#' @param ffemDataFile A character string path to the FFEM dataset, or a data.frame.
#' @param baseModelFile A character string path to the base NONMEM model file.
#' @param covariates A character vector of covariate names (must match the names
#'   in the $INPUT record) that the user intends to model.
#' @param quiet Logical. Should the function execute silently? Defaults to FALSE.
#' @param keepDoseOnlySubjects Logical. If FALSE (default), subjects without any valid PK observations (e.g.,
#'   only dosing records) are completely excluded from the generated dataset. If TRUE,
#'   these subjects are retained and their covariates are included as observations.
#' @param strID Character. The name of the subject identifier column. Defaults to "ID".
#' @param cstrKeepCols Character vector or `NULL`. The columns the FREM data set
#'   will keep; checked here only so that a column the base model drops is
#'   reported as such.
#'
#' @details An item `label=synonym` is a column named by whichever of the two
#'   is a reserved NONMEM label (`CP=DV` gives `DV`), and by the label when
#'   neither is; `covariates` and `cstrKeepCols` may use either name.
#'
#'   `$INPUT` names every column of the data file in order, so the
#'   columns are named by position. Items marked `=DROP` or `=SKIP`, or a bare
#'   `DROP`, are dropped - after the `IGNORE` / `ACCEPT` filters are applied,
#'   since those may refer to them (NONMEM help, `$DATA`). Columns beyond the
#'   last `$INPUT` item are ignored, with a message naming them.
#'
#' @return A list containing three elements:
#'   \item{validatedData}{The data frame with column names replaced by the parsed $INPUT names.}
#'   \item{originalNames}{The original data header names from the data file.}
#'   \item{inputNames}{The clean character vector of names parsed from the $INPUT record.}
#'
#' @family Data Assembly Internal
#' @concept data_assembly
#' @keywords internal
prepareAndValidateData <- function(ffemDataFile, baseModelFile, covariates, quiet = FALSE, keepDoseOnlySubjects = FALSE, strID = "ID",
                                   cstrKeepCols = NULL) {
  # 1. Read the data and stash original names
  if (is.data.frame(ffemDataFile)) {
    df <- as.data.frame(ffemDataFile)
  } else if (is.character(ffemDataFile) && file.exists(ffemDataFile)) {
    df <- data.table::fread(ffemDataFile, header = TRUE, data.table = FALSE)
  } else {
    stop("Cannot find or read FFEM dataset.", call. = FALSE)
  }
  originalNames <- names(df)

  # 2. Read the $INPUT record using a flexible regex
  inputBlock <- findrecord(baseModelFile, record = "^\\$INP(U(T)?)?\\s", quiet = TRUE)

  if (is.null(inputBlock)) {
    stop("Could not find the $INPUT record in the base model file: ", baseModelFile, call. = FALSE)
  }

  # 3. Parse the $INPUT record: one item per data column, in order
  items <- .fremInputItems(inputBlock)
  inputNames <- items$name[!items$dropped]
  droppedNames <- items$name[items$dropped]
  # Either name of a `label=synonym` item may be given.
  covariates <- .fremColumnNames(covariates, items)
  cstrKeepCols <- .fremColumnNames(cstrKeepCols, items)

  # 4. Perform validation checks. $INPUT names the data file's columns in
  # order, dropped ones included, so a data file with fewer columns cannot be
  # what the model reads. Columns beyond the last item are ignored with a
  # message (decided 2026-10-26; the NONMEM help does not cover this case).
  if (length(originalNames) < nrow(items)) {
    stop(
      paste0(
        "The number of columns in the data file (", length(originalNames), ") ",
        "does not match the number of items in the $INPUT record (", nrow(items), "). ",
        "$INPUT lists every column of the data file, =DROP columns included."
      ),
      call. = FALSE
    )
  }
  if (length(originalNames) > nrow(items)) {
    extra <- originalNames[(nrow(items) + 1):length(originalNames)]
    if (!quiet) {
      message(
        "The data file has ", length(extra), " column(s) more than the $INPUT record has items; ",
        "ignoring ", paste(extra, collapse = ", "), "."
      )
    }
    df <- df[, seq_len(nrow(items)), drop = FALSE]
  }

  # A dropped column is not in the model, so it can be neither modelled nor kept.
  for (arg in c("covariates", "cstrKeepCols")) {
    clash <- intersect(get(arg), droppedNames)
    if (length(clash) > 0) {
      stop("`", arg, "` names ", paste(clash, collapse = ", "),
        ", which the base model's $INPUT marks =DROP or =SKIP.",
        call. = FALSE
      )
    }
  }

  if (!is.null(covariates) && length(covariates) > 0) {
    if (!all(covariates %in% inputNames)) {
      missing_covs <- paste(covariates[!covariates %in% inputNames], collapse = ", ")
      stop("The following specified covariates were not found in the model's $INPUT record: ",
        missing_covs,
        call. = FALSE
      )
    }
  }

  # 5. Name the columns by position, dropped ones included, so that IGNORE /
  # ACCEPT conditions on a dropped column still apply.
  names(df) <- items$name

  # --- Apply IGNORE/ACCEPT statements ---
  if (exists("filterDataFromModel")) {
    df <- filterDataFromModel(baseModelFile = baseModelFile, wideData = df, quiet = quiet)
  } else {
    warning("filterDataFromModel function not found. Skipping IGNORE/ACCEPT filtering.")
  }

  # Then remove the dropped columns: the model never sees them.
  df <- df[, !items$dropped, drop = FALSE]

  # 5.5 OPTIONAL FILTER (Remove subjects without observations) ---
  if (!keepDoseOnlySubjects) {
    if ("EVID" %in% names(df) && strID %in% names(df)) {
      valid_ids <- unique(df[[strID]][df$EVID == 0])
      df <- df[df[[strID]] %in% valid_ids, , drop = FALSE]
    }
  }

  # 6. Return the results
  return(list(
    validatedData = df,
    originalNames = originalNames,
    inputNames = inputNames
  ))
}

#' The items of a `$INPUT` record
#'
#' An item is `label` or `label=synonym`; either name may be used, and a
#' reserved one gives the item its reserved meaning (NONMEM help, `$INPUT`).
#' The data column is named by the reserved one of the two when exactly one is
#' reserved, and by the label otherwise; the other name is returned as `synonym`.
#'
#' @param inputBlock The `$INPUT` record's lines, as [findrecord()] returns them.
#' @return A data frame with one row per item, in order: `name` (the column
#'   name; a bare `DROP` gets a placeholder), `synonym` (the other name, or
#'   `""`) and `dropped` (`TRUE` for `=DROP`, `=SKIP` or a bare `DROP` /
#'   `SKIP`; a dropped item has no synonym).
#' @keywords internal
#' @noRd
.fremInputItems <- function(inputBlock) {
  inputBlock[1] <- gsub("(?i)^\\$INP(U(T)?)?\\s*", "", inputBlock[1], perl = TRUE)
  record <- paste(gsub(";.*", "", inputBlock), collapse = " ")
  tokens <- strsplit(trimws(record), "\\s+")[[1]]
  tokens <- tokens[nchar(tokens) > 0]

  label <- sub("=.*", "", tokens)
  value <- ifelse(grepl("=", tokens), sub("^[^=]*=", "", tokens), "")
  isDrop <- function(x) toupper(x) %in% c("DROP", "SKIP")
  dropped <- isDrop(label) | isDrop(value)

  ## label=synonym: the reserved one names the column, the label when both or
  ## neither are reserved.
  swap <- !dropped & value != "" & !.fremIsReserved(label) & .fremIsReserved(value)
  name <- ifelse(swap, value, label)
  synonym <- ifelse(swap, label, value)

  ## A dropped item is named by its label, or by the value of DROP=name; a bare
  ## DROP has no name and gets a placeholder.
  other <- dropped & isDrop(label) & value != ""
  name[other] <- value[other]
  synonym[dropped] <- ""
  bare <- dropped & isDrop(name)
  name[bare] <- paste0(".DROP", seq_len(sum(bare)))

  data.frame(name = name, synonym = synonym, dropped = dropped, stringsAsFactors = FALSE)
}

## The data item labels with a reserved meaning in $INPUT: the reserved and
## semi-reserved lists of the NONMEM 7.5 help, $INPUT.
.fremReservedLabels <- c(
  "ID", "L1", "L2", "DV", "MDV", "RAW_", "MRG_", "RPT_",
  "TIME", "DATE", "DAT1", "DAT2", "DAT3", "DROP", "SKIP", "EVID", "AMT", "RATE",
  "SS", "II", "ADDL", "CMT", "PCMT", "CALL", "CONT",
  "XVID1", "XVID2", "XVID3", "XVID4", "XVID5", "REPL_"
)

.fremIsReserved <- function(x) toupper(x) %in% .fremReservedLabels

#' Read a name given as either side of an `$INPUT` `label=synonym` as the column name
#'
#' A name that is a column name is kept, so it wins over a synonym of another
#' item; one that is only a synonym becomes its item's column name.
#'
#' @param x Character vector of names, or `NULL`.
#' @param items The result of [.fremInputItems()].
#' @keywords internal
#' @noRd
.fremColumnNames <- function(x, items) {
  if (is.null(x)) {
    return(x)
  }
  bySynonym <- items$name[match(x, items$synonym)]
  ifelse(x %in% items$name | x == "" | is.na(bySynonym), x, bySynonym)
}

#' The synonyms of an `$INPUT` record's items, named by the column they stand for
#' @param items The result of [.fremInputItems()].
#' @return A named character vector: names are synonyms, values column names.
#' @keywords internal
#' @noRd
.fremInputSynonyms <- function(items) {
  has <- items$synonym != ""
  stats::setNames(items$name[has], items$synonym[has])
}
