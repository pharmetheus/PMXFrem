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
#' @return A data frame containing the data after all applicable IGNORE/ACCEPT
#'   conditions have been applied.
#'
#' @keywords internal
filterDataFromModel <- function(baseModelFile, wideData, quiet = FALSE) {
  
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
  # This looks for an IGNORE that is NOT followed by a parenthesis.
  char_ignore_match <- regexec("IGNORE\\s*=\\s*['\"]?([^\\s\\(])['\"]?", fullRecordString, ignore.case = TRUE)
  
  if (char_ignore_match[[1]][1] != -1 && !quiet) {
    warning(
      "A single-character IGNORE statement was found in the $DATA record.\n",
      "If this filter is used for anything else than managing the header line, ",
      "you will have to pre-filter the data or use IGNORE=(list).",
      call. = FALSE
    )
  }
  
  # --- 3. Apply Post-Parsing Filter (List-based IGNORE/ACCEPT) ---
  
  # Helper to translate NONMEM logic to R logic
  translate_condition <- function(condition_str) {
    # Use a unique placeholder to protect '==' from the single '=' replacement
    placeholder <- "___DOUBLE_EQUALS___"
    
    r_expr <- gsub("==", placeholder, condition_str)
    
    # Now, it is safe to replace the default single equals operator
    r_expr <- gsub("(?<![<>!])=(?!=)", " == ", r_expr, perl = TRUE)
    
    # Change the placeholder back to the correct '==' operator
    r_expr <- gsub(placeholder, " == ", r_expr)
    
    # Handle all other operators
    r_expr <- gsub(",", " | ", r_expr)
    r_expr <- gsub("\\.EQ\\.", " == ", r_expr, ignore.case = TRUE)
    r_expr <- gsub("\\.EQN\\.", " == ", r_expr, ignore.case = TRUE)
    r_expr <- gsub("\\.NE\\.", " != ", r_expr, ignore.case = TRUE)
    r_expr <- gsub("\\.NEN\\.", " != ", r_expr, ignore.case = TRUE)
    r_expr <- gsub("\\.GE\\.", " >= ", r_expr, ignore.case = TRUE)
    r_expr <- gsub("\\.GT\\.", " > ", r_expr, ignore.case = TRUE)
    r_expr <- gsub("\\.LE\\.", " <= ", r_expr, ignore.case = TRUE)
    r_expr <- gsub("\\.LT\\.", " < ", r_expr, ignore.case = TRUE)
    r_expr <- gsub("\\.AND\\.", " & ", r_expr, ignore.case = TRUE)
    r_expr <- gsub("\\.OR\\.", " | ", r_expr, ignore.case = TRUE)
    return(r_expr)
  }
  
  # Regex now makes the '=' optional: IGNORE(list) or IGNORE=(list)
  accept_matches <- regmatches(fullRecordString, gregexpr("ACCEPT\\s*=?\\s*\\((.*?)\\)", fullRecordString, ignore.case = TRUE))[[1]]
  ignore_matches <- regmatches(fullRecordString, gregexpr("IGNORE\\s*=?\\s*\\((.*?)\\)", fullRecordString, ignore.case = TRUE))[[1]]
  
  if (length(accept_matches) > 0 && length(ignore_matches) > 0) {
    stop("An ACCEPT=(list) and IGNORE=(list) cannot be used in the same $DATA record.", call. = FALSE)
  }
  
  filteredData <- wideData
  
  if (length(accept_matches) > 0) {
    accept_conditions <- sapply(accept_matches, function(m) regmatches(m, regexec("\\((.*)\\)", m))[[1]][2])
    r_accept_exprs <- sapply(accept_conditions, translate_condition)
    full_r_accept_expr <- paste0("(", r_accept_exprs, ")", collapse = " | ")
    
    if(!quiet) message("Applying ACCEPT condition: ", full_r_accept_expr)
    filteredData <- subset(filteredData, eval(parse(text = full_r_accept_expr)))
    
  } else if (length(ignore_matches) > 0) {
    ignore_conditions <- sapply(ignore_matches, function(m) regmatches(m, regexec("\\((.*)\\)", m))[[1]][2])
    r_ignore_exprs <- sapply(ignore_conditions, translate_condition)
    full_r_ignore_expr <- paste0("(", r_ignore_exprs, ")", collapse = " | ")
    
    if(!quiet) message("Applying IGNORE condition: ", full_r_ignore_expr)
    filteredData <- subset(filteredData, !eval(parse(text = full_r_ignore_expr)))
  }
  
  return(filteredData)
}

