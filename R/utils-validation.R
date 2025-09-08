#' Validate the Structural Integrity of the Generated FREM Dataset
#'
#' Compares the base records (FREMTYPE == 0) of the newly generated FREM dataset
#' against the original filtered base dataset. It guarantees that no rows were
#' dropped, no IDs were scrambled, and no structural PK/PD data was mutated.
#'
#' @param originalData The original data frame (post $DATA IGNORE/ACCEPT filtering).
#' @param fremData The finalized FREM data frame (before writing to disk).
#' @param cstrDV Character vector of DV column names (defaults to "DV").
#' @param strID The subject identifier column name.
#' @param quiet Logical. If FALSE, prints a success message upon passing.
#'
#' @return TRUE if validation passes. Throws a stop() error if validation fails.
#' 
#' @keywords internal
validateFremData <- function(originalData, fremData, cstrDV = "DV", strID = "ID", quiet = FALSE) {
  
  if (!"FREMTYPE" %in% names(fremData)) {
    stop("Validation failed: 'FREMTYPE' column is missing from the generated FREM data.", call. = FALSE)
  }
  
  # 1. Pre-filter originalData to mimic the pipeline's extraction rule for FREMTYPE=0
  # (Keep all dosing records, but drop invalid observations where DV is missing/-99)
  primaryDV <- cstrDV[1]
  if (primaryDV %in% names(originalData)) {
    if ("EVID" %in% names(originalData)) {
      keep_rows <- originalData[[primaryDV]] != -99 | originalData$EVID != 0
    } else {
      keep_rows <- originalData[[primaryDV]] != -99
    }
    keep_rows[is.na(keep_rows)] <- FALSE
    expected_base <- originalData[keep_rows, , drop = FALSE]
  } else {
    expected_base <- originalData
  }
  
  # 2. Isolate the base records from the generated data
  base_frem <- fremData[fremData$FREMTYPE == 0, , drop = FALSE]
  
  # 3. Row Count Validation
  if (nrow(base_frem) != nrow(expected_base)) {
    stop(sprintf(
      "CRITICAL DATA ERROR: Row count mismatch.\nOriginal valid data has %d rows, but FREM base data (FREMTYPE=0) has %d rows.\nThe pipeline dropped or duplicated records.",
      nrow(expected_base), nrow(base_frem)
    ), call. = FALSE)
  }
  
  # 4. Subject Integrity Validation
  orig_ids <- unique(expected_base[[strID]])
  frem_ids <- unique(base_frem[[strID]])
  
  if (length(orig_ids) != length(frem_ids) || !all(orig_ids == frem_ids)) {
    stop("CRITICAL DATA ERROR: Subject cohort mismatch. IDs were dropped, added, or reordered during generation.", call. = FALSE)
  }
  
  # 5. Strict Column-by-Column Structural Validation
  cols_to_check <- intersect(names(expected_base), names(base_frem))
  critical_cols <- c(strID, "TIME", "AMT", primaryDV, "EVID", "MDV", "CMT", "RATE", "SS", "II")
  cols_to_check <- unique(c(intersect(critical_cols, cols_to_check), cols_to_check))
  
  for (col in cols_to_check) {
    vec_orig <- expected_base[[col]]
    vec_frem <- base_frem[[col]]
    
    vec_orig[is.na(vec_orig)] <- -999999
    vec_frem[is.na(vec_frem)] <- -999999
    
    is_equal <- isTRUE(all.equal(as.numeric(vec_orig), as.numeric(vec_frem), tolerance = 1e-7))
    
    if (!is_equal) {
      is_equal <- isTRUE(all(as.character(vec_orig) == as.character(vec_frem)))
    }
    
    if (!is_equal) {
      diff_idx <- which(as.character(vec_orig) != as.character(vec_frem))[1]
      stop(sprintf(
        "CRITICAL DATA ERROR: Data mutation detected in column '%s'.\nMismatch starts at valid row %d (ID: %s).\nOriginal value: %s\nFREM value: %s",
        col, diff_idx, expected_base[[strID]][diff_idx], 
        as.character(expected_base[[col]][diff_idx]), 
        as.character(base_frem[[col]][diff_idx])
      ), call. = FALSE)
    }
  }
  
  if (!quiet) message("Data integrity validation passed: The FREM dataset perfectly mirrors the base dataset.")
  return(TRUE)
}
