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
#' 
#' @return A list containing three elements:
#'   \item{validatedData}{The data frame with column names replaced by the parsed $INPUT names.}
#'   \item{originalNames}{The original data header names from the data file.}
#'   \item{inputNames}{The clean character vector of names parsed from the $INPUT record.}
#'
#' @family Data Assembly Internal
#' @concept data_assembly
#' @keywords internal
prepareAndValidateData <- function(ffemDataFile, baseModelFile, covariates, quiet = FALSE, keepDoseOnlySubjects = FALSE, strID = "ID") {
  
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
  
  # 3. Parse the $INPUT record robustly
  inputBlock[1] <- gsub("(?i)^\\$INP(U(T)?)?\\s+", "", inputBlock[1], perl = TRUE)
  linesWithoutComments <- sapply(inputBlock, function(line) gsub(";.*", "", line), USE.NAMES = FALSE)
  fullRecordString <- paste(linesWithoutComments, collapse = " ")
  
  inputItems <- strsplit(fullRecordString, "\\s+")[[1]]
  inputItems <- inputItems[nchar(inputItems) > 0]
  
  inputNames <- sapply(inputItems, function(item) sub("=.*", "", item), USE.NAMES = FALSE)
  inputNames <- inputNames[!grepl("DROP", inputItems, ignore.case = TRUE)]
  
  # 4. Perform validation checks
  if (length(originalNames) != length(inputNames)) {
    stop(paste0("The number of columns in the data file (", length(originalNames), ") ",
                "does not match the number of items in the $INPUT record (", length(inputNames), ")."),
         call. = FALSE)
  }
  
  if (!is.null(covariates) && length(covariates) > 0) {
    if (!all(covariates %in% inputNames)) {
      missing_covs <- paste(covariates[!covariates %in% inputNames], collapse = ", ")
      stop("The following specified covariates were not found in the model's $INPUT record: ", 
           missing_covs, call. = FALSE)
    }
  }
  
  # 5. Apply the new names
  names(df) <- inputNames
  
  # --- Apply IGNORE/ACCEPT statements ---
  if (exists("filterDataFromModel")) {
    df <- filterDataFromModel(baseModelFile = baseModelFile, wideData = df, quiet = quiet)
  } else {
    warning("filterDataFromModel function not found. Skipping IGNORE/ACCEPT filtering.")
  }
  
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
