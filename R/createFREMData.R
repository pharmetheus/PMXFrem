#' Create a FREM dataset from a standard NONMEM dataset.
#'
#' Create a FREM dataset from a standard NONMEM dataset.
#'
#' If there are more than one DV variable in different columns in the FFEM data
#' set, they can be included in the DV column of the FREM data set with
#' different FREMTYPES by specifying the names of the columns in the `cstrDV`
#' argument.
#'
#' @param strFFEMData Name of FFEM-dataset (normal dataset) that will be used to
#'   create the FREM-dataset.
#' @param cstrKeepCols A vector of columns to keep in the dataset (for the
#'   updated new dataset).
#' @param cstrSetToZero A vector of variable names that should be set to zero on
#'   the covariate FREMTYPE lines. Typically columns with PK dosing information.
#' @param strFREMDataFileName Name of FREM dataset to create. Default=NULL => no
#'   dataset is written to disc
#' @param quiet If set to FALSE, the function outputs verbose information on
#'   what it is doing.
#' @param strID A string with the ID identifier column in the FFEM dataset.
#' @param cstrDV A vector of strings with DV variables that should be added,
#'   default="DV" (i.e. DV assumed fremtype=0), additional DVs are added with
#'   fremtype 1,2,3...etc.
#' @param cstrContCovs A vector of strings with continuous covariates to create.
#'   Default=NULL, i.e. no continuous covariates will be added
#' @param cstrCatCovs  A vector of strings with categorical covariates to
#'   create. Default=NULL, i.e. no categorical covariates will be added
#' @param logtCovs A vector of strings with continuous covariates that should be
#'   log-transformed (natural log) before being added to the DV column.
#' @param bRecodeDichotomous Logical. If TRUE, dichotomous covariates (e.g., coded 1/2) 
#'   will be automatically recoded to 0/1, and a new variable (e.g., SEX_1) will be 
#'   created. If FALSE (the default), dichotomous covariates must be strictly coded 
#'   as 0/1 in the input data; otherwise, a strict validation error will be thrown.
#' @param allowNon01 Logical. If TRUE, bypasses strict 0/1 validation and allows 
#'   dichotomous covariates with non-standard coding (e.g., 1/2) to pass through 
#'   untouched without being recoded. This provides 100% compatibility with PsN 
#'   legacy behavior. Defaults to FALSE.
#' @param cSortCols A character vector of column names to sort the final dataset by. 
#'   The special internally generated column \code{"ORIG_ROW_IDX"} can be used to 
#'   preserve the original row order of the input dataset. Default is 
#'   \code{c("ORIG_ROW_IDX", "FREMTYPE")}.
#' @param cSortDirection A numeric vector specifying the sort order for each column in 
#'   \code{cSortCols} (1 for ascending, -1 for descending). Default is \code{c(1, 1)}.
#' @param cFremtypes A vector of FREMTYPE values that each DV and covariate
#'   should use. Default=NULL.
#' @param missVal Numeric. Missing value indicator.
#' @param keepDoseOnlySubjects Logical. If \code{FALSE} (default), subjects without any valid PK observations (e.g., 
#' only dosing records) are completely excluded from the generated dataset. If \code{TRUE}, 
#' these subjects are retained and their covariates are included as observations.
#'
#' @return A data.frame with the FREM data set.
#' @export
#'
#' @examples
#' strFFEMData <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv",
#'                            package = "PMXFrem")
#' tmp10 <- createFREMData(
#'            strFFEMData          = strFFEMData,
#'            cstrKeepCols         = c("ID", "TIME", "AMT", "EVID", "RATE"),
#'            bRecodeDichotomous   = TRUE,
#'            cstrCatCovs          = c("SEX","SMOK","NCIL"),
#'            cstrContCovs         = c("WT","BMI","AGE"))
#'
#' @family Data Assembly
#' @concept data_assembly
createFREMData <- function(
    strFFEMData,
    strFREMDataFileName  = NULL,
    quiet                = TRUE,
    strID                = "ID",
    cstrKeepCols         = c("ID", "TIME", "AMT", "II", "EVID", "SS", "RATE"),
    cstrSetToZero        = c("AMT", "II", "SS", "EVID", "RATE"),
    cstrDV               = "DV",
    cstrContCovs         = NULL,
    cstrCatCovs          = NULL,
    logtCovs             = NULL,
    bRecodeDichotomous   = FALSE,
    allowNon01           = FALSE,
    missVal              = -99,
    cSortCols            = c("ORIG_ROW_IDX", "FREMTYPE"), 
    cSortDirection       = c(1, 1),
    cFremtypes           = NULL,
    keepDoseOnlySubjects = FALSE) {
  
  CovFremType       <- 100
  FremtypeIncrement <- 100
  iNewFremtypeDV    <- 0
  
  printq <- function(str, quiet) {
    if (!quiet) print(str)
  }
  
  dfFFEM <- NULL
  if (is.data.frame(strFFEMData)) {
    dfFFEM <- strFFEMData
  } else if (is.character(strFFEMData) && file.exists(strFFEMData)) {
    dfFFEM <- data.table::fread(strFFEMData, h = TRUE, data.table = FALSE, check.names = TRUE, showProgress = !quiet)
  } else {
    # Exact string match for the unit test expectation!
    stop("Cannot find FFEM dataset")
  }
  
  if (!keepDoseOnlySubjects && "EVID" %in% names(dfFFEM) && strID %in% names(dfFFEM)) {
    valid_ids <- unique(dfFFEM[[strID]][dfFFEM$EVID == 0])
    dfFFEM <- dfFFEM[dfFFEM[[strID]] %in% valid_ids, , drop = FALSE]
  }
  
  printq(paste0("Read the FFEM dataset, consisting of ", ncol(dfFFEM), " columns and ", nrow(dfFFEM), " rows"), quiet = quiet)
  dfFFEM$ORIG_ROW_IDX <- seq_len(nrow(dfFFEM))
  
  # --- 1. VALIDATION ---
  data_column_names <- names(dfFFEM)
  # Check if keep_cols exist, soft-drop them if they are missing (they may be generated in Phase 2)
  if (!is.null(cstrKeepCols)) {
    missing_cols <- cstrKeepCols[!cstrKeepCols %in% names(dfFFEM)]
    if (length(missing_cols) > 0) {
      warning(sprintf(
        "The following keep_cols are missing from the input data and will be ignored during this phase (they may be dynamically generated later): %s", 
        paste(missing_cols, collapse = ", ")
      ), call. = FALSE)
      
      # Keep only the columns that actually exist right now
      cstrKeepCols <- cstrKeepCols[cstrKeepCols %in% names(dfFFEM)]
    }
  }
  
  if (length(intersect(cstrCatCovs, logtCovs)) > 0) {
    stop("A covariate cannot be listed in both cstrCatCovs and logtCovs. Log-transforming categorical variables is invalid.")
  }
  
  valid_ContCovs <- cstrContCovs[cstrContCovs %in% names(dfFFEM)]
  valid_CatCovs  <- cstrCatCovs[cstrCatCovs %in% names(dfFFEM)]
  
  for (cov in setdiff(cstrContCovs, valid_ContCovs)) {
    printq(paste0("Can't add covariate ", cov, ", not found in FFEM dataset. Skipping this covariate."), quiet = quiet)
  }
  for (cov in setdiff(cstrCatCovs, valid_CatCovs)) {
    printq(paste0("Can't add covariate ", cov, ", not found in FFEM dataset. Skipping this covariate."), quiet = quiet)
  }
  
  # --- 2. TIME-VARYING WARNING ---
  for (cov in c(valid_ContCovs, valid_CatCovs)) {
    time_varying <- dfFFEM[dfFFEM[[cov]] != missVal, ]
    if (nrow(time_varying) > 0) {
      n_distinct_per_id <- tapply(time_varying[[cov]], time_varying[[strID]], function(x) length(unique(x)))
      violators <- sum(n_distinct_per_id > 1)
      if (violators > 0) {
        warning(sprintf("Covariate %s is time-varying for %d individual(s). FREM will use only the first baseline value.", cov, violators), call. = FALSE)
      }
    }
  }
  
  # --- 3. CATEGORICAL Y-1 EXPANSION ---
  final_CatCovs <- c()
  
  if (length(valid_CatCovs) > 0) {
    poly_covs <- c()
    
    for (cov in valid_CatCovs) {
      valid_vals <- unique(dfFFEM[[cov]])
      valid_vals <- valid_vals[valid_vals != missVal & !is.na(valid_vals)]
      
      if (length(valid_vals) == 0) {
        # FIX: Restored exact text match for missing covariates
        printq(paste0("No non-missing covariate values for ", cov, ". Skipping this covariate."), quiet = quiet)
      } else if (length(valid_vals) > 2) {
        poly_covs <- c(poly_covs, cov)
        # FIX: Removed rev() to generate columns in ascending order matching the old tests
        covVal <- sort(valid_vals)[-1]
        final_CatCovs <- c(final_CatCovs, paste0(cov, "_", covVal))
      } else if (length(valid_vals) == 2) {
        if (bRecodeDichotomous) {
          covVal <- sort(valid_vals)[2]
          new_name <- paste0(cov, "_", covVal)
          dfFFEM[[new_name]] <- ifelse(dfFFEM[[cov]] == missVal, -99, ifelse(dfFFEM[[cov]] == covVal, 1, 0))
          final_CatCovs <- c(final_CatCovs, new_name)
        } else {
          # Strict validation only fires if the kill-switch is NOT overridden
          if (!allowNon01 && !all(sort(valid_vals) == c(0, 1))) {
            stop(sprintf("Strict Validation Error: Dichotomous covariate '%s' is coded as %s/%s. It must be strictly 0/1. To bypass this check, set allowNon01 = TRUE, or to auto-recode it, set bRecodeDichotomous = TRUE.", cov, min(valid_vals), max(valid_vals)), call. = FALSE)
          }
          final_CatCovs <- c(final_CatCovs, cov)
        }
      } else {
        printq(paste0("Covariate ", cov, " has < 2 valid levels. Skipping."), quiet = quiet)
      }
    }
    
    if (length(poly_covs) > 0) {
      dfFFEM <- addFREMcovariates(
        dfFFEM           = dfFFEM, 
        covariates       = poly_covs, 
        missVal            = missVal, 
        includeReference = FALSE, 
        imputeMissing    = FALSE
      )
    }
  }
  
  numCatLevels <- length(final_CatCovs)
  
  # --- 4. FREMTYPE GENERATION ---
  if (is.null(cFremtypes)) {
    cFremtypes <- c(iNewFremtypeDV:(length(cstrDV) - 1))
    if (length(valid_ContCovs) > 0) {
      cFremtypes <- c(cFremtypes, seq(CovFremType, CovFremType + FremtypeIncrement * (length(valid_ContCovs) - 1), by = FremtypeIncrement))
    }
    maxval <- max(c(0, cFremtypes[cFremtypes >= CovFremType])) + FremtypeIncrement
    if (numCatLevels > 0) {
      cFremtypes <- c(cFremtypes, seq(maxval, maxval + (numCatLevels - 1) * FremtypeIncrement, by = FremtypeIncrement))
    }
  }
  
  cFremtypes <- unique(cFremtypes)
  if (length(cFremtypes) != length(cstrDV) + length(valid_ContCovs) + numCatLevels) {
    stop("The number of fremtypes are not the same as the number of frem variables.")
  }
  
  cols_to_return <- unique(c(cstrKeepCols, "FREMTYPE", cstrDV))
  cstrSetToZero <- cstrSetToZero[cstrSetToZero %in% names(dfFFEM)]
  dfAddList <- list()
  
  # --- 5. STACKING DVs ---
  for (i in seq_along(cstrDV)) {
    strDV <- cstrDV[i]
    
    # Evaluate which rows to keep
    if ("EVID" %in% names(dfFFEM)) {
      # Keep if DV is quantifiable (!= missVal), OR if it's a dosing/reset record (EVID != 0)
      keep_rows <- dfFFEM[[strDV]] != missVal | dfFFEM$EVID != 0
    } else {
      keep_rows <- dfFFEM[[strDV]] != missVal
    }
    
    # Catch Scenario 1: NAs evaluate to NA in logic checks. Convert them to FALSE so they drop safely
    # (Note: If EVID != 0, keep_rows evaluates to TRUE even if DV is NA, which is exactly what we want!)
    keep_rows[is.na(keep_rows)] <- FALSE
    
    dfDVData <- dfFFEM[keep_rows, ]
    
    if (nrow(dfDVData) > 0) {
      dfDVData$DV <- dfDVData[[strDV]]
      dfDVData$FREMTYPE <- cFremtypes[i]
      dfAddList[[length(dfAddList) + 1]] <- dfDVData
    } else {
      warning(paste0("Note that it might be inconsistencies in DV fremtypes since fremtype ", cFremtypes[i], " is not present!"))
    }
  }
  
  # --- 6. STACKING CONTINUOUS (With Log Logic) ---
  if (length(valid_ContCovs) > 0) {
    for (i in seq_along(valid_ContCovs)) {
      cov_name <- valid_ContCovs[i]
      dfDVData <- dfFFEM[dfFFEM[[cov_name]] != missVal, ]
      dfDVData <- dfDVData[!duplicated(dfDVData[[strID]]), ] 
      
      if (nrow(dfDVData) > 0) {
        if (!is.null(logtCovs) && cov_name %in% logtCovs) {
          if (any(dfDVData[[cov_name]] <= 0)) {
            stop(paste0("Cannot apply log-transformation to covariate ", cov_name, " because it contains values <= 0."))
          }
          dfDVData$DV <- log(dfDVData[[cov_name]])
        } else {
          dfDVData$DV <- dfDVData[[cov_name]]
        }
        dfDVData$FREMTYPE <- cFremtypes[i + length(cstrDV)]
        dfDVData[, cstrSetToZero] <- 0
        dfAddList[[length(dfAddList) + 1]] <- dfDVData
      } else {
        # FIX: Restored exact printq text
        printq(paste0("No non-missing covariate values for ", cov_name, ". Skipping this covariate."), quiet = quiet)
      }
    }
  }
  
  # --- 7. STACKING CATEGORICAL ---
  if (numCatLevels > 0) {
    for (i in seq_along(final_CatCovs)) {
      cov_name <- final_CatCovs[i]
      dfDVData <- dfFFEM[dfFFEM[[cov_name]] != missVal, ]
      dfDVData <- dfDVData[!duplicated(dfDVData[[strID]]), ]
      
      if (nrow(dfDVData) > 0) {
        dfDVData$DV <- dfDVData[[cov_name]]
        dfDVData$FREMTYPE <- cFremtypes[i + length(cstrDV) + length(valid_ContCovs)]
        dfDVData[, cstrSetToZero] <- 0
        dfAddList[[length(dfAddList) + 1]] <- dfDVData
      }
    }
  }
  
  # --- 8. FINALIZE ---
  if (length(dfAddList) > 0) {
    dfFREM <- as.data.frame(data.table::rbindlist(dfAddList, use.names = TRUE, fill = TRUE))
  } else {
    dfFREM <- as.data.frame(matrix(ncol = length(cols_to_return), nrow = 0))
    names(dfFREM) <- cols_to_return
  }
  
  missing_cols <- setdiff(cols_to_return, names(dfFREM))
  if (length(missing_cols) > 0) dfFREM[, missing_cols] <- NA
  
  # --- 8.5 DYNAMIC SORTING ---
  if (nrow(dfFREM) > 0 && length(cSortCols) > 0) {
    valid_sort_cols <- cSortCols[cSortCols %in% names(dfFREM)]
    
    if (length(valid_sort_cols) != length(cSortCols)) {
      warning("Some sort columns were not found in the dataset and were ignored.", call. = FALSE)
    }
    
    if (length(valid_sort_cols) > 0) {
      sort_args <- lapply(seq_along(valid_sort_cols), function(i) {
        col_name <- valid_sort_cols[i]
        
        # Safely extract direction, defaulting to 1 (ascending) if missing or NA
        direction <- if (i <= length(cSortDirection) && !is.na(cSortDirection[i])) cSortDirection[i] else 1
        
        if (direction == 1) {
          dfFREM[[col_name]]
        } else {
          -xtfrm(dfFREM[[col_name]])
        }
      })
      
      dfFREM <- dfFREM[do.call(order, sort_args), ]
    }
  }
  
  dfFREM <- dfFREM[, cols_to_return, drop = FALSE]
  
  if (!is.null(strFREMDataFileName)) {
    write.csv(dfFREM, file = strFREMDataFileName, row.names = FALSE, quote = FALSE)
  }
  
  return(as.data.frame(dfFREM))
}
