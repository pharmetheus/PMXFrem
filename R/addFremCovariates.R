#' Add FREM covariates to a FFEM data set
#'
#' Add binarised FREM covariates to a FFEM data set.
#'
#' This is mainly an internal utility function.
#'
#' Binarised covariates will be added based either on the covariates present in
#' a frem model (`modFile`) file or from the names specified in the `covariates`
#' vector.
#'
#' The new columns in the FFEM data set will contain the binarised versions of
#' the polychotomous  covariates named with the original covariate name appended
#' by "_#", where "#" is the level value of the covariate. For example, for a
#' covariate NCIL with the values 0, 1 and 2 `addFREMcovariates()` will add
#' columns NCIL_1 and NCIL_2 each with the values 0 and 1 for rows where the
#' value of NCIL is not 1/2 and 1 when the in NCIL is 1 or 2, respectively. If
#' an ID has a missing value of NCIL, then NCIL_1 and NCIL_2 will both be 0,
#' effectively imputing the missing NCIL with the lowest level of the NCIL
#' covariate.
#'
#' The binarisation itself is performed by [PMXForest::oneHotEncode()]; this
#' function is the FREM-aware wrapper that derives the columns to create from a
#' model file or a covariate vector.
#'
#' @param dfFFEM A data.frame containing the FFEM data.
#' @param modFile Path to a FREM model file.
#' @param covariates A character vector of covariates to binarise.
#' @param includeReference Logical. If TRUE, creates a dummy column for the lowest
#'   reference level as well (default: FALSE).
#' @param imputeMissing Logical. If TRUE (default), missing values are imputed to 0
#'   (the reference category). If FALSE, missing values are preserved as missVal.
#' @param missVal The integer/numeric value representing missing data (default: -99).
#'
#' @return A data.frame with the new binarised covariate columns.
#'
#' @examples
#' library(dplyr)
#'
#' # Load the base dataset
#' dfFFEM <- read.csv(
#'   system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package="PMXFrem")) %>%
#'   filter(BLQ != 1)
#'
#' modFile <- system.file("extdata/SimNeb/run31.mod", package="PMXFrem")
#'
#' ## Example 1: Add covariates automatically based on a FREM model file
#' dfFFEM_mod <- addFREMcovariates(dfFFEM, modFile = modFile)
#'
#' ## Example 2: Add specific polychotomous covariates (Default: Y-1 categories)
#' # For RACEL (levels 1, 2, 3), this will generate RACEL_3 and RACEL_2.
#' # The lowest valid level (1) is dropped as the reference category.
#' dfFFEM_standard <- addFREMcovariates(dfFFEM, covariates = "RACEL")
#'
#' ## Example 3: Add specific covariates AND include the reference category
#' # This will generate RACEL_3, RACEL_2, AND RACEL_1.
#' dfFFEM_full <- addFREMcovariates(dfFFEM, covariates = "RACEL", includeReference = TRUE)
#'
#' @family Data Assembly Internal
#' @concept data_assembly
#' @export
addFREMcovariates <- function(dfFFEM, modFile=NULL, covariates=NULL, missVal = -99, includeReference = FALSE, imputeMissing = TRUE) {

  if(!is.data.frame(dfFFEM)) stop("dfFFEM has to be a data.frame")
  if(is.null(modFile) && is.null(covariates)) stop("modFile and covariates can not both be NULL")

  ## Build a one-hot encoding spec: one entry per base covariate, with the exact
  ## levels (and column order) this function has historically produced.
  spec <- list()

  if(!is.null(modFile)) {
    fremCovs <- getCovNames(modFile)$polyCatCovs
    for(cov in fremCovs) {
      myCov    <- stringr::str_replace(cov, "_[0-9]*", "")
      myCovNum <- as.numeric(stringr::str_replace(cov, paste0(myCov, "_"), ""))
      spec[[myCov]]$levels <- c(spec[[myCov]]$levels, myCovNum)
    }
    for(myCov in names(spec)) {
      ## FREM reference level: one below the lowest binarised level.
      spec[[myCov]]$ref <- min(spec[[myCov]]$levels) - 1
    }
  }

  if(!is.null(covariates)) {
    addCovs <- c()
    for(cov in covariates) {
      if(!(cov %in% names(dfFFEM))) {
        warning(cov, " does not exist in the data set")
        next
      }

      valid_vals <- unique(dfFFEM[[cov]])
      valid_vals <- valid_vals[valid_vals != missVal & !is.na(valid_vals)]

      if(length(valid_vals) <= 1) {
        warning(cov, " has only one non-missing level, not added to data set.")
        next
      }
      if(length(valid_vals) == 2) {
        warning(cov, " has only two non-missing levels, not added to data set.")
        next
      }
      addCovs <- c(addCovs, cov)
    }

    if(length(addCovs) == 0) stop("No binarised covariates to add to the FFEM data.")

    for(cov in addCovs) {
      valid_vals <- sort(unique(dfFFEM[[cov]][dfFFEM[[cov]] != missVal & !is.na(dfFFEM[[cov]])]))
      nonRef     <- if(includeReference) valid_vals else valid_vals[-1]
      ## Highest level first, matching the historical column order.
      spec[[cov]] <- list(ref = valid_vals[1], levels = rev(nonRef))
    }
  }

  if(length(spec) == 0) return(dfFFEM)

  PMXForest::oneHotEncode(
    dfFFEM,
    spec             = spec,
    sep              = "_",
    missVal          = missVal,
    includeReference = FALSE,          # handled via the explicit `levels` above
    imputeMissing    = imputeMissing,
    dropOriginal     = FALSE
  )
}
