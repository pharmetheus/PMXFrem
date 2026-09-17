#' Create the input matrix for `getExplainedVar`
#'
#' Create the dfCovs data.frame that is an argument to the `getExplainedVar`.
#' function.
#'
#' @param modFileName A path to the FREM model file.
#' @param fremCovs A vector of covariates that are part of the FREM definition
#'   of the FREM model file. Default is all, i.e. `getCovNames(modFile =
#'   modFileName)$orgCovNames`
#' @param conditionalCovs Covariates to include in the output `dfCovs` beyond
#'   the FREM ones - typically covariates in the fixed-effects part of the FREM
#'   model file. Unlike the FREM covariates, which are isolated one row at a
#'   time, these are set to 1 in *every* row: `getExplainedVar()` then reports
#'   what each FREM covariate explains with these in the model. Note that their
#'   own contribution is part of every row. In types 1-3 of `getExplainedVar()`
#'   the value used is each subject's own, from `data`, not the 1 in `dfCovs`.
#' @param additionalCovs Deprecated. Use `conditionalCovs`.
#' @param missVal Numeric. Missing value indicator.
#' @return A data.frame that can be used as the dfCovs argument to
#'   `getExplainedVar`.
#' @export
#'
#' @examples
#'
#' modFile <- system.file("extdata", "SimNeb", "run31.mod", package = "PMXFrem")
#'
#' # Use a covariates in the FREM model file
#' setupDfCovsEV(modFile)
#'
#' # Use only a subset of the covariates in the FREM specification and add an additional covariate.
#' setupDfCovsEV(modFile, fremCovs = c("AGE", "SEX"), conditionalCovs = "FORM")
#'
#' @family Data Assembly
#' @concept data_assembly
setupDfCovsEV <- function(modFileName,
                          fremCovs = getCovNames(modFile = modFileName)$orgCovNames,
                          missVal = -99,
                          conditionalCovs = NULL,
                          additionalCovs = NULL) {
  conditionalCovs <- .additionalToConditional(additionalCovs, conditionalCovs)

  ## Get the covariates from the model
  covNames <- getCovNames(modFile = modFileName)

  # ## Input check
  if (!all(fremCovs %in% covNames$orgCovName)) stop(paste("One or more covariates in fremCovs are not present in the FREM part of the model."))

  # ## Merge fremCovs and conditionalCovs
  covs <- unique(c(fremCovs, conditionalCovs))

  dfCovs <- data.frame(matrix(ncol = length(covs), nrow = length(covs) + 1))
  names(dfCovs) <- covs
  dfCovs <- dfCovs %>% mutate_all(function(x) 1)

  for (i in 2:nrow(dfCovs)) {
    dfCovs[i, names(dfCovs) != names(dfCovs)[i - 1]] <- missVal
  }

  ## The conditional covariates are what every row is conditioned on, so they
  ## stay on in all of them; only the FREM covariates are isolated one row at a
  ## time. Left missing in the other rows, as this used to do, nothing was
  ## conditioned on them unless the caller filled the column by hand.
  if (length(conditionalCovs) > 0) dfCovs[, conditionalCovs] <- 1

  return(dfCovs)
}

## Fold the deprecated `additionalCovs` into `conditionalCovs`.
##
## The old name says when it was added rather than what it does; the covariates
## it carries are the ones getExplainedVar() conditions on. Renamed, with the
## old spelling kept working so nothing breaks, following the same shape as
## PMXForest's refLevels -> catRef.
##
## @noRd
.additionalToConditional <- function(additionalCovs, conditionalCovs) {
  if (is.null(additionalCovs)) {
    return(conditionalCovs)
  }
  if (!is.null(conditionalCovs)) {
    stop("Supply either `conditionalCovs` or the deprecated `additionalCovs`, ",
      "not both.",
      call. = FALSE
    )
  }
  warning("`additionalCovs` is deprecated in setupDfCovsEV(); use ",
    "`conditionalCovs` instead. It names the covariates getExplainedVar() ",
    "conditions on, which is what they are for.",
    call. = FALSE
  )
  additionalCovs
}
