#' Create the input matrix for `getExplainedVar`
#'
#' Create the dfCovs data.frame that is an argument to the `getExplainedVar`.
#' function.
#'
#' @param modFileName A path to the FREM model file.
#' @param fremCovs A vector of covariates that are part of the FREM definition
#'   of the FREM model file. Default is all, i.e. `getCovNames(modFile =
#'   modFileName)$orgCovNames`
#' @param additionalCovs Any additional covariates to be included in the output
#'   dfCovs. For example covariates that are part of the fixed effects part of
#'   the FREM model file
#' @param missVal Numeric. Missing value indicator.
#' @return A data.frame that can be used as the dfCovs argument to
#'   `getExplainedVar`.
#' @export
#'
#' @examples
#'
#' modFile <- system.file("extdata", "SimNeb", "run31.mod",package = "PMXFrem")
#' 
#' # Use a covariates in the FREM model file
#' setupDfCovsEV(modFile)
#' 
#' # Use only a subset of the covariates in the FREM specification and add an additional covariate.
#' setupDfCovsEV(modFile, fremCovs = c("AGE", "SEX"), additionalCovs = "FORM")
#' 
#' @family Data Assembly
#' @concept data_assembly
setupDfCovsEV <- function(modFileName,
                        fremCovs       = getCovNames(modFile = modFileName)$orgCovNames,
                        missVal = -99,
                        additionalCovs = NULL) {
  ## Get the covariates from the model
  covNames <- getCovNames(modFile = modFileName)

  # ## Input check
  if (!all(fremCovs %in% covNames$orgCovName)) stop(paste("One or more covariates in fremCovs are not present in the FREM part of the model."))

  # ## Merge fremCovs and additionalCovs
  covs <- unique(c(fremCovs, additionalCovs))

  dfCovs <- data.frame(matrix(ncol = length(covs), nrow = length(covs) + 1))
  names(dfCovs) <- covs
  dfCovs <- dfCovs %>%  mutate_all(function(x) 1)

  for (i in 2:nrow(dfCovs)) {
    dfCovs[i, names(dfCovs) != names(dfCovs)[i - 1]] <- missVal
  }

  return(dfCovs)
}
