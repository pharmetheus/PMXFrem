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
#'   the FREM model file.
#' @return A data.frame that can be used as the dfCovs argument to
#'   `getExplainedVar`.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' setupdfCovs(modFile)
#'
#' #   AGE ALT AST BILI BMI BSA CRCL ETHNIC GENO2  HT LBWT NCIL RACEL SEX SMOK  WT
#' # 1    1   1   1    1   1   1    1      1     1   1    1    1     1   1    1   1
#' # 2    1 -99 -99  -99 -99 -99  -99    -99   -99 -99  -99  -99   -99 -99  -99 -99
#' # 3  -99   1 -99  -99 -99 -99  -99    -99   -99 -99  -99  -99   -99 -99  -99 -99
#' # 4  -99 -99   1  -99 -99 -99  -99    -99   -99 -99  -99  -99   -99 -99  -99 -99
#' # 5  -99 -99 -99    1 -99 -99  -99    -99   -99 -99  -99  -99   -99 -99  -99 -99
#' # 6  -99 -99 -99  -99   1 -99  -99    -99   -99 -99  -99  -99   -99 -99  -99 -99
#' # 7  -99 -99 -99  -99 -99   1  -99    -99   -99 -99  -99  -99   -99 -99  -99 -99
#' # 8  -99 -99 -99  -99 -99 -99    1    -99   -99 -99  -99  -99   -99 -99  -99 -99
#' # 9  -99 -99 -99  -99 -99 -99  -99      1   -99 -99  -99  -99   -99 -99  -99 -99
#' # 10 -99 -99 -99  -99 -99 -99  -99    -99     1 -99  -99  -99   -99 -99  -99 -99
#' # 11 -99 -99 -99  -99 -99 -99  -99    -99   -99   1  -99  -99   -99 -99  -99 -99
#' # 12 -99 -99 -99  -99 -99 -99  -99    -99   -99 -99    1  -99   -99 -99  -99 -99
#' # 13 -99 -99 -99  -99 -99 -99  -99    -99   -99 -99  -99    1   -99 -99  -99 -99
#' # 14 -99 -99 -99  -99 -99 -99  -99    -99   -99 -99  -99  -99     1 -99  -99 -99
#' # 15 -99 -99 -99  -99 -99 -99  -99    -99   -99 -99  -99  -99   -99   1  -99 -99
#' # 16 -99 -99 -99  -99 -99 -99  -99    -99   -99 -99  -99  -99   -99 -99    1 -99
#' # 17 -99 -99 -99  -99 -99 -99  -99    -99   -99 -99  -99  -99   -99 -99  -99   1
#'
#' setupdfCovs(modFile, fremCovs = c("AGE", "SEX"), additionalCovs = "FORM")
#'
#' # AGE SEX FORM
#' # 1   1   1    1
#' # 2   1 -99  -99
#' # 3 -99   1  -99
#' # 4 -99 -99    1
#' }
setupdfCovs <- function(modFileName,
                        fremCovs       = getCovNames(modFile = modFileName)$orgCovNames,
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
    dfCovs[i, names(dfCovs) != names(dfCovs)[i - 1]] <- -99
  }

  return(dfCovs)
}
