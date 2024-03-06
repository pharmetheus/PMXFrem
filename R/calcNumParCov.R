#' Calculate the number of parameters in a FREM model that are associated with
#' covariates.
#'
#' @param dfext The NONMEM .ext file from the FREM run.
#' @inheritParams calcFFEM
#'
#' @return A scalar. The number of parameters for which covariates are sought.
#' @export
#'
#' @examples
#' extFile <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
#' dfExt   <- getExt(extFile = extFile)
#' n       <- calcNumParCov(dfExt, numNonFREMThetas = 7, numSkipOm = 2)
calcNumParCov <- function(dfext,
                          numNonFREMThetas,
                          numSkipOm = 0) {

  numFREMThetas <- length(grep("THETA", names(dfext))) - numNonFREMThetas

  iNumOmega <- length(grep("OMEGA", names(dfext)))
  numTotEta <- -1 / 2 + sqrt(1 / 4 + 2 * iNumOmega)
  numParCov <- numTotEta - numSkipOm - numFREMThetas

  return(numParCov)
}
