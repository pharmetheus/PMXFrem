#' calcNumParCov
#'
#' @description Calculate the number of parameters for which covariates are sought.
#' @param dfext 
#' @inheritParams calcFFEM
#'
#' @return A scalar. The number of parameters for which covariates are sought.
#' @export
#'
#' @examples
#' \dontrun{
#' calcNumParCov(dfExt,numNonFREMThetas = 9,numSkipOm = 2)
#' }
calcNumParCov <- function(dfext, numNonFREMThetas,
                          numSkipOm=0) {
 
  numFREMThetas <- length(grep("THETA",names(dfext)))-numNonFREMThetas
  
  iNumOmega <- length(grep("OMEGA", names(dfext)))
  numTotEta <- -1 / 2 + sqrt(1 / 4 + 2 * iNumOmega)
  numParCov <- numTotEta - numSkipOm - numFREMThetas
  
  return(numParCov)
}