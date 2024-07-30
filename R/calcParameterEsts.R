#' Calculate parameter estimates from FREM output to be used in a parameter
#' table
#'
#' This is an internal utility function that facilitates a standardized way of
#' computing FFEM parameter estimates from FREM results.
#'
#' @inheritParams calcFFEM
#' @param parVector A vector of values corresponding to the line with final
#'   parameter estimates from a NONMEM ext file.
#' @param thetaNum The theta numbers to include in the output. This refers to
#'   the number associated with the theta value in `parVector`. `
#' @param omegaNum The omega numbers to include in the output. This refers to
#'   the number associated with the omega value in `parVector`. `
#' @param sigmaNum The sigma numbers to include in the output. This refers to
#'   the number associated with the omega value in `parVector`. `
#'
#' @return A vector of parameter estimates corresponding to `thetaNum`,
#'   `omegaNum`and `sigmaNum`.
#' @export
#' @keywords internal
#' @seealso [fremParameterTable()]
#'
#' @examples
#' \dontrun{
#'  fremParEsts <- calcParameterEsts(extRes,thetaNum,omegaNum,sigmaNum,numNonFREMThetas,numSkipOm,
#' covNames=covNames, availCov=availCov,quiet=quiet)
#' }
calcParameterEsts <- function(parVector,thetaNum,omegaNum,sigmaNum,numNonFREMThetas,numSkipOm,...) {

  #Find the thetas
  thetaEsts <- parVector %>% select(one_of(paste0("THETA",thetaNum)))

  ## Figure out the number of FREM omegas
  fremOmegas <- diag(calcFFEM(parVector,
                              numNonFREMThetas = numNonFREMThetas,
                              numSkipOm        = numSkipOm,
                              ...)$Vars)

  ## Figure out the non-FREM omegas to extract from the ext information
  extOmegaNum <- head(omegaNum,-length(fremOmegas))
  if(length(extOmegaNum)>0) {
    extOmegas   <- paste0("OMEGA.",extOmegaNum,".",extOmegaNum,".")
  } else {
    extOmegas <- NULL
  }

  ## Figure out the sigmas to extract from the ext info
  if(!is.null(sigmaNum)) {
    extSigmas   <- paste0("SIGMA.",sigmaNum,".",sigmaNum,".")
  }

  res <- c(as.numeric(thetaEsts))
  if(!is.null(extOmegas)) res <- c(res,as.numeric(parVector %>% select(one_of(extOmegas))))
  res <- c(res,fremOmegas)
  if(!is.null(sigmaNum)) res <- c(res,as.numeric(parVector %>% select(one_of(extSigmas))))


#     {
#     res <- c(
#       as.numeric(thetaEsts),
# as.numeric(parVector %>% select(one_of(extOmegas))),
#       fremOmegas,
#       as.numeric(parVector %>% select(one_of(extSigmas)))
#     ) } else {
#       res <- c(
#         as.numeric(thetaEsts),
#         fremOmegas,
#         as.numeric(parVector %>% select(one_of(extSigmas)))
#       )
#     }

  return(res)
}
