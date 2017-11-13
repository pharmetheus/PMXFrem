#' addFremCovariates
#'
#' @description Adds the FREM covariates to teh FFEM file. Internal function
#' @param dfFFEM Data frame with FFEM data
#' @param modFile name of the FREM model file.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' \dontrun{
#' covList <- getCovNames(modFile="fremModel.mod")
#' }
addFremCovariates <- function(dfFFEM,modFile) {

  fremCovs <- getCovNames(modFile)$polyCatCovs

  for(cov in fremCovs) {
    myCov <- str_replace(cov,"_[0-9]*","")
    myCovNum <- str_replace(cov,paste0(myCov,"_"),"")
    dfFFEM[[cov]] <- ifelse(dfFFEM[[myCov]]==myCovNum,1,0)
  }

  ## Restore column order FFEM, FREMTYPE,fremcovs
    return(dfFFEM)
}
