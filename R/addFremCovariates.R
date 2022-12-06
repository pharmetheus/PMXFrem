#' addFREMcovariates
#'
#' @description Adds the binarised FREM covariates to the FFEM data set.
#' The columns will contain the binarised versions of the polycothomous covariates
#' named with the original covariate name appended by "_#", where "#" is the level value of the covariate. For example, for a covariate NCIL
#' with the values 0, 1 and 2 addFREMcovariates will add columns NCIL_1 and NCIL_2 each with the values 0 and 1 for rows where the value of NCIL is not 1/2
#' and 1 when the in NCIL is 1 or 2, respectively.
#' @param dfFFEM Data frame with FFEM data
#' @param modFile name of the FREM model file.
#'
#' @return A data frame with columns appended.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' dfFFEMnew <-addFREMcovariates(dfFFEM,modFile = modFile)
#'
#' }
addFREMcovariates <- function(dfFFEM,modFile) {

  fremCovs <- getCovNames(modFile)$polyCatCovs

  for(cov in fremCovs) {
    myCov <- str_replace(cov,"_[0-9]*","")
    myCovNum <- str_replace(cov,paste0(myCov,"_"),"")
    dfFFEM[[cov]] <- ifelse(dfFFEM[[myCov]]==myCovNum,1,0)
  }

  ## Restore column order FFEM, FREMTYPE,fremcovs
    return(dfFFEM)
}
