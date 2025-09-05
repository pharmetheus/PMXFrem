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
#' an ID has a missing vaue of NCIL, then NCIL_1 and NCIL_2 will both be 0,
#' effectively imputing the missing NCIL with the lowest level of the NCIL
#' covariate.
#'
#' The covariate to add can come either from a frem model file or be specified
#' with the argument `covariates`.
#'
#' @param dfFFEM Data frame with FFEM data
#' @param modFile name of the FREM model file to extract polychotomous
#'   covariates from
#' @param covariates character vector of polychotomous covariates to add
#'   binarised columns for
#' @param iMiss The missing value indicator. Default is -99.
#'
#' @return A data frame with columns appended.
#' @export
#'
#' @examples
#' library(readr)
#' library(dplyr)
#' library(magrittr)
#'
#' dfFFEM <- read_csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv",package="PMXFrem"),
#' show_col_types = FALSE) %>% filter(BLQ!=1)
#' modFile <- system.file("extdata/SimNeb/run31.mod",package="PMXFrem")
#'
#' ## Add covariates from a FREM model file
#' dfFFEMnew <-addFREMcovariates(dfFFEM,modFile = modFile)
#'
#' ## Add the covariates specified on the command line
#' dfFFEMnew <-addFREMcovariates(dfFFEM,covariates ="RACEL")
addFREMcovariates <- function(dfFFEM,modFile=NULL,covariates=NULL,iMiss = -99) {

  ## Check input
  if(!is.data.frame(dfFFEM)) stop("dfFFEM has to be a data.frame")
  if(is.null(modFile) && is.null(covariates)) stop("modFile and covariates can not both be NULL")

  ## Add columns in the FFEM data frame to match the polychotomous  covariates in
  ## a FREM model
  if(!is.null(modFile)) {
    fremCovs <- getCovNames(modFile)$polyCatCovs
    for(cov in fremCovs) {
      myCov         <- str_replace(cov,"_[0-9]*","")
      myCovNum      <- str_replace(cov,paste0(myCov,"_"),"")
      dfFFEM[[cov]] <- ifelse(dfFFEM[[myCov]]==myCovNum,1,0)
    }
  }

  ## Add binarised versions of the specified covariates
  if(!is.null(covariates)) {

    addCovs <- c()
    for(cov in covariates) {

      if(!(cov %in% names(dfFFEM))) {
        warning(cov," does not exist in the data set")
        next
      } else {
        dfFFEMnoMiss <-  dfFFEM[dfFFEM[[cov]] != iMiss, ]
      }

      if(length(unique(dfFFEMnoMiss[[cov]])) == 1) {
        warning(cov," has only one non-missing level, not added to data set.")
        next
        }
      if(length(unique(dfFFEMnoMiss[[cov]])) == 2) {
        warning(cov," has only two non-missing levels, not added to data set.")
        next
      }

      addCovs <- c(addCovs,cov)
    }

    if(length(addCovs)==0) stop("No binarised covariates to add to the FFEM data.")

    ## Process the covariates

    ## Get the unique values of the covariates in reverse order and exclude the
    ## lowest number (to match what frem is doing)
    for(cov in addCovs) {
      covVal        <- rev(sort(unique(dfFFEMnoMiss[[cov]]))[-1])
      # browser()
      for(myCovNum in covVal) {
        newCovName <- paste0(cov,"_",myCovNum)
        dfFFEM[[newCovName]] <- ifelse(dfFFEM[[cov]]==myCovNum,1,0)
      }
    }
  }

  ## Restore column order FFEM, FREMTYPE,fremcovs
  return(dfFFEM)
}
