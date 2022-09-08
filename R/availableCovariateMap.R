#' availableCovariateMap
#'
#' @description Creates a data frame with individual information about missing covariates. Each row holds information about one individual and each (frem) covariate has one column. 
#' The value for each ID and covariate combination is either TRUE or FALSE, indicating wether the covariate is missing or not (TRUE=not missing, FALSE=missing).
#' @param modFile The FREM model file name.
#' @param analysisData A data frame with the FFEM data set.
#' @param cores Number of cores to use in the foreach loop.
#'
#' @return A data.frame with nrow=teh number of IDs in analysisData and ncol=1+teh number of covariates in modFile. The first column holds the ID numbers.
#' @export
#'
#' @examples
#' \dontrun{
#' dataMap <- availableCovariateMap(modFile,analysisData)
#' }
availableCovariateMap <- function(modFile,analysisData,cores=1) {
  
  ## Create the map of available covariates per subject
  covNames   <- getCovNames(modFile)$covNames
  fremCovs   <- getCovNames(modFile)$polyCatCovs
  orgCovs    <- getCovNames(modFile)$orgCovNames
  
  ffemDataSet <- analysisData %>% 
    addFremCovariates(.,modFile) %>% 
    distinct(ID,.keep_all=TRUE) %>% 
    select(ID,orgCovs,covNames)
  
  mapFun <- function(data,orgCovs)  {
    for(cov in orgCovs) {
      if(data[1,cov]==-99 & length(grepl(cov,names(data))) > 1) {
        data[1,grepl(cov,names(data))] <- -99
      }
    }
    return(data)
  }
  
  ## Start cluster
  registerDoParallel(cores=cores)
  dataI <- foreach(k = 1:nrow(ffemDataSet)) %dopar% {
    mapFun(data=ffemDataSet[k,],orgCovs=orgCovs)
  }
  
  dataI              <- data.frame(rbindlist(dataI))
  dataI              <- dataI[,c("ID",covNames)]
  dataMap            <- dataI[]
  dataMap[,covNames] <- TRUE
  
  for(c in covNames) {
    dataMap[,c] <- ifelse(dataI[,c]==-99,FALSE,TRUE)
  }
  
  return(dataMap)
}