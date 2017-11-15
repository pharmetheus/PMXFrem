#' calcParameters
#'
#' @description Collects the typical individual parameters, etas and covariate effects from a FREM run.
#' @param runno The run number to use to construct the file names (see Details).
#' @param modName The model name (see Details).
#' @param modDevDir The path to the directory where the NONMEM runs are.
#' @param noBaseThetas The number of thetas in the base model, i.e. the number of non-covariate thetas.
#' @param noEtas The number of etas in the model.
#' @param noSigmas The number of sigma parameters in the model (including the sigma(s) associated with the covariates).
#' @param parNames Vector of the names of the parameters names in the base model, i.e. the names of the non-covariate parameters.
#' @param noParCov Number of parameters for which covariate relations are sought (often the same as noBaseThetas).
#' @param dataFile The name of the data file used in the base model, i.e. the original data file.
#' @param fremData File The name of the data file used for the frem model.
#' @param parTab The path to a NONMEM table file that should contain at least the columns ID, TVPAR names and etas (see Details).
#' @param availCov NULL, "none" or a vector of strings specifying the covariates to compute the covariate effects for.
#' @param cores The number of cores to use for the calculation of the covariate effects.
#'
#' @details The function will generate three data.frames (returned as a list) with the typical individual parameters, etas and covariate effects for the specified 
#' covariates (availCov). 
#' 
#' The typical parameters have to be named "TV" followed by the names in parNames. E.g. if one element in parNames is CL, then the 
#' corresponding typical individual parameter name should be TVCL. Unless the model includes structural covariates (covairiates that are included in the model "outside" the
#' the FREM part), all rows in tvpar will be identical.
#' 
#' The etas in the data.frame etas are the ones estimated from the FREM model.
#' 
#' The covs data.frame will have one column for each of the parameters in parNames. If noEtas < noBaseThetas, then the additional columns (noEtas - noBaseThetas) will be all zeros.
#' 
#' If modName is NULL is specified, it will be assumed that the files associated with the model will be names run[runno].mod, run[runno].ext, etc (e.g. run1.mod, run1.ext). If modName is not
#' NULL, the files will be assumed to have names like modName.mod, modName.ext, etc.
#' 
#' @return A three element list: tvpar, etas and covs. Each of the elements is a data.frame with the same number of rows as parTab.
#' @export
#'
#' @examples
#' \dontrun{
#' tmp <- calcParameters(45,modDevDir="/PMX/Projects/Pharmetheus/PMX-FREMPRES-PMX-1/Analysis/Model/Tanezumab",noBaseThetas=7,noEtas=6,noSigmas=3,
#' parNames = c("CL","V1","Q","V2","VM","KM","MIX"),
#' parTab = "/PMX/Projects/Pharmetheus/PMX-FREMPRES-PMX-1/Analysis/Model/Tanezumab/xptab45",
#' dataFile="/PMX/Projects/Pharmetheus/PMX-FREMPRES-PMX-1/Analysis/ProducedData/Dataset/tanezumabFREMPresData3.csv",
#' fremDataFile="/PMX/Projects/Pharmetheus/PMX-FREMPRES-PMX-1/Analysis/Model/Tanezumab/frem44.dir/frem_dataset.dta",
#' availCov="LBBSA",cores=4)
#' }
calcParameters <- function(runno,modName=NULL,modDevDir,noBaseThetas,noEtas=noBaseThetas,noSigmas=2,noParCov=noEtas,dataFile,fremDataFile,parTab,
                           parNames,availCov=NULL,cores=4) {
 
  ## Will return a list with three data.frames: TVPAR, etas and covs 
  ## parTab is a NONMEM table file from the frem run with the TV parameters  (theta + potential ) and the etas.
 
  # availCov = "none": don't add any covariate effects
  # availCov = NULL: add all covariate effects
  # availCov = covnameVector: add the covariate effects from the covariates in covnameVector 
  
  # addEtas = FALSE: Don't add etas to the individual parameters
  # addEtas = TRUE: Add etas to the individual parameters
  
  ## Read the parTab
    parTab <- fread(parTab,skip=1,h=T,data.table=FALSE,check.names = TRUE,verbose=FALSE,showProgress=FALSE) %>% 
      select(one_of(c("ID",paste0("TV",parNames),paste0("ETA",1:noEtas)))) %>% distinct(ID,.keep_all=TRUE)
  
  ## If we are not interested in any covariates
  if(is.null(availCov) || availCov!="none") {
    
    fullFREMdata <- createVPCdata(runno,modName=modName,modDevDir = modDevDir,noBaseThetas=noBaseThetas,noSigmas=noSigmas,noParCov=noParCov,dataFile=dataFile,fremDataFile=fremDataFile,
                                  parNames=parNames,newDataFile=NULL,quiet=TRUE,availCov=availCov,cores=cores)
    
    covs <- fullFREMdata %>% distinct(ID,.keep_all=TRUE) %>% select(one_of("ID",parNames))
    
    ret.list <- list(
      tvpar = parTab %>% select(one_of(c("ID",paste0("TV",parNames)))),
      etas  = parTab %>% select(one_of(c("ID",paste0("ETA",1:noEtas)))),
      covs  = covs
    )
  } else {
    
    covs        <- data.frame(matrix(0,ncol=noBaseThetas,nrow=nrow(parTab)))
    names(covs) <- parNames

    ret.list <- list(
      tvpar = parTab %>% select(one_of(c("ID",paste0("TV",parNames)))),
      etas  = parTab %>% select(one_of(c("ID",paste0("ETA",1:noEtas)))),
      covs  = covs
    )
  
  }
  
  return(ret.list)
}



