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
#' @param dataFile The name of the data file used in the base model, i.e. the original data file, or a data,frame with the same data.
#' @param parTab NULL or the path to a NONMEM table file that should contain at least the columns ID, TVPAR names and etas (see Details).
#' @param availCov NULL, "none" or a vector of strings specifying the covariates to compute the covariate effects for.
#' @param cores The number of cores to use for the calculation of the covariate effects.
#'
#' @details The function will generate three data.frames (returned as a list) with the typical individual parameters, etas and covariate effects for the specified
#' covariates (availCov).
#'
#' If parTab is NULL, the tvpar and etas components of the returned list will be NULL.
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
#' availCov="LBBSA",cores=4)
#' }
calcParmeters <- function(runno=NULL,
                           numNonFREMThetas,
                           modName       = NULL,
                           covSuffix     = "FREMCOV",
                           parNames      = paste("Par",1:numParCov,sep=""),
                           numParCov     = NULL,
                           numSkipOm     = 0,
                           dataFile,
                           newDataFile   = paste("vpcData",runno,".csv",sep=""),
                           availCov      = "all",
                           idvar         = "ID",
                           modDevDir     = NULL,
                           quiet         = FALSE,
                           cores         = 1,
                           dfext         = NULL,
                           FFEMData      = NULL,
                           covmodel      = "linear",
                           ...) {
  
  

  fileNames <- getFileNames(runno=runno,modName=modName,modDevDir=modDevDir,...)
  modFile   <- fileNames$mod
  extFile   <- fileNames$ext
  phiFile   <- fileNames$phi
  
  dfphi<-getPhi(phiFile)
  
  dfExt<-getExt(extFile)

  dfone<-FFEMData$newData[!duplicated(FFEMData$newData[[idvar]]),c(idvar,vpcData$indCovEf)]
  
  if (nrow(dfphi)!=nrow(dfone)) error("Number of unique individuals in the dataset and phi file needs to be the same")

  etafrem<-dfphi[,3:(2+numSkipOm+nrow(vpcData$Omega))]
  etaprim<-etafrem
  for (i in 1:length(vpcData$indCovEff)) {
    etaprim[,(i+numSkipOm)]<-etafrem[,(i+numSkipOm)]-dfone[,i+1]
  }
  
  names(etaprim)<-paste0(names(etaprim),"_PRIM")

  if (nrow(dfExt)>1) dfExt  <- dfExt[dfExt$ITERATION==-1000000000,]
  
  numFREMThetas<-length(grep("THETA",names(dfExt)))-numNonFREMThetas
  df_th  <- as.numeric(dfExt[,2:(numNonFREMThetas+1)])
  df_thm <- as.numeric(dfExt[,(numNonFREMThetas+2):(numNonFREMThetas+1+numFREMThetas)])
  
  
  covariates<-dfphi[,(3+numSkipOm+nrow(vpcData$Omega)):((2+numSkipOm+nrow(vpcData$Omega))+numFREMThetas)]
  names(covariates)<-getCovNames(modFile = modFile)$covNames
  if (covmodel=="linear") {
    for (i in 1:length(df_thm)) {
      covariates[,i]<-covariates[,i]+df_thm[i]
    }
  }

  return(cbind(dfphi[,2],etaprim,etafrem,covariates))

}
  

