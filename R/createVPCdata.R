#' createVPCdata
#'
#'
#' @description Add combined FFEM coefficients to the data set for the specified covariates.
#' @param runno The run number to use to construct the file names (see Details).
#' @param modName The model name (see Details).
#' @param noBaseThetas The number of thetas in the base model, i.e. the number of non-covariate thetas.
#' @param noSigmas The number of sigma parameters in the model (including the sigma(s) associated with the covariates).
#' @param parNames Vector of the names of the parameters names in the base model, i.e. the names of the non-covariate parameters.
#' @param dataFile The name of the data file used in the base model, i.e. the original data file, or a data.frame with the same data.
#' @param newDataFile The name of a new data file with the FFEM columns added. Default is vpcData{runno}.csv. If NULL, will return a data frame with the data instead of writing it to disk.
#' @param availCov The names of the covariates to use when computing the FFEM coefficients (using the FREM names, which will be different for categorical covaroates). Will use all covariates if NULL.
#' @param idvar The name of the ID column,
#' @param modDevDir The path to the directory where the NONMEM runs are.
#' @param quiet Whether the FFEM expression and re-computed OMEGA matrix should be output in the terminal.
#' @param cores How many cores to use in the calculations of the FFEM expressions.
#'
#' @details This function will compute a combined term of all (FREM) covariate effects for each parameter in the FFEM model. The combined coefficients are added to the FFEM data set with names that
#' appends COV to the parameter name, e.g. the column with the combined covariate efftecs for parameter PAR will be called PARCOV. All rows for an individual will have the same value for PARCOV.
#'
#' To implement an FFEM model with the covariate effects from the FREM analysis, change the FFEM model from:
#'
#' PAR = TVPAR * EXP(ETA)
#'
#' to
#'
#' PAR = TVPAR * EXP(ETA + PARCOV)
#'
#' the estimated FREM covariate effects will now impact PAR.(The PARCOV term should be added to the corresponding ETA).
#'
#' To run a VPC for a FREM model, follow the following steps
#' \enumerate{
#' \item Create the VPC data set using this (createVPCdata) function.
#' \item Modify the FFEM model so that:
#' \itemize{
#' \item $DATA specifies the name of the vpc data file.
#' \item $INPUT includes the new columns.
#' \item the combined covariate coefficients are added to the etas (as above).
#' \item the initial estimates of the structural model thetas and sigmas are set to the estimates from the FREM model.
#' \item the initial estimate of the OMEGA BLOCK are set to be omega matrix that is printed in the terminal from the createVPCdata function (need to set quiet=FALSE).
#' \item the $SIM and $TABLE records are appropriately set.
#' }
#' \item Run the modified model :)
#' }

#' @return If newDataFile is NULL, a data frame. If quiet is FALSE, the FFEM expressions and new OMEGA matrix is printed in the console.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' createVPCdata("11",noBaseThetas=11,noSigmas=3,dataFile="../../ProducedData/Dataset/smv1DatasetWHZ2.csv",fremDataFile="frem9.dir/frem_dataset.csv",
#' parNames=c("BASE","PLMAX","HLKON","HLKOFF","BASSL","BP","BASEW","PLMAXW","HLKONW","HLKOFFW","BASSLW"),newDataFile="vpcData11.csv",
#' availCov=c("BIRTHWT","BIRTHLEN"))
#'
#' createVPCdata("13",noBaseThetas=11,noSigmas=3,dataFile="../../ProducedData/Dataset/smv1DatasetWHZ3.csv",fremDataFile="frem12.dir/frem_dataset.csv",
#'               parNames=c("BASE","PLMAX","HLKON","HLKOFF","BASSL","BP","BASEW","PLMAXW","HLKONW","HLKOFFW","BASSLW"),newDataFile="vpcData13.csv")
#' }
createVPCdata <- function(runno,modName=NULL,noBaseThetas,noSigmas,parNames=c("BASE","PLMAX","HLKON","HLKOFF","BASSL"),
                          dataFile,newDataFile=paste("vpcData",runno,".csv",sep=""),availCov=NULL,idvar="ID",
                          modDevDir=NULL,quiet=FALSE,cores=1,dfext=NULL,...) {
  
  
  if(is.null(modDevDir)) {
    if(is.null(modName)) {
      modFile    <- paste0("run",runno,".mod")
      extFile    <- paste0("run",runno,".ext")
    } else {
      modFile    <- paste0(modName,".mod")
      extFile    <- paste0(modName,".ext")
    }
  } else {
    if(is.null(modName)) {
      modFile    <- paste0(modDevDir,"/run",runno,".mod")
      extFile    <- paste0(modDevDir,"/run",runno,".ext")
    } else {
      modFile    <- paste0(modDevDir,"/",modName,".mod")
      extFile    <- paste0(modDevDir,"/",modName,".ext")
    }
  }
  
  covNames <- getCovNames(modFile)$covNames
  fremCovs <- getCovNames(modFile)$polyCatCovs
  orgCovs  <- getCovNames(modFile)$orgCovNames
  
  # It is much faster to send in extdf than to create it fo reach ID.
  # Only read it from file if it isn't passed via dfext
  if(is.null(dfext)) {
    theExtFile <- getExt(extFile)
  } else {
    theExtFile <- dfext
  }

  ## Run this to get the omega matrix to use in the vpc
  if(is.null(availCov)) availCov    <- covNames
  
  tmp <- calcFFEM(noBaseThetas=noBaseThetas,noCovThetas=length(covNames),noSigmas,dfext=theExtFile,parNames=parNames,covNames=covNames,availCov=availCov,quiet=quiet,...)

  ## Create a data set with all the original covariates + the frem-specific ones
  # Read the FFEM data set and rename the id column to ID (to simplify the coding below. The id column will get its original name in the new data file.)
  if(!is.data.frame(dataFile)) {
    data <- fread(dataFile,h=T,data.table=FALSE,showProgress=FALSE) %>% 
      renameColumn(oldvar=idvar,newvar="ID")
  } else { ## The dataFile was supplied as a data frame and not a name
    data <- dataFile %>% 
      renameColumn(oldvar=idvar,newvar="ID")
  }
  ## Add the FREM covariates to the data file
  data <- addFremCovariates(dfFFEM = data,modFile)
  
  dataI <- data %>% distinct(ID,.keep_all=TRUE)
  dataI <- dataI[,c("ID",orgCovs,covNames)]
  
  ## Go through the individuals to make sure that missing values for polycats are coded properly
  registerDoParallel(cores=cores)
  mapFun <- function(data,cov,orgCovs)  {
    for(cov in orgCovs) {
      if(data[1,cov]==-99 & length(grepl(cov,names(data))) > 1) {
        data[1,grepl(cov,names(data))] <- -99
      }
    }
    return(data)
  }
  
  dataI <- foreach(k = 1:nrow(dataI)) %dopar% {
    mapFun(data=dataI[k,],cov=cov,orgCovs=orgCovs)
  }
  
  dataI <- data.frame(rbindlist(dataI))
  
  
  
  dataI <- dataI[,c("ID",covNames)]
  dataMap <- dataI[]
  dataMap[,covNames] <- TRUE
  
  for(c in covNames) {
    dataMap[,c] <- ifelse(dataI[,c]==-99,FALSE,TRUE)
  }
  
  ## Loop over each individual to compute their covariate contributions ##
  myFun <- function(data,parNames,dataMap=dataMap,availCov=NULL) {
    ID <- data$ID
    if(is.null(availCov)) {
      availCov  <- covNames[as.logical(dataMap[dataMap$ID==ID,-1])]
    } else {
      myCov    <- covNames[as.logical(dataMap[dataMap$ID==ID,-1])]
      availCov <- myCov[myCov %in% availCov]
    }
    
    ffemObj   <- calcFFEM(noBaseThetas=noBaseThetas,noCovThetas=length(covNames),noSigmas,dfext=theExtFile,parNames=parNames,covNames=covNames,availCov=availCov,quiet=TRUE,...)
    
    retDf <- data.frame(ID=ID)
    for(i in 1:length(parNames)) {
      retDf[[parNames[i]]] <- as.numeric(eval(parse(text=ffemObj$Expr[i])))
    }
    
    return(retDf)
  }
  
  dataOne <- data %>% distinct(ID,.keep_all=TRUE)
  
  covEff <- foreach(k = 1:nrow(dataOne)) %dopar% {
    myFun(data=dataOne[k,],parNames,dataMap=dataMap,availCov=availCov)
  }
  
  covEff <- data.frame(rbindlist(covEff))
  
  
  data2 <- left_join(data,covEff)
  
  # Remove the frem covariates
  data3 <- data2[,!(names(data2) %in% fremCovs)]
  
  ## Give the ID column back its original name
  data3 <- data3 %>% renameColumn(oldvar="ID",newvar=idvar)
  
  # Write the data to file
  if(!is.null(newDataFile)) {
    write.csv(data3,file=newDataFile,quote = FALSE,row.names = FALSE)
  } else {
    data3
  }
}
