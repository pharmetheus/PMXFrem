#' createVPCdata
#'
#'
#' @description Add combined FFEM coefficients to the data set for the specified covariates.
#' @inheritParams calcFFEM 
#' @inheritParams getFileNames
#' @param dataFile The name of the data file used in the base model, i.e. the original data file, or a data.frame with the same data.
#' @param newDataFile The name of a new data file with the FFEM columns added. Default is vpcData{runno}.csv. If NULL, will return a data frame with the data instead of writing it to disk.
#' @param idvar The name of the ID column,
#' @param cores How many cores to use in the calculations of the FFEM expressions.
#'
#' @details This function will compute a combined term of all (FREM) covariate effects for each parameter in the FFEM model and create a 
#' new data file with these combined effects appended. The combined coefficients are added to the FFEM data set with names that
#' appends COV to the parameter name, e.g. the column with the combined covariate efftecs for parameter PAR will be called PARCOV. 
#' All rows for an individual will have the same value for PARCOV.
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
#' 
#' vpcData <- createVPCdata(modName          = "run9",
#'                          modDevDir        = "inst/extdata/SimVal",
#'                          numNonFREMThetas = 9,
#'                          numSkipOm        = 2,
#'                          dataFile         = data,
#'                          newDataFile      = "vpcData.csv")
#'                          
#'}

createVPCdata <- function(runno=NULL,
                          numNonFREMThetas,
                          modName       = NULL,
                          numFREMThetas = length(grep("THETA",names(dfext)))-numNonFREMThetas,
                          covSuffix     = "FREMCOV",
                          parNames      = paste("Par",1:numParCov,sep=""),
                          numParCov     = NULL,
                          numSkipOm     = 0,
                          dataFile,
                          newDataFile   = paste("vpcData",runno,".csv",sep=""),
                          availCov      = NULL,
                          idvar         = "ID",
                          modDevDir     = NULL,
                          quiet         = FALSE,
                          cores         = 1,
                          dfext         = NULL,
                          ...) {
  

  fileNames <- getFileNames(runno=runno,modName=modName,modDevDir=modDevDir,...)
  modFile   <- fileNames$mod
  extFile   <- fileNames$ext
  
  covNames <- getCovNames(modFile)$covNames
  fremCovs <- getCovNames(modFile)$polyCatCovs
  orgCovs  <- getCovNames(modFile)$orgCovNames
  
  # It is much faster to send in extdf than to create it fo reach ID.
  # Only read it from file if it isn't passed via dfext
  if(is.null(dfext)) {
    dfext <- getExt(extFile)
  } else {
    dfext <- dfext
  }

  if (is.null(numParCov)) {
    numParCov <- calcNumParCov(dfext,numNonFREMThetas, numSkipOm)
  }
  
  
  ## Run this to get the omega matrix to use in the vpc
  if(is.null(availCov)) availCov    <- covNames
  
  tmp <- calcFFEM(dfext=dfext,numNonFREMThetas,covNames=covNames,parNames=parNames,availCov=availCov,quiet=quiet,numSkipOm=numSkipOm,...)
  
  ## Create a data set with all the original covariates + the frem-specific ones
  # Read the FFEM data set and rename the id column to ID (to simplify the coding below. The id column will get its original name in the new data file.)
  if(!is.data.frame(dataFile)) {
    data <- fread(dataFile,h=T,data.table=FALSE,showProgress=FALSE) %>% 
      rename("ID" = idvar)
  } else { ## The dataFile was supplied as a data frame and not a name
    data <- dataFile %>% 
      rename("ID" = idvar)
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
  myFun <- function(data,parNames,dataMap=dataMap,availCov=NULL,covSuffix) {
    ID <- data$ID
    if(is.null(availCov)) {
      availCov  <- covNames[as.logical(dataMap[dataMap$ID==ID,-1])]
    } else {
      myCov    <- covNames[as.logical(dataMap[dataMap$ID==ID,-1])]
      availCov <- myCov[myCov %in% availCov]
    }

    ffemObj <- calcFFEM(dfext=dfext,numNonFREMThetas,covNames=covNames,availCov=availCov,quiet=TRUE,parNames=parNames,numSkipOm=numSkipOm,...)
    
    retDf <- data.frame(ID=ID)
    for(i in 1:length(parNames)) {
      colName <- paste0(parNames[i],covSuffix)
      retDf[[colName]] <- as.numeric(eval(parse(text=ffemObj$Expr[i])))
      # retDf[[parNames[i]]] <- as.numeric(eval(parse(text=ffemObj$Expr[i])))
    }
    
    return(retDf)
  }
  
  dataOne <- data %>% distinct(ID,.keep_all=TRUE)
  
  covEff <- foreach(k = 1:nrow(dataOne)) %do% {
    myFun(data=dataOne[k,],parNames,dataMap=dataMap,availCov=availCov,covSuffix)
  }

  covEff <- data.frame(rbindlist(covEff))
  
  
  data2 <- left_join(data,covEff,by="ID")
  
  # Remove the frem covariates
  data3 <- data2[,!(names(data2) %in% fremCovs)]
  
  ## Give the ID column back its original name
  data3 <- data3 %>% rename(!!idvar := "ID")
  
  # Write the data to file
  if(!is.null(newDataFile)) {
    write.csv(data3,file=newDataFile,quote = FALSE,row.names = FALSE)
  } else {
    data3
  }
}
