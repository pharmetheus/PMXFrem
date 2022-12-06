#' updateFREM
#'
#' @description Appends or removes covariates in a FREM data set and FREM model.
#' @param strFREMModel File name of the FREM-model to add/remove covariates to/from.
#' @param strFREMData Name of FREM-dataset to add/remove covariates to/from (not used with strUpdateType "NoData").
#' @param strFFEMData Name of FFEM-dataset (normal dataset) that will be used to append the FREM-dataset, (not used with strUpdateType "NoData").
#' @param cstrContCovsToAdd A vector of continuous covariate names to add, default = NULL, (not used with strUpdateType "NoData").
#' @param cstrCatCovsToAdd A vector of categorical covariate names to add, default = NULL, (not used with strUpdateType "NoData").
#' @param cstrCovsToAddOrder A vector of the order of the covariate names to add, default = NULL (i.e. alphabetic order will be used), (not used with strUpdateType "NoData").
#' Note: if used, this should contain all covariates in cstrCatCovsToAdd as well as cstrContCovsToAdd.
#' @param strNewFREMData Name of the new dataset, default=paste0(strFREMData_without_extension,"new",".",extension), (not used with strUpdateType "NoData").
#' @param strUpdateType Update function to run: "DataAndModel" - Create new data and add/remove variables from the model (with updated inits).
#' "NoData" - Do not create data or add variables to model, only update the frem model in terms of new inits
#' @param quiet If set to FALSE, the function outputs verbose information on what it is doing.
#' @param strID A string with the ID identifier column in the FFEM dataset.
#' @param basenames_th A vector of strings with the names of the base variables (used for commenting thetas), should be the same length as number of nonFREMThetas in the model, if NULL, BASE1,BASE2,etc are used as names.
#' @param basenames_om A vector of strings with the names of the base variables (used for commenting omegas), should be the same length as number of numSkipOm+numParCov in the model, if NULL, BASE1,BASE2,etc are used as names.
#' @param cstrKeepCols A vector of columns to keep in the dataset (for the updated (small) dataset).
#' @param bWriteData If FALSE; add new variables to the model file but do not write new datasets, has no effect when "NoData" is used.
#' @param bWriteFIX If TRUE; FIX is written to the theta parameter estimates code for the covariates that were fixed in the model file, if FALSE; all theta parameters are assumed to be estimated.
#' @param cstrDV A vector of strings which are DV variables, important if new DVs (or new individuals) should be added, default="HAZ" (i.e. HAZ assumed fremtype=0), new DVs are added with fremtype 1,2,3...etc.
#' @param cstrRemoveCov A vector of strings for covariates that should be remove, note that FREMTYPEs for remaining covariates might/will change. The removal of covariates are done before any adding of data and/or new covariates.
#' Note that if this functionality is used to remove the last existing category of a categorical covariate, this should be done by removing the orginal name of the covariate and not the specific categorical covariate, i.e. "SITEID" instead of "SITEID_1" to ensure consistent renumbering of FREMTYPEs
#' @param covEpsNum The number of the epsilons parameter to be used for the covariates.
#' @param overrideExistingCheck If TRUE, the existing check will be overriden and covariates will be added even though they are present in $DATA of the modefile
#' @param sortFREMDataset The columns to sort the new data set on.
#'
#' @return Will write a new fremdata set to disc. WIll also write a stub model file with the new covariates and initial estimates (the name will be runX_new.txt).
#' @export
#'
#' @examples
#' \dontrun{
#' ## Remove SEX from the model and data set
#' updateFREM(
#'      strFREMModel      = system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"),
#'      strFREMData       = system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem"),
#'      strFFEMData       = system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"),
#'      cstrRemoveCov     = c("SEX"),
#'      cstrCatCovsToAdd  = NULL,
#'      cstrContCovsToAdd = NULL,
#'      strID             = "ID",
#'      strNewFREMData    = "frem_dataset_noSEX.csv",
#'      numNonFREMThetas  = 7,
#'      numSkipOm         = 2,
#'      bWriteData        = TRUE,
#'      quiet             = F,
#'      bWriteFIX         = TRUE,
#'      sortFREMDataset  = c("ID","TIME","FREMTYPE"),
#'      cstrKeepCols = c("ID","TIME","AMT","EVID","RATE","DV","FOOD","FREMTYPE"))
#'
#' ## Only update inits
#' updateFREM(strFREMModel      = system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"),
#'      numNonFREMThetas  = 7,
#'      numSkipOm         = 2,
#'      bWriteData        = FALSE,
#'      quiet             = F,
#'      strUpdateType     = "NoData")
#'
#' }
updateFREM <- function(strFREMModel,
                       strFREMData           = "",
                       strFFEMData           = "",
                       cstrContCovsToAdd     = NULL,
                       cstrCatCovsToAdd      = NULL,
                       cstrCovsToAddOrder    = NULL,
                       strNewFREMData        = NULL,
                       strUpdateType         = "DataAndModel",
                       quiet                 = TRUE,
                       strID                 = "ID",
                       basenames_th          = NULL,
                       basenames_om          = NULL,
                       numNonFREMThetas,
                       numSkipOm             = 0,
                       numParCov             = NULL,
                       cstrKeepCols          = c("ID","DV","FREMTYPE"),
                       bWriteData            = TRUE,
                       bWriteFIX             = TRUE,
                       cstrDV                = "DV",
                       cstrRemoveCov         = NULL,
                       covEpsNum             = 2,
                       overrideExistingCheck = FALSE,
                       sortFREMDataset       = c(strID,"TIME","FREMTYPE")) {

  library(tools)
  iFremTypeIncrease<-100 #The FREMTYPE INCREASE TO USE
  dDefaultCovValue<-1E-05 #The covariance value to use as initial estimate for new covariates

  printq<-function(str,quiet) {
    if (!quiet) print(str)
  }

  if (strUpdateType!="NoData") {
    if (is.null(strNewFREMData))  strNewFREMData<-paste0(file_path_sans_ext(strFREMData),"_new.",file_ext(strFREMData))
    if ((is.null(strFREMData) || strFREMData=="") || (is.null(strFFEMData) || strFFEMData==""))  stop("strFREMData and strFFEMData must be set to dataset")
  }

  if (strUpdateType=="NoData" || strUpdateType=="NewInits") {
    if (!file.exists(paste0(file_path_sans_ext(strFREMModel),".ext"))) stop(paste0("NoData and NewInits demands that ",paste0(file_path_sans_ext(strFREMModel),".ext")," exists"))
  }


  iNumTHETA=-1
  iNumOM=-1

  if (file.exists(paste0(file_path_sans_ext(strFREMModel),".ext"))) { #Use the ext file to figure out number of THETAS, OMEGAS and SIGMAS
    dfext<-subset(getExt(extFile = paste0(file_path_sans_ext(strFREMModel),".ext")),ITERATION=="-1000000000") #Read in parameter values
    dfextfix<-subset(getExt(extFile = paste0(file_path_sans_ext(strFREMModel),".ext")),ITERATION=="-1000000006") #Read in which parameteras that are fixed
    iNumTHETA<-length(names(dfext)[regexpr('THETA.*',names(dfext))==1])
    iNumOM<-length(names(dfext)[regexpr('OMEGA.*',names(dfext))==1])
    iNumOM<--1/2+sqrt(1/4+2*iNumOM) #Get the number of diagonals from the number of OM elements
    THETA<-as.numeric(dfext[,names(dfext)[regexpr('THETA.*',names(dfext))==1]])
    THETAFIX<-as.numeric(dfextfix[,names(dfextfix)[regexpr('THETA.*',names(dfextfix))==1]])
    OMEGA  <- as.numeric(dfext[,names(dfext)[regexpr('OMEGA.*',names(dfext))==1]])

    OM                             <- matrix(0, nrow=iNumOM, ncol=iNumOM) #Define an empty matrix
    OM[upper.tri(OM,diag = TRUE)]  <- OMEGA #Assign upper triangular + diag
    tOM                            <- t(OM) #Get a transposed matrix
    OM[lower.tri(OM,diag = FALSE)] <- tOM[lower.tri(tOM,diag = FALSE)] #Assign the lower triangular except diag

    if (is.null(numParCov)) {
      numParCov <- calcNumParCov(dfext,numNonFREMThetas, numSkipOm)
    }


  } else {
    #Parsing of the FREM model
    if (file.exists(strFREMModel)) {
      mod       <- scan(strFREMModel,what="character",sep="\n",quiet=TRUE)
      os <- mod[grep('THETA\\([0-9]+\\)',mod)] # Returns positions of every THETA(NUMBER)

      for (str in os) { ###Get the maximum THETA number, i.e. number of THETAs in model
        tmp<-gsub(".*THETA\\(([0-9]+)\\).*", "\\1", str)
        if (as.numeric(tmp)>iNumTHETA) iNumTHETA<-as.numeric(tmp)
      }

      #Note, only accepts ETA with space before
      os <- mod[grep('[ ]ETA\\([0-9]+\\)',mod)] # Returns positions of every ETA(NUMBER)

      for (str in os) { ###Get the maximum OMEGA number, i.e. number of ETAs in model
        tmp<-gsub(".*[ ]ETA\\(([0-9]+)\\).*", "\\1", str)
        if (as.numeric(tmp)>iNumOM) iNumOM<-as.numeric(tmp)
      }

      if (is.null(numParCov)) {
        stop("If no *.ext file exist, the number of parameters  (numParCov) needs to be specified!")
      }


    } else {
      stop(paste0("Cannot find the FREM model: ",strFREMModel))
    }
  }



  printq(paste0(iNumTHETA," THETAs found in modelfile"),quiet=quiet)
  printq(paste0(iNumOM," OMEGAs/ETAs found in modelfile"),quiet=quiet)


  dfFFEM<-NULL
  dfFREM<-NULL

  covnames<-getCovNames(modFile = strFREMModel)

  addedList<-c()
  noBaseThetas<-numNonFREMThetas
  if (strUpdateType!="NoData") {
    if (file.exists(strFFEMData)) {
      dfFFEM <- fread(strFFEMData,h=T,data.table=FALSE,check.names=TRUE,showProgress=!quiet)
    } else {
      stop("Cannot find FFEM dataset: ",strFFEMData)
    }
    if (file.exists(strFREMData)) {
      dfFREM <- fread(strFREMData,h=T,data.table=FALSE,check.names=TRUE,showProgress=!quiet)
    } else {
      stop("Cannot find FREM dataset: ",strFREMData)
    }

    #### Removal of covariates
    if (!is.null(cstrRemoveCov)) {
      cOrgCovsToRemove<-c()
      cCovNamesToRemove<-c()
      for (i in 1:length(cstrRemoveCov)) { #Go trough all covariates to remove
        for (j in 1:length(covnames$covNames)) {
          strCov<-covnames$covNames[j]
          strCovClean<-str_replace(strCov,"_.*","") #Remove underscore (if any)
          if (cstrRemoveCov[i]==strCov) { #This is a continuous covariate or a categorical specified covariate, e.g. SITEID_7
            if (cstrRemoveCov[i] %in% covnames$orgCovNames) {
              printq(paste0("Found a continuous covariate to remove ",strCov," with FREMTYPE=",j*iFremTypeIncrease),quiet = quiet)
            } else {
              printq(paste0("Found a categorical covariate to remove ",strCov," with FREMTYPE=",j*iFremTypeIncrease),quiet = quiet)
            }
            cOrgCovsToRemove<-c(cOrgCovsToRemove,strCov)
            cCovNamesToRemove<-c(cCovNamesToRemove,strCov)
          } else { #This is a categorical covariate
            if (strCovClean==cstrRemoveCov[i]) {
              iCategory<-gsub(".+_([0-9]+)", "\\1", strCov) #Get the categorical covariate category
              printq(paste0("Found a categorical covariate to remove ",strCovClean," with category ",iCategory," and FREMTYPE=",j*iFremTypeIncrease),quiet = quiet)
              cOrgCovsToRemove<-c(cOrgCovsToRemove,strCovClean)
              cCovNamesToRemove<-c(cCovNamesToRemove,strCov)
            }
          }
        }
      }

      iCovariateIndexToRemove<-which(covnames$covNames %in% cCovNamesToRemove) #Get the covariate index of the variables to remove
      #Removing rows from FREM dataset
      printq(paste0("Removing all unwanted fremtypes from FREM dataset, in total ",nrow(dfFREM[(dfFREM[["FREMTYPE"]] %in% (iCovariateIndexToRemove*iFremTypeIncrease)),])," observations"),quiet = quiet)
      dfFREM<-dfFREM[!(dfFREM[["FREMTYPE"]] %in% (iCovariateIndexToRemove*iFremTypeIncrease)),] #Only keep the fremtypes that should not be removed

      #Recode all FREMTYPES in frem dataset
      printq(paste0("Recoding all remaining FREMTYPEs in FREM dataset..."),quiet = quiet)
      remainingCovs<-covnames$covNames[which(!(covnames$covNames %in% cCovNamesToRemove))]
      iuniqueFREMTYPEs<-sort(unique(dfFREM[dfFREM[["FREMTYPE"]]>=iFremTypeIncrease,"FREMTYPE"]))

      for (i in 1:length(iuniqueFREMTYPEs)) {
        if (iuniqueFREMTYPEs[i]!=i*iFremTypeIncrease) {
          printq(paste0("Updating ",nrow(dfFREM[dfFREM[["FREMTYPE"]]==iuniqueFREMTYPEs[i],])," rows with FREMTYPE=",iuniqueFREMTYPEs[i]," (",remainingCovs[i],") to FREMTYPE=",i*iFremTypeIncrease),quiet = quiet)
          dfFREM[dfFREM[["FREMTYPE"]]==iuniqueFREMTYPEs[i],"FREMTYPE"]<-i*iFremTypeIncrease
        }
      }
      printq(paste0("Recoding FREMTYPES, done!"),quiet = quiet)
      #Update all TH and OM variables
      THETA<-THETA[-(iCovariateIndexToRemove+noBaseThetas)]
      OM<-OM[-(iCovariateIndexToRemove+numParCov+numSkipOm),-(iCovariateIndexToRemove+numParCov+numSkipOm)]
      THETAFIX<-THETAFIX[-(iCovariateIndexToRemove+noBaseThetas)]
      iNumOM<-iNumOM-length(iCovariateIndexToRemove)
      iNumTHETA<-iNumTHETA-length(iCovariateIndexToRemove)

      #Update all covariate names
      covnames$covNames<-covnames$covNames[!(covnames$covNames %in% cCovNamesToRemove)]
      covnames$orgCovNames<-covnames$orgCovNames[!(covnames$orgCovNames %in% unique(cOrgCovsToRemove))]
      covnames$polyCatCovs<-covnames$polyCatCovs[covnames$polyCatCovs %in% covnames$covNames]
    }


    printq(paste0("Variables already in FREM model (n=",length(covnames$orgCovNames),"): ",paste0(covnames$orgCovNames, collapse = " ")),quiet=quiet)
    if (!is.null(cstrContCovsToAdd)) printq(paste0("Continuous covariates that will be added to FREM (n=",length(cstrContCovsToAdd),"): ",paste0(cstrContCovsToAdd, collapse = " ")),quiet=quiet)
    if (!is.null(cstrCatCovsToAdd)) printq(paste0("Categorical covariates that will be added to FREM (n=",length(cstrCatCovsToAdd),"): ",paste0(cstrCatCovsToAdd, collapse = " ")),quiet=quiet)

    iFremType<-max(dfFREM$FREMTYPE) #Get the next fremtype to use


    #### Add all covariates together and sort them when adding
    if (is.null(cstrCovsToAddOrder)) cstrCovsToAddOrder<-sort(c(cstrContCovsToAdd,cstrCatCovsToAdd))

    ### Make all new variables (covariates) and store them in a list
    covList<-list()
    for (strCov in cstrCovsToAddOrder) { #Generate values, mean/variance for each covariate

      #Move categorical covariates with only 2 levels to cont covariates instead
      if (strCov %in% cstrCatCovsToAdd && length(unique(dfFFEM[dfFFEM[[strCov]]!=-99,strCov]))==2) {
        cstrContCovsToAdd<-c(strCov,cstrContCovsToAdd)
        cstrCatCovsToAdd<-cstrCatCovsToAdd[-which(cstrCatCovsToAdd==strCov)]
        printq(paste0("Switching: ",strCov,", from categorical to continuous since only 2 levels"),quiet=quiet)
      }

      if (strCov %in% cstrContCovsToAdd) {#Add a continuous covariate to the fremdataset
        if (!strCov %in% covnames$covNames || overrideExistingCheck==TRUE) {
          tmp<-dfFFEM[dfFFEM[[strCov]]!=-99,]
          tmp<-tmp[!duplicated(tmp[[strID]]),c(strID,strCov)]
          iFremType=iFremType+iFremTypeIncrease
          l<-list(Name=strCov,Mean=mean(tmp[[strCov]]),Var=var(tmp[[strCov]]),Fremtype=iFremType)
          l[["Data"]]<-tmp
          covList[[strCov]]<-l
          printq(paste0("Identifying new covariate: ",strCov," with fremtype ",iFremType),quiet=quiet)
          addedList<-c(addedList,strCov)
        } else {
          printq(paste0("Skipping continuous covariate: ",strCov,", already existing as fremtype"),quiet=quiet)
        }
      } else { ###Add a categorical covariate to the fremdataset
        covValues<-sort(unique(dfFFEM[dfFFEM[[strCov]]!=-99,strCov]))
        for (j in 2:length(covValues)) {
          strCov2<-paste0(strCov,"_",covValues[j])

          if (!strCov2 %in% covnames$covNames || overrideExistingCheck==TRUE) {

            dfFFEM[[strCov2]]<-dfFFEM[[strCov]]
            dfFFEM[[strCov2]][dfFFEM[[strCov]]!=-99 & dfFFEM[[strCov]]==covValues[j]]<-1
            dfFFEM[[strCov2]][dfFFEM[[strCov]]!=-99 & dfFFEM[[strCov]]!=covValues[j]]<-0

            tmp<-dfFFEM[dfFFEM[[strCov2]]!=-99,] #Get all which are not missing
            tmp<-tmp[!duplicated(tmp[[strID]]),c(strID,strCov2)]
            iFremType=iFremType+iFremTypeIncrease
            l<-list(Name=strCov2,Mean=mean(tmp[[strCov2]]),Var=var(tmp[[strCov2]]),Fremtype=iFremType)
            l[["Data"]]<-tmp
            covList[[strCov2]]<-l
            printq(paste0("Identifying new covariate: ",strCov2," with fremtype ",iFremType),quiet=quiet)
            addedList<-c(addedList,strCov2)
          } else {
            printq(paste0("Skipping category: ",strCov2,", already existing as fremtype"),quiet=quiet)
          }
        }
      }
    }

    ### Add new individuals to the FREM dataset and update the already existing FREMTYPES, i.e. the individuals to add are the individuals in FFEM data which does not exist in FREM data
    dataToAdd<-dfFFEM[!(dfFFEM[[strID]] %in% dfFREM[[strID]]),]
    printq(paste0("Found ",nrow(dataToAdd[!duplicated(dataToAdd[[strID]]),])," individuals that should be added to the FREM dataset"),quiet = quiet)


    ### Add existing (and new) DVs
    iFremtypeDV<-unique(dfFREM["FREMTYPE"][dfFREM["FREMTYPE"]<iFremTypeIncrease])
    iNewFremtypeDV<-NULL
    if (is.null(cstrDV)) {
      warning("No DVs recognized; no DVs added for new individuals")
      iFremtypeDV<-NULL
    } else {
      if (length(iFremtypeDV)>length(cstrDV)) {
        warning(paste0("Found more DV Fremtypes than DV variables, only adding DVs: ",paste0(cstrDV,collapse=", "))," to new individuals")
        iFremTypeDV=0:(length(cstrDV)-1)
      }
      if (length(iFremtypeDV)<length(cstrDV)) {
        printq(paste0("Found new DVs to add: ",paste0(cstrDV[(length(iFremtypeDV)+1):length(cstrDV)],collapse=", ")),quiet=quiet)
        iNewFremtypeDV<-length(iFremtypeDV):(length(cstrDV)-1)
      }
    }


    dfAddList<-list()
    if (length(iFremtypeDV)>0) {
      ### Add existing DVs for new individuals
      for (i in 1:length(iFremtypeDV)) {
        strDV<-cstrDV[i]
        dfDVData<-dataToAdd[dataToAdd[[strDV]]!=-99,unique(c(names(dataToAdd)[names(dataToAdd) %in% names(dfFREM)],strDV)),] #Get the dataset with non-missing DV values
        if (nrow(dfDVData)==0) { #If no non-missing DVs
          printq(paste0("No observations for ",strDV," (fremtype=",iFremtypeDV[i],"); not adding any observations!"),quiet = quiet)
        } else {
          dfDVData$DV<-dfDVData[[strDV]]
          if (!(strDV %in% names(dfFREM))) dfDVData[[strDV]]<-NULL
          dfDVData$FREMTYPE<-iFremtypeDV[i]
          if (length(names(dfDVData))==length(names(dfFREM)) && all(sort(names(dfDVData))==sort(names(dfFREM)))) { #If match in names between FREM and FFEM dataset
            dfDVData<-dfDVData[,names(dfFREM)]
            dfAddList[[i]]<-dfDVData
            printq(paste0("Adding ",nrow(dfDVData)," observations (",strDV,") from ",nrow(dfDVData[!duplicated(dfDVData[[strID]]),]), " individuals as fremtype ",iFremtypeDV[i]),quiet = quiet)
          } else {
            strMissnames<-names(dfFREM)[!(names(dfFREM) %in% names(dfDVData))]
            stop("Variables not found in FFEM dataset: ",paste0(strMissnames,collapse = ", "), "; please add column(s) and try again")
          }
        }
      }

      #Add old DVs to FREM dataset
      dfFREM<-rbind(dfFREM,as.data.frame(data.table::rbindlist(dfAddList)))
    }

    dfAddList<-list()
    if (length(iNewFremtypeDV)>0) {
      ### Add new DVs for all individuals
      for (i in 1:length(iNewFremtypeDV)) {
        strDV<-cstrDV[length(iFremtypeDV)+i]
        dfDVData<-dfFFEM[dfFFEM[[strDV]]!=-99,unique(c(names(dfFFEM)[names(dfFFEM) %in% names(dfFREM)],strDV)),] #Get the dataset with non-missing new DV values
        if (nrow(dfDVData)==0) { #If no non-missing DVs
          printq(paste0("No observations for ",strDV," (fremtype=",iNewFremtypeDV[i],"); not adding any observations!"),quiet = quiet)
          warning(paste0("Note that it might be inconsistencies in DV fremtypes since fremtype ",iNewFremtypeDV[i]," is not present!"))
        } else {
          dfDVData$DV<-dfDVData[[strDV]]
          if (!(strDV %in% names(dfFREM))) dfDVData[[strDV]]<-NULL
          dfDVData$FREMTYPE<-iNewFremtypeDV[i]
          if (length(names(dfDVData))==length(names(dfFREM)) && all(sort(names(dfDVData))==sort(names(dfFREM)))) { #If match in names between FREM and FFEM dataset
            dfDVData<-dfDVData[,names(dfFREM)]
            dfAddList[[i]]<-dfDVData
            printq(paste0("Adding ",nrow(dfDVData)," observations (",strDV,") from ",nrow(dfDVData[!duplicated(dfDVData[[strID]]),]), " individuals as fremtype ",iNewFremtypeDV[i]),quiet = quiet)
          } else {
            strMissnames<-names(dfFREM)[!(names(dfFREM) %in% names(dfDVData))]
            stop("Variables not found in FFEM dataset: ",paste0(strMissnames,collapse = ", "), "; please add column(s) and try again")
          }
        }
      }

      #Add new DVs to FREM dataset
      dfFREM<-rbind(dfFREM,as.data.frame(data.table::rbindlist(dfAddList)))
    }


    ### Add old FREM variables for new individuals
    dfAddList<-list()
    if (nrow(dataToAdd)>0) {
    for (i in 1:length(covnames$covNames)) { #For all existing covariates (fremtypes)
      strCov<-covnames$covNames[i]
      iFremtype=iFremTypeIncrease*i

      strCovClean<-str_replace(strCov,"_.*","") #Remove underscore (if any)

      #Get the covariate data
      dfData<- dataToAdd[dataToAdd[[strCovClean]]!=-99,unique(c(names(dataToAdd)[names(dataToAdd) %in% names(dfFREM)],strCovClean)),] #Get the dataset with non-missing new DV values

      if (nrow(dfData)==0) { #If no non-missing covariates for this covariate
        printq(paste0("No observed covariate values for ",strCov," (fremtype=",iFremtype,"); not adding any covariate values!"),quiet = quiet)
      }else {

        dfData<-dfData[!duplicated(dfData[[strID]]),] #Get only one value per covariate, i.e. time-invariant covariates
        dfData$FREMTYPE<-iFremtype

        if (strCov==strCovClean) { #This is a continuous covariate
          dfData$DV<-dfData[[strCovClean]]
          dfData<-dfData[,names(dfFREM)]
          dfAddList[[i]]<-dfData
          printq(paste0("Adding ",nrow(dfData)," continuous covariate values (",strCovClean,") from ",nrow(dfData[!duplicated(dfData[[strID]]),]), " individuals as fremtype ",iFremtype),quiet = quiet)
        } else { #This is a categorical covariate
          iCategory<-gsub(".+_([0-9]+)", "\\1", strCov) #Get the categorical covariate category
          dfData$DV<-ifelse(dfData[[strCovClean]]==iCategory,1,0)
          dfData<-dfData[,names(dfFREM)]
          dfAddList[[i]]<-dfData
          printq(paste0("Adding ",nrow(dfData)," categorical covariate values (",strCov,") from ",nrow(dfData[!duplicated(dfData[[strID]]),]), " individuals as fremtype ",iFremtype),quiet = quiet)
        }
      }
    }

    #Add old Covariates (new individuals) to FREM dataset
    dfFREM<-rbind(dfFREM,as.data.frame(data.table::rbindlist(dfAddList)))

    }

    dfAddList<-list()
    ### Add the new FREM variables to the FREM dataset, note the variables are not added as columns
    dfFREMOne<-dfFREM[!duplicated(dfFREM[[strID]]),] #Get one row per subject from FREM dataset

    if (!is.null(addedList)) {
      for (i in 1:length(addedList)) { #Add all variables
        strcov<-addedList[i]
        l<-covList[[strcov]] #Get the covariate info
        dftmp<-dfFREMOne  #Get the one row per ID for this covariates to add (i.e. non-missing covariate IDs)
        dfnew<-l[["Data"]]
        dfnew$NEWVARIABLE<-dfnew[[l[["Name"]]]]
        dfnew[[l[["Name"]]]]<-NULL

        dftmp<-merge(dftmp,dfnew,by=strID,all.x=FALSE,all.y=FALSE) #Only match where we find a variable

        dftmp$DV<-dftmp$NEWVARIABLE #Set DV to correct value
        dftmp$FREMTYPE<-l[["Fremtype"]]
        dftmp$NEWVARIABLE<-NULL
        dfAddList[[i]]<-dftmp

        printq(paste0("Adding ",nrow(dftmp)," covariate values (",l[["Name"]],") from ",nrow(dftmp[!duplicated(dftmp[[strID]]),]), " individuals as fremtype ",l[["Fremtype"]]),quiet = quiet)
      }
    }

    #Add new FREM variables to FREM dataset
    dfFREM<-rbind(dfFREM,as.data.frame(data.table::rbindlist(dfAddList)))


    #Writing the FREM dataset to disc!!
    if(!is.null(sortFREMDataset)) {
    #  dfFREM <- dfFREM[] %>% arrange(ID,AGE,FREMTYPE) %>% select(one_of(cstrKeepCols))
      if (is.null(cstrKeepCols)) { #Keep all columns
        dfFREM %>% arrange(!!!syms(sortFREMDataset))
      } else{
        dfFREM <- dfFREM %>% arrange(!!!syms(sortFREMDataset)) %>% select(one_of(cstrKeepCols))
      }
    } else {
      if (is.null(cstrKeepCols)) {
        dfFREM <- dfFREM %>% arrange(ID,FREMTYPE)
      } else {
        dfFREM <- dfFREM %>% arrange(ID,FREMTYPE) %>% select(one_of(cstrKeepCols))
      }
    }

    if (bWriteData) {
      write.csv(dfFREM,file=strNewFREMData,row.names = FALSE,quote = FALSE)
      if (!("FREMTYPE" %in% names(dfFREM))) warning("No FREMTYPE available in dataset, add in cstrKeepCols and rerun updateFREM")
    }

  }


  ### Print Model code

  strNewCovNames<-c(covnames$covNames,addedList)

  #Make a copy of the FREM model
  con=file(strFREMModel,open="r")
  line=readLines(con)
  close(con)

  strinput=""

  ## Print the MU-code and the COV-code
  for(i in (noBaseThetas+1):(length(strNewCovNames)+noBaseThetas)) {
    mu_count=i-noBaseThetas+numSkipOm+numParCov
    strinput<-c(strinput,paste0("      MU_",mu_count," = ","THETA(",i,")"))
    strinput<-c(strinput,paste0("      COV",mu_count," = MU_",mu_count," + ETA(",mu_count,")"))
  }

  # Replace MU code and the COV-code
  tmpl<-grep(pattern = "COV\\d+ = MU\\_\\d+",x=line)
  if (is.null(tmpl))  warning("can't replace MU and COV lines")
  if (!is.null(tmpl)) {
    line<-c(line[1:(min(tmpl-2))],strinput,line[(max(tmpl)+1):length(line)])
  }

  fremTypes <- seq(from=iFremTypeIncrease, by=iFremTypeIncrease,length.out = length(strNewCovNames))

  ## Print the ;;;FREM CODE BEGIN COMPACT
  strinput=""
  strinput<-c(strinput,paste0(";;;FREM CODE BEGIN COMPACT"))

  # Print the FREMTYPE code
  for (i in 1:length(strNewCovNames)) {
    strinput<-c(strinput,paste0("      IF(FREMTYPE.EQ.",fremTypes[i],") THEN"))
    strinput<-c(strinput,paste0(";       ",strNewCovNames[i]))
    strinput<-c(strinput,paste0("        Y = COV",i+numSkipOm+numParCov," + EPS(",covEpsNum,")"))
    strinput<-c(strinput,paste0("        IPRED = COV",i+numSkipOm+numParCov))
    strinput<-c(strinput,paste0("      ENDIF"))
  }
  ## Print the ;;;FREM CODE END COMPACT
  strinput<-c(strinput,paste0(";;;FREM CODE END COMPACT"))
  #Replace FREM code
  line<-findrecord(line,record = ";;;FREM CODE BEGIN COMPACT",replace = strinput,quite = T)

  #### Print parameters values, $THETA, $OMEGA
  if (is.null(basenames_th)) basenames_th<-paste0("BASE",1:numNonFREMThetas)
  if (is.null(basenames_om)) basenames_om<-paste0("BASE",1:(numSkipOm+numParCov))

  if (!is.null(addedList) & length(addedList)>0) { #Expand OM matrix
    if (exists("OM")==FALSE || is.null(OM)) stop("OM missing, make sure ext file is provided when adding covariates")
    OMNEW<-matrix(dDefaultCovValue,ncol(OM)+length(addedList),nrow(OM)+length(addedList))
    OMNEW[1:ncol(OM),1:nrow(OM)]<-OM
    OM<-OMNEW
  }

  theta_comment<-paste0(" ; ",1:iNumTHETA," TV_",c(basenames_th,covnames$covNames))
  om_comment<-paste0(" ; ",1:iNumOM," BSV_",c(basenames_om,covnames$covNames))

  ####Add new variables to THETA vector and OM matrix
  if (!is.null(addedList)) {
    for (i in 1:length(addedList)) { #Add all variables to THETA vector and OM matrix
      strcov<-addedList[i]
      l<-covList[[strcov]] #Get the covariate info
      THETA<-c(THETA,l[["Mean"]]) #Add mean value as $THETA
      THETAFIX<-c(THETAFIX,0) #Add that new THETA is not fix
      theta_comment<-c(theta_comment,paste0(" ; ",iNumTHETA+1," TV_",l[["Name"]]))
      iNumTHETA=iNumTHETA+1 #Update the number of thetas
      OM[iNumOM+1,iNumOM+1]<-l[["Var"]] #Add variance of OM as initial estimate
      om_comment<-c(om_comment,paste0(" ; ",iNumOM+1," BSV_",l[["Name"]]))
      iNumOM<-iNumOM+1 #Update the number of omegas
    }
  }

  strinput=""
  for (i in 1:iNumTHETA) {
    strFIX=""
    if (THETAFIX[i]==1 && bWriteFIX) strFIX=" FIX"
    strinput<-c(strinput,paste0("$THETA ",THETA[i],strFIX," ",theta_comment[i]))
  }

  #Add new $THETA
  line<-findrecord(line,record = "\\$THETA",replace = strinput,quite = T)

  #Build OM Matrix
  newommatrix<-buildmatrix(as.matrix(OM))
  #Add OM comments
  j<-length(newommatrix)
  for (i in length(om_comment):(1+numSkipOm)){
    newommatrix[j]<-paste0(newommatrix[j]," ",om_comment[i])
    j<-j-1
  }
  #Replace $OMEGA
  line <- findrecord(line,record="\\$OMEGA",replace=newommatrix,quite=T)

  ## Replace $DATA
  line <- findrecord(line,record="\\$DATA",replace=paste0("$DATA ", strNewFREMData," IGNORE=@"),quite=T)

  ## Replace $INPUT
  line <- findrecord(line,record="\\$INPUT",replace=paste0("$INPUT ", paste0(names(dfFREM),collapse = " ")),quite=T)
  
  ## Write new model file
  strNewModelFileName<-paste0(file_path_sans_ext(strFREMModel),"_new.mod")
  con=file(strNewModelFileName,open="w")
  writeLines(line,con)
  close(con)
}
