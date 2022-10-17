#' createFREMData
#'
#' @description Creates a FREM dataset from a standard NONMEM dataset. Only the basic DV are added (FREMTYPE=0)
#' @param strFREMData Name of FREM-dataset to create. Default=NULL => no dataset is written to disc
#' @param strFMEMData Name of FFEM-dataset (normal dataset) that will be used to create the FREM-dataset.
#' @param quiet If set to FALSE, the function outputs verbose information on what it is doing.
#' @param strID A string with the ID identifier column in the FMEM dataset.
#' @param cstrKeepCols A vector of columns to keep in the dataset (for the updated new dataset).
#' @param cstrDV A vector of strings with DV variables that should be added, default="HAZ" (i.e. HAZ assumed fremtype=0), additional DVs are added with fremtype 1,2,3...etc.
#' @param cstrContCovs A vector of strings with continuous covariates to create. Default=NULL, i.e. no continous covariates will be added 
#' @param cstrCatCovs  A vector of strings with categorical covariates to create. Default=NULL, i.e. no categorical covariates will be added 
#' @param bRecodeDichotomous Set to true if dichotomous covariates should be re-scaled to 1,0. E.g. if set to TRUE and SEX is coded as 1 or 2, a new variable SEXN_2 will be created with the values of 0 or 1. Only applicable if cstrCatCov is not zero and at least one of the cat covariates is dichotomous
#' @param cSortCols The column names to sort by (must  be present in cstrKeepCols)
#' @param cSortDirection The sort column order, ascending = 1, descending = -1, must be the same length as cSortCols
#' @param cFremtypes A vector of FREMTYPE values that each DV and covariate should use. The order should be: DV variables, continuous covariates, categorical covariates. Default=NULL, i.e.FREMTYPE of DV1=0, DV2=1,..,CONT1=100, ..CONTN=N*100, CAT1=(N+1+NumLevels1)*100, CATM=(N+M*NumLevelsM)*100
#'
#' @return Will write a new fremdata set to disc if strFREMData is not NULL and return the FREM dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' }
createFREMData <- function(strFREMData=NULL,strFFEMData="",quiet=TRUE,strID="ID",
                       cstrKeepCols=c("ID","ID2","AGE","DV","BIRTHWT","BIRTHLEN","SEXN","FREMTYPE"),cstrDV="DV",
                       cstrContCovs=NULL, cstrCatCovs=NULL, bRecodeDichotomous=TRUE,
                       cSortCols=c("ID","AGE","FREMTYPE"), cSortDirection=c(1,1,-1),
                       cFremtypes=NULL) {

  CovFremType<-100 #The Fremtype value for the next covariate
  FremtypeIncrement<-100 #The FREMTYP incrementer for covariates
  iNewFremtypeDV<-0 #The start FREMTYPE for new DV
  
  if (is.null(cFremtypes)) { #If we should create fremtypes, otherwise the fremtype are given
    cFremtypes<-c(iNewFremtypeDV:(length(cstrDV)-1)) #Add DV fremtypes
    if (!is.null(cstrContCovs)) cFremtypes<-c(cFremtypes,seq(CovFremType,CovFremType+FremtypeIncrement*(length(cstrContCovs)-1),by=FremtypeIncrement)) #Add continuous cov FREMTYPES
    maxval<-max(cFremtypes[cFremtypes>=CovFremType]+FremtypeIncrement,CovFremType)
    if (!is.null(cstrCatCovs)) cFremtypes<-c(cFremtypes,seq(maxval,maxval+(length(cstrCatCovs)-1)*FremtypeIncrement,by=FremtypeIncrement)) #Add continuous cov FREMTYPES
  }  
  
  cFremtypes<-unique(cFremtypes)
    
  
  printq<-function(str,quiet) {
   if (!quiet) print(str)
  }

  if (length(cFremtypes)!=length(cstrDV)+length(cstrContCovs)+length(cstrCatCovs)) {
    stop("The number of fremtypes are not the same as the number of frem variables, i.e. ",length(cFremtypes)," != ",length(cstrDV)+length(cstrContCovs)+length(cstrCatCovs))
  }
  
  
  dfFFEM<-NULL
  dfFREM<-NULL

  if (file.exists(strFFEMData)) {
    dfFFEM <- fread(strFFEMData,h=T,data.table=FALSE,check.names=TRUE,showProgress=!quiet)
  } else {
    stop("Cannot find FFEM dataset: ",strFFEMData)
  }
  
  printq(paste0("Read the FFEM dataset, consisting of ",ncol(dfFFEM)," columns and ",nrow(dfFFEM)," rows"),quiet = quiet)
  

  dfAddList<-list()
      ### Add new DVs for all individuals
      for (i in 1:length(cstrDV)) {
        strDV<-cstrDV[i]
        namestokeep<-unique(c(cstrKeepCols,strDV))
        dfDVData<-dfFFEM[dfFFEM[[strDV]]!=-99,namestokeep[namestokeep %in% names(dfFFEM)]] #Get the dataset with non-missing new DV values
        if (nrow(dfDVData)==0) { #If no non-missing DVs
          printq(paste0("No observations for ",strDV," (fremtype=",cFremtypes[i],"); not adding any observations!"),quiet = quiet)
          warning(paste0("Note that it might be inconsistencies in DV fremtypes since fremtype ",cFremtypes[i]," is not present!"))
        } else {
          dfDVData$DV<-dfDVData[[strDV]]
          dfDVData$FREMTYPE<-cFremtypes[i]
          dfDVData<-dfDVData[,unique(cstrKeepCols)] #Only keep wanted columns
          dfAddList[[i]]<-dfDVData
          printq(paste0("Adding ",nrow(dfDVData)," observations (",strDV,") from ",nrow(dfDVData[!duplicated(dfDVData[[strID]]),]), " individuals as fremtype ",cFremtypes[i]),quiet = quiet)
          }
        }
      
  
    #Add continuous covariates  
    if (!is.null(cstrContCovs)) {
      for (i in 1:length(cstrContCovs)) {
        if (!cstrContCovs[i] %in% names(dfFFEM)) { 
        printq(paste0("Can't add covariate ",cstrContCovs[i],", not found in FFEM dataset. Skipping this covariate."),quiet = quiet)
        } else {
        dfDVData<-dfFFEM[dfFFEM[[cstrContCovs[i]]]!=-99,] #Get the dataset with non-missing covariate values
        dfDVData<-dfDVData[!duplicated(dfDVData[[strID]]),] #Assume, time independent, i.e. one observation per individual
        if (nrow(dfDVData)>0) { #If we have any non-missing cov values
          dfDVData$DV<-dfDVData[[cstrContCovs[i]]]
          dfDVData$FREMTYPE<-cFremtypes[i+length(cstrDV)]
          dfDVData<-dfDVData[,unique(cstrKeepCols)] #Only keep wanted columns
          dfAddList[[length(dfAddList)+1]]<-dfDVData
          printq(paste0("Adding ",nrow(dfDVData)," covariate values (",cstrContCovs[i],") from ",nrow(dfDVData), " individuals as fremtype ",cFremtypes[i+length(cstrDV)]),quiet = quiet)
        } else {
          printq(paste0("No non-missing covariate values for ",cstrContCovs[i],". Skipping this covariate."),quiet = quiet)
        }
      }
     }
    }
  
  #Add categorical covariates  
  if (!is.null(cstrCatCovs)) {
    k<-1 #Counter for FREMTYPES
    for (i in 1:length(cstrCatCovs)) {
    
      strCov<-cstrCatCovs[i]
      if (!strCov %in% names(dfFFEM)) { 
        printq(paste0("Can't add covariate ",strCov,", not found in FFEM dataset. Skipping this covariate."),quiet = quiet)
      } else {
        dfDVData<-dfFFEM[dfFFEM[[strCov]]!=-99,] #Get the dataset with non-missing covariate values
        dfDVData<-dfDVData[!duplicated(dfDVData[[strID]]),] #Assume, time independent, i.e. one observation per individual
        if (nrow(dfDVData)>0) { #If we have any non-missing cov values
          uniqval<-sort(unique(dfDVData[[strCov]])) #Get the unique values of the categorical covariate, sort from smaller to bigger
          if (length(uniqval)>2 || (length(uniqval)==2 && bRecodeDichotomous)) #Dichotomous covariate that should be recoded or higher order polycotomous cov
          {
            for (j in 2:length(uniqval)) {
              strCov2<-paste0(strCov,"_",uniqval[j])
              dfDVData[[strCov2]]<-dfDVData[[strCov]]
              dfDVData[[strCov2]][dfDVData[[strCov]]==uniqval[j]]<-1
              dfDVData[[strCov2]][dfDVData[[strCov]]!=uniqval[j]]<-0
              dfDVData$DV<-dfDVData[[strCov2]]
              dfDVData$FREMTYPE<-cFremtypes[k+length(cstrDV)+length(cstrContCovs)]
              dfDVData<-dfDVData[,unique(cstrKeepCols)] #Only keep wanted columns
              dfAddList[[length(dfAddList)+1]]<-dfDVData
              printq(paste0("Adding ",nrow(dfDVData)," covariate values (",strCov2,") from ",nrow(dfDVData), " individuals as fremtype ",cFremtypes[k+length(cstrDV)+length(cstrContCovs)]),quiet = quiet)
              k<-k+1
            }
          } else { #Do not change the name of the covariate
            dfDVData$DV<-dfDVData[[strCov]]
            dfDVData$FREMTYPE<-cFremtypes[k+length(cstrDV)+length(cstrContCovs)]
            dfDVData<-dfDVData[,unique(cstrKeepCols)] #Only keep wanted columns
            dfAddList[[length(dfAddList)+1]]<-dfDVData
            printq(paste0("Adding ",nrow(dfDVData)," covariate values (",strCov,") from ",nrow(dfDVData), " individuals as fremtype ",cFremtypes[k+length(cstrDV)+length(cstrContCovs)]),quiet = quiet)
            k<-k+1
          }
         } else {
            printq(paste0("No non-missing covariate values for ",strCov,". Skipping this covariate."),quiet = quiet)
         }
       }
    }
  }
  
  
  #Add new DVs and covariates to FREM dataset
  dfFREM<-rbind(dfFREM,as.data.frame(data.table::rbindlist(dfAddList)))
  
  if (!is.null(cSortCols)) { #Sort everything according to cSortCols
    cstrSortTxt<-NULL
    for (i in 1:length(cSortCols)) {
      strPrefix<-""
      if (cSortDirection[i]==-1) strPrefix<-"-"
      cstrSortTxt<-c(cstrSortTxt,paste0(strPrefix,"dfFREM$",cSortCols[i]))
    }
    dfFREM<-eval(parse(text=paste0("dfFREM[order(",paste0(cstrSortTxt,collapse = ","),"),]")))
  }
  
  dfFREM<-dfFREM[,cstrKeepCols] #Only keep the specified columns
  if (!is.null(strFREMData)) {
    write.csv(dfFREM,file=strFREMData,row.names = FALSE,quote = FALSE)
  }
  
  return(dfFREM)
}
