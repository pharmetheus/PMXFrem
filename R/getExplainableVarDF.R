#' getExplainableVarDF()
#'
#' @description Get a data frame with explainable variability information based on a dataset of subjects with covariates
#'
#' 
#' @param type Which type of explained var we should use, type=1 (default), i.e. based on data and calculated etas, type=2 is that the total variability is calculated using ETA samples instead of EBEs, hence etas argument is not needed but numETASamples is needed instead. 
#' @param data the dataset to based the explained varaibility on, used with type=1
#' @param dfCovs A data frame with covariates to based the variability plots on
#' @param strID the subject identifier in the dfCovs dataset, default='ID'
#' @param runno the FREM run number to based the plot on
#' @param modDevDir the FREM run model directory
#' @param cstrCovariates A string vector with names of the covariate that should be investigated, if NULL COV1, COV2 etc. will be assigned
#' @param functionList A list of functions with input (basethetas, covthetas,dfrow and ...) for in which the explained variability will be calculated. If the function returns a vector of values, each value will be used but functionListName must contain the names with a length of all return for all functions in the functionList
#' @param functionListName A vector of strings (names) of the parameters for each function in the functionList
#' @param noBaseThetas Number of structural thetas in FREM model
#' @param noCovThetas Number of covariate thetas in FREM model
#' @param noSigmas Number of sigmas in FREM model
#' @param noParCov Number of parameters for which covariate relations are sought (often the same as noBaseThetas).
#' @param parNames Names of the parameters
#' @param covNames Names of the covariates
#' @param dfext a data frame with the final estimates in a ext-file format
#' @param availCov Names of the covariates to use in the calculation of the FFEM model
#' @param etas the etas used to calculate the explained varaibility, used with type==1 and should be the same size as number of individuals in data
#' @param quiet If output should be allowed during the function call, default= FALSE,
#' @param ncores the number of cores to use for the calculations, default = 1 which means no parallellization
#' @param cstrPackages a character vector with the packages needed to run calculations in parallel, default = NULL
#' @param cstrExports a character vector with variables needed to run the calculations in parallel, default = NULL 
#' @param numETASamples (default = 100) the number of samples used ot integrate over individual parameters when calculating the total variance of the functionList, only used using type==2 
#' @param ... additional variables to be forwarded to the the functionList functions

#'
#' @return a data frame with summary statistics for each parameters and covariate combinations:
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' dfForest <- getForestDF(dfCovs)
#' }
getExplainableVarDF <- function(type=1,data,dfCovs,strID="ID",runno,modDevDir,cstrCovariates=NULL,functionList=list(function(basethetas,covthetas,dfrow,etas,...){return(basethetas[1]*exp(covthetas[1]+etas[1]))}),functionListName="PAR1",noBaseThetas, noCovThetas, noSigmas, noParCov = noBaseThetas,
                                parNames = paste("Par", 1:noParCov, sep = ""), covNames = paste("Cov",1:noCovThetas, sep = ""),dfext=NULL,
                                availCov = covNames, etas=NULL,quiet = FALSE,ncores=1,cstrPackages=NULL,cstrExports=NULL,numETASamples=100,...) {
  
  thetas=as.numeric(dfext[2:(noBaseThetas+1)])
  if (is.null(cstrCovariates)) {
    cstrCovariates<-paste0("COV",1:nrow(dfCovs))
  }

  if (type==1 || type==2) { #Assuming explained variability based on data + ETA values (or sampled ETA values)
    
    if (type==2) {#Get the ETA samples from N(0,1) and then rescale to correct variance
      ETAsamples<-matrix(rnorm(noBaseThetas*numETASamples),nrow=noBaseThetas,ncol=numETASamples) 
    }
    #Get the coefficients for each individual based on each individuals all available covariates
    # dfdata<-createVPCdata(runno = runno,noBaseThetas = noBaseThetas,noSigmas = noSigmas,modDevDir = modDevDir,quiet = quiet,
    #             dataFile = data,newDataFile = NULL,cores = ncores,availCov=availCov)
    # 
    ## Add the FREM covariates to the data file
    # data <- addFremCovariates(dfFFEM = data,modFile)
    fremCovs <- getCovNames(paste0(modDevDir,"run",runno,".mod"))$polyCatCovs
    orgCovs  <- getCovNames(modFile)$orgCovNames
    
    for(cov in fremCovs) {
      myCov <- str_replace(cov,"_[0-9]*","")
      myCovNum <- str_replace(cov,paste0(myCov,"_"),"")
      data[[cov]] <- ifelse(data[[myCov]]==myCovNum,1,0)
    }
    
    dataI <- data %>% distinct(ID,.keep_all=TRUE) #Get one row per subject
    dataI <- dataI[,c("ID",orgCovs,covNames)] #Only keep covariates and ID
    ## Go through the individuals to make sure that missing values for polycats are coded properly
    registerDoParallel(cores=ncores)
    mapFun <- function(data,orgCovs)  {
      for(cov in orgCovs) {
        if(data[1,cov]==-99 & length(grepl(cov,names(data))) > 1) {
          data[1,grepl(cov,names(data))] <- -99
        }
      }
      return(data)
    }
    
    dataI <- foreach(k = 1:nrow(dataI)) %dopar% {
      mapFun(data=dataI[k,],orgCovs=orgCovs)
    }
    
    dataI <- data.frame(rbindlist(dataI))
    
    #### Go through all dfCovs combinations to calculate the variability for each of them
    
    dfrest<-data.frame()
    
    for (i in 1:nrow(dfCovs)) {
      currentNames<-names(dfCovs[i,])[as.numeric(dfCovs[i,])!=-99]
      if (any(!currentNames %in% covNames)) {
        print(paste0("Can't find some of the covariates: ",currentNames," in the model, quiting."))
        return(NULL)
      }
      
      strCovsRow<-names(dfCovs[i,])[as.numeric(dfCovs[i,])!=-99] #Get the covariate that we should condition on

      dftmp1<-foreach (k = 1:nrow(dataI),.packages = cstrPackages,.export = cstrExports,.verbose = !quiet,.combine=bind_rows) %dopar% { #For all subjects in data
    #  dftmp1<-foreach (k = 1:nrow(dataI),.packages = cstrPackages,.export = cstrExports,.verbose = !quiet,.combine=bind_rows) %do% { #For all subjects in data
        
        dftmp<-data.frame()
        datatmp <- dataI[k,covNames] #Get only covnames
        avcov<-names(datatmp)[which(datatmp!=-99)] #Get the non-missing covariates only
        data47_jxrtp <- datatmp
        coveffects <- rep(0,length(parNames))
        #Calculate the FFEM based on some know covariates based on the row in dfCovs which are non-missing
        ffemObj<-FREMfunctions::calcFFEM(noBaseThetas=noBaseThetas,noCovThetas = noCovThetas,noSigmas = noSigmas,dfext=dfext,covNames = covNames,
                                         availCov = avcov[avcov %in% strCovsRow],quiet = quiet)
        
        for(j in 1:length(parNames)) {
          ffem_expr<-stringr::str_replace_all(ffemObj$Expr[j],pattern = "data\\$",replacement = "data47_jxrtp$")
          if (length(names(dfCovs[i,])[as.numeric(dfCovs[i,])!=-99])!=0) coveffects[j] <- as.numeric(eval(parse(text=ffem_expr)))
          
        }
        if (i==1) {#Calculate a FFEM for each individual to get the total variability as well
          
          ffemObjAll<-FREMfunctions::calcFFEM(noBaseThetas=noBaseThetas,noCovThetas = noCovThetas,noSigmas = noSigmas,dfext=dfext,covNames = covNames,
                                              availCov = avcov,quiet = quiet)
          coveffectsAll <- rep(0,length(parNames))
          if (type==2) {
            Chol = chol(ffemObjAll$Vars) #Get the covariance matrix and then Cholesky decompose
            etasamples<-t(ETAsamples) %*% Chol #Transform the ETA samples to N(0,COV) matrix
          }
          for(j in 1:length(parNames)) {
            ffem_expr_all<-stringr::str_replace_all(ffemObjAll$Expr[j],pattern = "data\\$",replacement = "data47_jxrtp$")
            if (length(avcov)!=0) {
              coveffectsAll[j] <- as.numeric(eval(parse(text=ffem_expr_all)))
            }
          }
          ##Call each parameters in the functionList to calculate, this is for covariate +
          n=1
          for (j in 1:length(functionList)) {
            if (type==2) {
              val<-0
              for (m in 1:numETASamples) { #For all ETA samples
                val<-val+functionList[[j]](basethetas=thetas,covthetas=coveffectsAll,dfrow=dataI[k,],etas=etasamples[m,],...)
              }
              val<-val/numETASamples #Take expectation over all samples
            } else {
              val<-functionList[[j]](basethetas=thetas,covthetas=coveffectsAll,dfrow=dataI[k,],etas=as.numeric(etas[k,]),...)
            }
            valeta0<-functionList[[j]](basethetas=thetas,covthetas=coveffectsAll,dfrow=dataI[k,],etas=rep(0,length(thetas)),...)
            listcount<-length(val) 
            
            for (l in 1:listcount) {
              dftmp<-dplyr::bind_rows(dftmp,data.frame(ITER=k,COVS=0,NAME=as.character(functionListName[n]),VALUE=val[[l]]))
              dftmp<-dplyr::bind_rows(dftmp,data.frame(ITER=k,COVS=-1,NAME=as.character(functionListName[n]),VALUE=valeta0[[l]]))
              n<-n+1
            }
          }
        }
        
        ##Call each parameters in the functionList to calculate
        n=1
        for (j in 1:length(functionList)) {
          val<-functionList[[j]](basethetas=thetas,covthetas=coveffects,dfrow=dfCovs[i,],etas=rep(0,length(thetas)),...)
          listcount<-length(val) 
          
          for (l in 1:listcount) {
            dftmp<-dplyr::bind_rows(dftmp,data.frame(ITER=k,COVS=i,NAME=as.character(functionListName[n]),VALUE=val[[l]]))
            n<-n+1
          }
        }
        dftmp
      }
      dfrest<-dplyr::bind_rows(dfrest,dftmp1)
    }
  } #Type==1
  
    
  
  dfres<-data.frame()
  for (j in 1:length(functionListName)) {
    TOTVAR<-var(subset(dfrest,NAME==as.character(functionListName[j]) & COVS==0)$VALUE) #Get total variability, i.e. ITER == 0
    TOTCOVVAR<-var(subset(dfrest,NAME==as.character(functionListName[j]) & COVS==-1)$VALUE) #Get total covariate variability, i.e. ITER == -1
    for (i in 1:nrow(dfCovs)) {
      dfres<-rbind(dfres,
                   data.frame(COVNUM=i,COVNAME=cstrCovariates[i],PARAMETER=functionListName[j],
                              TOTVAR=TOTVAR,
                              TOTCOVVAR=TOTCOVVAR,
                              COVVAR=var(subset(dfrest,NAME==as.character(functionListName[j]) & COVS==i)$VALUE)))
    }
  }
  stopImplicitCluster()
  return(dfres)
}


