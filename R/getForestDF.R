#' getForestDF()
#'
#' @description Get a data frame with Forest border for each univariate or multivariate covariate (and value(s)) in the input data frame
#' @param dfCovs A data frame with covariates to include, if a covariate value is set -99 or NA, they are assumed missing and hence not include in the FFEM transformations
#' @param cdfCovsNames A string vector 
#' @param functionList A list of functions with input (thetas, coveffects and ...) for in which the change from the reference value will be calculated
#' @param functionListName A vector of strings (names) of the parameters for each function in the functionList
#' @param nobaseThetas the number of structural thetas in the model
#' @param dfParameters A data frame with parameter samples from the uncertainty distribution. The final estimate vector is assumed to be at the first row. The column order is assumed the same as in the NONMEM ext file except the ITERATION and OBJ columns which are not expected.
#' @param quiet If output should be allowed during the function call, default= FALSE,
#' @param probs A vector of probabilities (uncertainty of the parameters)
#' @param dfRefRow A data frame (one row) with the covariate values that will be used as the reference, if NULL the typical subject is used as reference
#' @param cGrouping A vector of numbers on how to group the y-axis of the forest plot, the length of the vector should match the number of rows in dfCovs. If NULL (default) a educated guess of the grouping will be set
#' @param fixedSpacing A boolean (TRUE/FALSE) if fixed spacing between covariate groups should be applied, otherwise the y coordinates are calculated relative to the number of groups and numbers of covriates within a group. If fixed spacing are used, groupdist and withingroupdist will be used as well.
#' @param groupeddist A number defining the y distance between groups of covariates
#' @param withingroupeddist A number defining the y distance within groups of covariates

#' 
#'
#' @return a data frame with summary statistics for each parameters and covariate combinations:
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' dfForest <- getForestDF(dfCovs)
#' }
getForestDF <- function(dfCovs,cdfCovsNames=NULL,functionList=list(function(thetas,coveffects,...){return(thetas[1]*exp(coveffects[1]))}),functionListName="PAR1",noBaseThetas, noCovThetas, noSigmas, noParCov = noBaseThetas, dfParameters,
                        parNames = paste("Par", 1:noParCov, sep = ""), covNames = paste("Cov",1:noCovThetas, sep = ""),
                        availCov = covNames, quiet = FALSE,probs=c(0.025,0.5,0.975),dfRefRow=NULL,cGrouping=NULL,fixedSpacing=TRUE,groupdist=0.2,withingroupdist=0.1) {

  
  resList<-list()
  dfCovs[is.na(dfCovs)]<--99
  
  getGroups<-function(df){
    cGroups<-c()
    cUnique<-c()
    iGroup<-0
    for (i in 1:nrow(df)) {
      tmp<-paste0(names(dfCovs[i,])[as.numeric(dfCovs[i,])!=-99],collapse=",")
      if (tmp %in% cUnique) {
        tmpl=which(tmp==cUnique)
        cGroups<-c(cGroups,tmpl)
      } else {
        iGroup<-iGroup+1
        cGroups<-c(cGroups,iGroup)
        cUnique<-c(cUnique,tmp)
      }
    }
    return(cGroups)
  }
  if (is.null(cGrouping)) cGrouping<-getGroups(dfCovs)
  
  counter<-1
  for (k in 1:nrow(dfParameters)) {
    dfext<-cbind(first = 0, dfParameters[k,]) #Dummy to get same format as ext file
    thetas=as.numeric(dfext[2:(noBaseThetas+1)])
    for (i in 1:nrow(dfCovs)) {
      currentNames<-names(dfCovs[i,])[as.numeric(dfCovs[i,])!=-99]
      if (any(!currentNames %in% covNames$covNames)) {
        print(paste0("Can't find some of the covariates: ",currentNames," in the model, quiting."))
        return(NULL)
      }
      if (!is.null(dfRefRow)) { #Get another ref value than typical value
        ffemObjRef<-calcFFEM(noBaseThetas=noBaseThetas,noCovThetas = noCovThetas,noSigmas = noSigmas,dfext=dfext,covNames = covNames$covNames,
                          availCov = names(dfRefRow)[as.numeric(dfRefRow)!=-99],quiet = quiet)
      }
      
      ffemObj<-calcFFEM(noBaseThetas=noBaseThetas,noCovThetas = noCovThetas,noSigmas = noSigmas,dfext=dfext,covNames = covNames$covNames,
             availCov = names(dfCovs[i,])[as.numeric(dfCovs[i,])!=-99],quiet = quiet)
      coveffects <- rep(0,length(parNames))
      coveffects_base <- rep(0,length(parNames))
      data47_jxrtp <- dfCovs[i,]
      for(j in 1:length(parNames)) {
        ffem_expr<-str_replace_all(ffemObj$Expr[j],pattern = "data\\$",replacement = "data47_jxrtp$")
        if (length(names(dfCovs[i,])[as.numeric(dfCovs[i,])!=-99])!=0) coveffects[j] <- as.numeric(eval(parse(text=ffem_expr)))
        if (!is.null(dfRefRow)) {
           ffem_expr_base<-str_replace_all(ffemObjRef$Expr[j],pattern = "data\\$",replacement = "dfRefRow$")
           coveffects_base[j]<-as.numeric(eval(parse(text=ffem_expr_base)))
        }
      }
      for (j in 1:length(functionList)) {
        val<-functionList[[j]](thetas,coveffects,dfext)
        if (!is.null(dfRefRow)) { ## Add a new reference line based on some cvoariate values
          valbase=functionList[[j]](thetas,coveffects_base,dfext)
        } else {
          valbase=functionList[[j]](thetas,rep(0,length(parNames)),dfext)
        }
        resList[[counter]]<-data.frame(ITER=k,COVS=i,NAME=functionListName[j],VALUE=val,VALUEBASE=valbase)
        counter<-counter+1
      }
    }
  }
  dfres<-as.data.frame(data.table::rbindlist(resList))

  getCovNameString <- function(dfrow) {
    strName<-""
    colnames<-names(dfrow)
    for (i in 1:ncol(dfrow)) {
      if (dfrow[1,i]!=-99) {
        if (strName=="") {
          strName<-paste0(colnames[i],"=",dfrow[1,i])
        } else {
          strName<-paste0(strName,", ",colnames[i],"=",dfrow[1,i])
        }
      }
    }
    if (strName=="") strName<-"Ref cov"
    return (strName)
  }
  
  dfret<-data.frame()
  for (i in 1:nrow(dfCovs)) {
    if (is.null(cdfCovsNames)) {
      covname<-getCovNameString(dfCovs[i,])
    } else {
      covname<-cdfCovsNames[i]
    }
    group<-cGrouping[i]
    for (j in 1:length(functionList)) {
      dft<-dfres[dfres$COVS==i & dfres$NAME==functionListName[j],] #Subset each covariate and parameter
      quant<-quantile(dft$VALUE,probs = probs,names = FALSE)
      mean_base<-mean(dft$VALUEBASE)
      median_base<-median(dft$VALUEBASE)
      true_base<-dft$VALUEBASE[dft$ITER==1] #Assume first parameter vector in dfParameters are the base estimated parameter vector
      dfrow<-cbind(dfCovs[i,],data.frame(GROUP=group,COVNUM=i,COVNAME=covname,PARAMETER=functionListName[j],REFMEAN=mean_base,REFTRUE=true_base,REFMEDIAN=median_base))
      for (k in 1:length(probs)) {
        dfp<-data.frame("X1"=1)
        dfp[[paste0("q",k)]]<-quant[k]
        dfrow<-cbind(dfrow,dfp[,2])
        names(dfrow)[ncol(dfrow)]<-paste0("q",k)
      }
      dfret<-rbind(dfret,dfrow)
    }
  }
  
  
  if (!fixedSpacing) {
  #Relative sizes
  dfret$Y<-NA
  for (i in 1:length(sort(unique(dfret$GROUP)))) {
    dft<-dfret[dfret$GROUP==sort(unique(dfret$GROUP))[i],]
    num_in_group<-nrow(dft)/length(unique(dfret$PARAMETER))
    for (n in 1:length(unique(dft$COVNUM))) {
      dfret$Y[dfret$GROUP==sort(unique(dfret$GROUP))[i] & dfret$COVNUM==unique(dft$COVNUM)[n]]<-(i-1)+n/(num_in_group+1)
    }
  }
  } else {
  #Fixed sizes
  dfret$Y<-NA
  a<-0
  for (i in 1:length(sort(unique(dfret$GROUP)))) {
    dft<-dfret[dfret$GROUP==sort(unique(dfret$GROUP))[i],]
    num_in_group<-nrow(dft)/length(unique(dfret$PARAMETER))
    for (n in 1:length(unique(dft$COVNUM))) {
      dfret$Y[dfret$GROUP==sort(unique(dfret$GROUP))[i] & dfret$COVNUM==unique(dft$COVNUM)[n]]<-a
      a<-a+withingroupdist
    }
    a<-a+groupdist
    }
  }
  return(dfret)
}


