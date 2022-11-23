#' getExplainableVarDF()
#'
#' @description Get a data frame with explainable variability information based on a dataset of subjects with covariates
#'
#'
#' @param type Which type of explained var we should use, type=0 (based on FO delta rule),
#'                                                        type=1 (default), i.e. based on data and calculated etas (ebes),
#'                                                        type=2 is that the total variability is calculated using ETA samples instead of EBEs and average of individual data for fixed cov relationships, hence etas argument is not needed but numETASamples is needed instead.
#'                                                        type=3 is that the total variability is calculated using ETA samples instead of EBEs and using the first individual data for fixed cov relationships, hence etas argument is not needed but numETASamples is needed instead. If no fixed cov relationship are used, type=2 is exactly the same as type=3 but type=3 is faster.
#' @param data the dataset to based the explained variability on, used with type=1
#' @param dfCovs A data frame with covariates to based the variability plots on
#' @param dfext a data frame with the final estimates in a ext-file format
#' @param strID the subject identifier in the dfCovs dataset, default='ID'
#' @inheritParams getFileNames
#' @param cstrCovariates A string vector with names of the covariate that should be investigated, if NULL COV1, COV2 etc. will be assigned
#' @param functionList A list of functions with input (basethetas, covthetas,dfrow and ...) for in which the explained variability will be calculated. If the function returns a vector of values, each value will be used but functionListName must contain the names with a length of all return for all functions in the functionList
#' @param functionListName A vector of strings (names) of the parameters for each function in the functionList
#' @param numNonFREMThetas Number of structural thetas in FREM model
#' @param numFREMThetas Number of covariate thetas in FREM model
#' @param numSigmas Number of sigmas in FREM model
#' @param numParCov Number of parameters for which covariate relations are sought (often the same as numNonFREMThetas).
#' @param numSkipOm Number of Omegas that are not associated with FREM covariates, i.e. skip before calculating the FREM varianes, default= 0
#' @param parNames Names of the parameters
#' @param availCov Names of the covariates to use in the calculation of the FFEM model, default=NULL (use all covariates)
#' @param etas the etas used to calculate the explained variability, used with type==1 and should be the same size as number of individuals in data
#' @param quiet If output should be allowed during the function call, default= FALSE,
#' @param ncores the number of cores to use for the calculations, default = 1 which means no parallellization
#' @param cstrPackages a character vector with the packages needed to run calculations in parallel, default = NULL
#' @param cstrExports a character vector with variables needed to run the calculations in parallel, default = NULL
#' @param numETASamples (default = 100) the number of samples used ot integrate over individual parameters when calculating the total variance of the functionList, only used using type==2 & type==3
#' @param seed (default = -1 = random, used when sampling ETAs in type==2 and type==3)
#' @param ... additional variables to be forwarded to the the functionList functions

#'
#' @return a data frame with summary statistics for each parameters and covariate combinations:
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dfForest <- getExplainableDF(dfCovs)
#' }
getExplainableVarDF <- function(type=1,data,dfCovs,dfext=NULL,strID="ID",runno=NULL,modName=NULL,modDevDir=".",cstrCovariates=NULL,
                                functionList=list(function(basethetas,covthetas,dfrow,etas,...){return(basethetas[1]*exp(covthetas[1]+etas[1]))}),functionListName="PAR1",numNonFREMThetas, numFREMThetas=length(grep("THETA",names(dfext)))-numNonFREMThetas, numSigmas=length(grep("SIGMA",names(dfext))), numParCov = NULL,
                                parNames = NULL, numSkipOm=0, availCov = NULL, etas=NULL,quiet = FALSE,
                                ncores=1,cstrPackages=NULL,cstrExports=NULL,numETASamples=100,seed=NULL,...) {

  fileNames <- getFileNames(runno=runno,modName=modName,modDevDir=modDevDir,...)
  modFile   <- fileNames$mod
  extFile   <- fileNames$ext
  phiFile   <- fileNames$phi

  if (is.null(dfext)) {
    dfext   <- getExt(extFile = extFile)
  }

  if (nrow(dfext) > 1) dfext <- dfext[dfext$ITERATION == -1000000000, ]
  thetas <- as.numeric(dfext[2:(numNonFREMThetas + 1)])
  if (is.null(cstrCovariates)) {
    cstrCovariates <- paste0("COV", 1:nrow(dfCovs))
  }

  if (is.null(parNames)) {
    if (is.null(numParCov)) {
      iNumOmega <- length(grep("OMEGA", names(dfext)))
      numTotEta <- -1 / 2 + sqrt(1 / 4 + 2 * iNumOmega)
      numParCov <- numTotEta - numSkipOm - numFREMThetas
    }
    parNames <- paste("Par", 1:numParCov, sep = "")
}

  if (type==1 && is.null(etas)) {
    dfPhi   <- getPhi(phiFile)
    etas    <- dfPhi[, 3:(2+numParCov+numSkipOm)] # Include the structural model etas only
  }

  if (!is.null(seed))
    set.seed(seed)

  CN       <- getCovNames(modFile)
  fremCovs <- CN$polyCatCovs
  orgCovs  <- CN$orgCovNames
  covNames <- CN$covNames

  if (is.null(availCov)) availCov<-covNames

  #Function to get FREM covariate names from FFEM covariates
  getFREMCovNames <- function(currNames) {
  covrow <- NULL
  ffemCovs <- str_replace(fremCovs, "_[0-9]*", "")

  for (cov in c(currNames, fremCovs)) {
    myCov <- str_replace(cov, "_[0-9]*", "")
    index <- which(cov == fremCovs)
    # If a FREM binarized covariate
    if (!is.null(index) & length(index) > 0) {
      covrow <- c(covrow, cov)
    } else {
      index <- which(myCov == ffemCovs)
      # If not a FREM binarized covariate
      if (is.null(index) | length(index) == 0) {
        covrow <- c(covrow, cov)
      }
    }
  }
  return(covrow)
}

  #Delta rule based derivation of explained variability
  if (type==0) {

    #Define delta_rule function
    deltarule <- function(params, covmatrix, transform_fun, ...) {
      param_new <- transform_fun(params, ...) # Calculate the transformation

      # Calculate the derivatives by numeric differentiation with the numDeriv library
      # library(numDeriv)
      # Options to grad function, i.e. could be e.g. global settings
      if (!exists("ma")) ma <- list(eps = 1e-4, d = 0.0001, zero.tol = sqrt(.Machine$double.eps / 7e-7), r = 4, v = 2, show.details = FALSE)
      param_deriv <- grad(transform_fun, params, method = "Richardson", method.args = ma, ...)

      new_var <- 0 # Initialize the variance of the transformed function
      for (i in 1:length(params)) {
        for (j in 1:length(params)) {
          new_var <- new_var + param_deriv[i] * param_deriv[j] * covmatrix[i, j]
        }
      }

      return(c(param_new, new_var)) # Return a vector with the tranformed parameter value and the transformed parameter variance
    }

    # Define internal wrapper function to be used with propagation of variabilities
    parf <- function(x, basethetas, covthetas, dfrow, myfunc, ...) {
      return(unlist(myfunc(basethetas, covthetas, dfrow, x, ...)))
    }

    ffemObjAllNoCov <- calcFFEM(
      numNonFREMThetas = numNonFREMThetas, numFREMThetas = numFREMThetas, numSigmas = numSigmas, dfext = dfext, covNames = covNames,
      availCov = NULL, quiet = quiet, numParCov = numParCov, numSkipOm = numSkipOm
    )

    ffemObjAllCov <- calcFFEM(
      numNonFREMThetas = numNonFREMThetas, numFREMThetas = numFREMThetas, numSigmas = numSigmas, dfext = dfext, covNames = covNames,
      availCov = covNames, quiet = quiet, numParCov = numParCov, numSkipOm = numSkipOm
    )

    dfres <- data.frame()
    for (j in 1:length(functionListName)) {
      TOTVAR <- deltarule(
        params = rep(0, length(diag(ffemObjAllNoCov$FullVars))), covmatrix = ffemObjAllNoCov$FullVars, transform_fun = parf, basethetas = thetas,
        covthetas = rep(0, length(parNames)), dfrow = dfCovs[1, ], myfunc = functionList[[j]], ...
      )[2]
      TOTCOVVAR <- deltarule(
        params = rep(0, length(diag(ffemObjAllCov$FullVars))), covmatrix = ffemObjAllCov$FullVars, transform_fun = parf, basethetas = thetas,
        covthetas = rep(0, length(parNames)), dfrow = dfCovs[1, ], myfunc = functionList[[j]], ...
      )[2]


      for (i in 1:nrow(dfCovs)) {
        currentNames <- names(dfCovs[i, ])[as.numeric(dfCovs[i, ]) != -99]
        tmpcovs <- getFREMCovNames(currentNames)
        # Calculate the FFEM based on some know covariates based on the row in dfCovs which are non-missing
        ffemObj <- calcFFEM(
          numNonFREMThetas = numNonFREMThetas, numFREMThetas = numFREMThetas, numSigmas = numSigmas, dfext = dfext, covNames = covNames,
          availCov = tmpcovs, quiet = quiet, numParCov = numParCov, numSkipOm = numSkipOm
        )
        COVVAR <- deltarule(
          params = rep(0, length(diag(ffemObj$FullVars))), covmatrix = ffemObj$FullVars, transform_fun = parf, basethetas = thetas,
          covthetas = rep(0, length(parNames)), dfrow = dfCovs[1, ], myfunc = functionList[[j]], ...
        )[2]
        dfres <- rbind(
          dfres,
          data.frame(
            COVNUM    = i, COVNAME = cstrCovariates[i],
            PARAMETER = functionListName[j],
            TOTVAR    = TOTVAR,
            TOTCOVVAR = TOTVAR - TOTCOVVAR,
            COVVAR    = TOTVAR - COVVAR
          )
        )
      }
    }
    return(dfres)
  }


  if (type==1 || type==2 || type==3) { #Assuming explained variability based on data + ETA values (or sampled ETA values)

    if (type==2 || type==3) {#Get the ETA samples from N(0,1) and then rescale to correct variance
      ETAsamples<-matrix(rnorm((numParCov+numSkipOm)*numETASamples),nrow=(numParCov+numSkipOm),ncol=numETASamples)
    }

    for(cov in fremCovs) {
      myCov <- str_replace(cov,"_[0-9]*","")
      myCovNum <- str_replace(cov,paste0(myCov,"_"),"")
      if (!myCov %in% names(data)) {
        warning(paste0("Can't find ",myCov," in the dataset, exiting!"))
      }
      data[[cov]] <- ifelse(data[[myCov]]==myCovNum,1,0)
    }
    dataI <- data[!duplicated(strID),] #Get one row per subject and keep only covariates and ID

    ## Register to allow for parallel computing
    if (ncores>1) registerDoParallel(cores = ncores)

    mapFun <- function(data,orgCovs)  {
      for(cov in orgCovs) {
        if(data[1,cov]==-99 & length(grepl(cov,names(data))) > 1) {
          data[1,grepl(cov,names(data))] <- -99
        }
      }
      return(data)
    }

    if (ncores>1) {
      dataI <- foreach(k = 1:nrow(dataI)) %dopar% {
        mapFun(data=dataI[k,],orgCovs=orgCovs)
      }
      dataI <- data.frame(rbindlist(dataI))
    } else {
      dataI2<-data.frame()
      for (k in 1:nrow(dataI)) dataI2<-bind_rows(dataI2,mapFun(data=dataI[k,],orgCovs=orgCovs))
      dataI<-dataI2
    }
    dataI$jxrtp47<- -99 #Just a dummy thing

    #### Go through all dfCovs combinations to calculate the variability for each of them
    dfrest<-data.frame()
    for (i in 1:nrow(dfCovs)) {
      currentNames <- names(dfCovs[i, ])[as.numeric(dfCovs[i, ]) != -99]

      strCovsRow <- names(dfCovs[i, ])[as.numeric(dfCovs[i, ]) != -99] # Get the covariate that we should condition on

      if (type == 3 || type == 2) { # Get the type of
        ffemObjAllNoCov <- calcFFEM(
          numNonFREMThetas = numNonFREMThetas, numFREMThetas = numFREMThetas, numSigmas = numSigmas, dfext = dfext, covNames = covNames,
          availCov = NULL, quiet = quiet, numParCov = numParCov, numSkipOm = numSkipOm
        )
        Chol <- chol(ffemObjAllNoCov$FullVars) # Get the covariance matrix and then Cholesky decompose
        etasamples <- t(ETAsamples) %*% Chol # Transform the ETA samples to N(0,COV) matrix
      }

      internalCalc<-function(k){ #The calculation function

        #Get the FREM covariates that is used in each row of dfCovs
        tmpcovs      <- getFREMCovNames(currentNames)
        dftmp        <- data.frame()
        datatmp      <- dataI[k, covNames] # Get only covnames
        avcov        <- names(datatmp)[which(datatmp != -99)] # Get the non-missing covariates only
        data47_jxrtp <- datatmp
        coveffects   <- rep(0, length(parNames))

        #Calculate the FFEM based on some know covariates based on the row in dfCovs which are non-missing
        ffemObj <- calcFFEM(
          numNonFREMThetas = numNonFREMThetas, numFREMThetas = numFREMThetas, numSigmas = numSigmas, dfext = dfext, covNames = covNames,
          availCov = avcov[avcov %in% tmpcovs], quiet = quiet, numParCov = numParCov, numSkipOm = numSkipOm
        )

        for (j in 1:length(parNames)) {
          ffem_expr <- stringr::str_replace_all(ffemObj$Expr[j], pattern = "data\\$", replacement = "data47_jxrtp$")
          if (length(names(dfCovs[i, ])[as.numeric(dfCovs[i, ]) != -99]) != 0) coveffects[j] <- as.numeric(eval(parse(text = ffem_expr)))
        }

        if (i==1) {#If first row in dfCovs, Calculate a FFEM for each individual to get the total covariate variability as well
          ffemObjAll <- calcFFEM(
            numNonFREMThetas = numNonFREMThetas, numFREMThetas = numFREMThetas, numSigmas = numSigmas, dfext = dfext, covNames = covNames,
            availCov = avcov, quiet = quiet, numParCov = numParCov, numSkipOm = numSkipOm
          )
          coveffectsAll <- rep(0, length(parNames))

          for (j in 1:length(parNames)) {
            ffem_expr_all <- stringr::str_replace_all(ffemObjAll$Expr[j], pattern = "data\\$", replacement = "data47_jxrtp$")
            if (length(avcov) != 0) {
              coveffectsAll[j] <- as.numeric(eval(parse(text = ffem_expr_all)))
            }
          }
          ##Call each parameters in the functionList to calculate, this is for covariate +
          n <- 1
          for (j in 1:length(functionList)) {
            if (type == 2) {
              val <- 0
              tmpval <- 0
              for (m in 1:numETASamples) { # For all ETA samples
                val <- functionList[[j]](basethetas = thetas, covthetas = rep(0, length(coveffectsAll)), dfrow = dataI[k, ], etas = etasamples[m, ], ...) ### CHECK THIS, coveffectsAll=0?

                if (m == 1) {
                  tmpval <- matrix(0, ncol = numETASamples, nrow = length(val))
                }
                tmpval[, m] <- unlist(val)
              }
              for (m in 1:nrow(tmpval)) {
                val[m] <- var(tmpval[m, ]) # Calculate var over all samples
              }
            }
            if (type == 1) {
              val <- functionList[[j]](basethetas = thetas, covthetas = rep(0, length(coveffectsAll)), dfrow = dataI[k, ], etas = as.numeric(etas[k, ]), ...)
            }
            if (type == 3 && k == 1) { # only for "one" individual
              tmpval <- 0
              val <- 0
              for (m in 1:numETASamples) { # For all ETA samples
                val <- functionList[[j]](basethetas = thetas, covthetas = rep(0, length(coveffectsAll)), dfrow = dataI[k, ], etas = etasamples[m, ], m, ...)
                if (m == 1) {
                  tmpval <- matrix(0, ncol = numETASamples, nrow = length(val))
                }
                tmpval[, m] <- unlist(val)
              }
              for (m in 1:nrow(tmpval)) {
                val[m] <- var(tmpval[m, ]) # Calculate var over all samples
              }
            }
            valeta0 <- functionList[[j]](basethetas = thetas, covthetas = coveffectsAll, dfrow = dataI[k, ], etas = rep(0, length(thetas)), ...)
            listcount <- length(valeta0)
            for (l in 1:listcount) {
              if (type != 3 || type == 3 && k == 1) {
                dftmp <- dplyr::bind_rows(dftmp, data.frame(ITER = k, COVS = 0, NAME = as.character(functionListName[n]), VALUE = val[[l]]))
              }
              dftmp <- dplyr::bind_rows(dftmp, data.frame(ITER = k, COVS = -1, NAME = as.character(functionListName[n]), VALUE = valeta0[[l]]))
              n <- n + 1
            }
          }
        }

        ##Call each parameters in the functionList to calculate
        n=1

        for (j in 1:length(functionList)) {
          datatmp   <- dataI[k, c(tmpcovs, "jxrtp47")]
          val       <- functionList[[j]](basethetas = thetas, covthetas = coveffects, dfrow = datatmp, etas = rep(0, length(thetas)), ...)
          listcount <- length(val)

          for (l in 1:listcount) {
            dftmp <- dplyr::bind_rows(dftmp, data.frame(ITER = k, COVS = i, NAME = as.character(functionListName[n]), VALUE = val[[l]]))
            n     <- n + 1
          }
        }

        return(dftmp)
      }


      if (ncores>1) {
        dftmp1<-foreach (k = 1:nrow(dataI),
                                  .packages = cstrPackages,
                                  .export = cstrExports,.verbose = !quiet,
                                  .combine=bind_rows) %dopar% {
          internalCalc(k)
        }
      } else {
        dftmp1<-data.frame()
        for (k in 1:nrow(dataI)) dftmp1<-bind_rows(dftmp1,internalCalc(k))
      }
      dfrest<-dplyr::bind_rows(dfrest,dftmp1)
    }
  } #Type==1

  dfres<-data.frame()
  for (j in 1:length(functionListName)) {
    if (type==3) {
      TOTVAR<-subset(dfrest,NAME==as.character(functionListName[j]) & COVS==0)$VALUE #Get total variability, i.e. ITER == 0
    } else {
      if (type==2) {
        #Take mean of variances
        TOTVAR<-mean(subset(dfrest,NAME==as.character(functionListName[j]) & COVS==0)$VALUE) #Get total variability, i.e. ITER == 0
      }else {
        TOTVAR<-var(subset(dfrest,NAME==as.character(functionListName[j]) & COVS==0)$VALUE) #Get total variability, i.e. ITER == 0
      }
    }
    TOTCOVVAR<-var(subset(dfrest,NAME==as.character(functionListName[j]) & COVS==-1)$VALUE) #Get total covariate variability, i.e. ITER == -1
    for (i in 1:nrow(dfCovs)) {
      dfres<-rbind(dfres,
                   data.frame(COVNUM=i,COVNAME=cstrCovariates[i],PARAMETER=functionListName[j],
                              TOTVAR=TOTVAR,
                              TOTCOVVAR=TOTCOVVAR,
                              COVVAR=var(subset(dfrest,NAME==as.character(functionListName[j]) & COVS==i)$VALUE)))
    }
  }
  if (ncores>1) stopImplicitCluster()
  return(dfres)
}


