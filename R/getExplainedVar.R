#' Calculate the explained variability for covariates in a FREM model.
#'
#' Get a data frame with explainable variability information based on a dataset
#' of subjects with covariates
#'
#' The total variability of the metric (e.g. a primary or secondary parameter)
#' is derived from the FREM model from the estimated multivariate OMEGA matrix,
#' estimated EBEs, or sampled ETAs from the estimated multivariate OMEGA matrix,
#' and setting the FREM covariate coefficients to zero. There are four
#' alternatives that can be requested via the type argument.
#'
#' **type=0:** Use the the delta rule (FO approximation) to derive the total variability
#' of the metric across the involved omegas.
#'
#' **type=1:** Use the estimated EBEs and calculate the variance of the desired
#' metric across the individuals in the data set. Requires etas or a phi-file
#' associated with the model. The default method to calculate the total
#' variability.
#'
#' **type=2:** Use numETASamples sampled eta vectors from the
#' estimated multivariate omega matrix to compute numETASamples values of the
#' metric. The total variability is the average (over all individuals) of the
#' variances of the numETASamples values of the metrics.
#'
#' **type=3:** Use numETASamples sampled eta vectors from the
#' estimated multivariate omega matrix to compute numETASamples values of the
#' metric for one individual (the first) in the data set. The total variability
#' is the variance of the numETASamples values of the metric.
#'
#' If there are no FFEM covariate relationships in the model, then type 2 and 3
#' are identical, but type=3 will be faster.
#'
#' type=0 will be fast but not always accurate.
#'
#' type=1 will also be relatively fast and handles FFEM covariates, but is
#' sensitive to shrinkage and small data sets.
#'
#' type=2 is the slowest way to calculate the total variability but handles FFEM
#' covariates and is not sensitive to shrinkage.
#'
#' type=3 is not sensitive to shrinkage and is preferred over type=2 if there
#' are no FFEM covariates.
#'
#' @param type How the total variability should be derived. See Details.

#' @param data the data.frame with dataset to base the explained variability
#'   on, used with type=1-3
#' @param dfCovs A data frame with covariates to based the variability plots on
#' @param dfext a data frame with the final estimates in a ext-file format
#' @param strID the subject identifier in the dfCovs dataset, default='ID'
#' @inheritParams getFileNames
#' @param cstrCovariates A string vector with names of the covariates that
#'   should be investigated, i.e. must have the same length as the number of
#'   rows in dfCovs. If NULL then COV1, COV2 etc. will be used.
#' @param functionList A list of functions with input (basethetas,
#'   covthetas,dfrow and ...) for in which the explained variability will be
#'   calculated. If the function returns a vector of values, each value will be
#'   used but functionListName must contain the names with a length of all
#'   return for all functions in the functionList
#' @param functionListName A vector of strings (names) of the parameters for
#'   each function in the functionList
#' @param numNonFREMThetas Number of structural thetas in FREM model
#' @param numFREMThetas Number of covariate thetas in FREM model
#' @param numSigmas Number of sigmas in FREM model
#' @param numParCov Number of parameters for which covariate relations are
#'   sought (often the same as numNonFREMThetas).
#' @param numSkipOm Number of Omegas that are not associated with FREM
#'   covariates, i.e. skip before calculating the FREM varianes, default= 0
#' @param parNames Names of the parameters
#' @param availCov Names of the covariates to use in the calculation of the FFEM
#'   model, default=NULL (use all covariates)
#' @param etas the etas used to calculate the explained variability, used with
#'   type=1 and should be the same size as number of individuals in data. If
#'   `NULL` when type=1 the estas will be obtained from the phi-file.
#' @param quiet If output should be allowed during the function call, default=
#'   FALSE,
#' @param ncores the number of cores to use for the calculations, default = 1
#'   which means no parallellization
#' @param cstrPackages a character vector with the packages needed to run
#'   calculations in parallel, default = NULL
#' @param cstrExports a character vector with variables needed to run the
#'   calculations in parallel, default = NULL
#' @param numETASamples (default = 100) the number of samples used ot integrate
#'   over individual parameters when calculating the total variance of the
#'   functionList, only used using type==2 & type==3
#' @param seed (default = -1 = random, used when sampling ETAs in type==2 and
#'   type==3)
#' @param ... additional variables to be forwarded to the the functionList
#'   functions
#'
#' @return A data frame with summary statistics for each parameters and
#'   covariate combinations. There will be one row for each parameter-covariate
#'   combination + one row for each partameter with the combination of all
#'   covariates.
#'
#' * COVNUM: The covariate number. Repeated per PARAMETER. 1 is the explained variability of All covariates.
#' * COVNAME: The name of the covariate as specified in the original data set.
#' * PARAMETER: The name(s) of the parameter(s).
#' * TOTVAR: The total variability according to `type`. Repeated for each parameter-covariate combination.
#' * TOTCOVVAR: The total variability explained when all covariates are used.Repeated for each parameter-covariate combination.
#' * COVVAR: The variability explained by each covariate by itself.
#'
#' @export
#'
#' @details
#' The \code{availCov} argument specifies the covariates in the frem model that should be used for the calculations
#' of the maximum variability (TOTCOVVAR). If NULL (default), all covariates inteh frem model will be used for the derivation of TOTCOVVAR.
#'
#' @examples
#' modDevDir <- system.file("extdata/SimNeb",package="PMXFrem")
#' fremRunno <- 31
#' modFile   <- file.path(modDevDir,paste0("run",fremRunno,".mod"))
#' covNames  <- getCovNames(modFile = modFile)
#'
#' ## Set up dfCovs
#' dfData <- read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")) %>%
#'   filter(BLQ == 0) %>%
#'   distinct(ID,.keep_all = T)
#'
#' dfCovs <- setupdfCovs(modFile)
#'
#' cstrCovariates <- c("All",names(dfCovs))
#'
#' ## The parameter function list
#' functionList2 <- list(
#'   function(basethetas,covthetas, dfrow, etas, ...){ return(basethetas[2]*exp(covthetas[1] + etas[3]))},
#'   function(basethetas,covthetas, dfrow, etas, ...){ return(basethetas[3]*exp(covthetas[2] + etas[4]))}
#' )
#' functionListName2 <- c("CL","V")
#'
#' dfres1 <- getExplainedVar(type             = 1,
#'                           data             = dfData,
#'                           dfCovs           = dfCovs,
#'                           numNonFREMThetas = 7,
#'                           numSkipOm        = 2,
#'                           functionList     = functionList2,
#'                           functionListName = functionListName2,
#'                           cstrCovariates   = cstrCovariates,
#'                           modDevDir        = modDevDir,
#'                           runno            = fremRunno,
#'                           ncores           = 10,
#'                           quiet            = TRUE,
#'                           seed             = 123
#' )

getExplainedVar <- function(
    type             = 1,
    data,
    dfCovs,
    dfext            = NULL,
    strID            = "ID",
    runno            = NULL,
    modName          = NULL,
    modDevDir        = ".",
    cstrCovariates   = NULL,
    functionList     = list(function(basethetas, covthetas, dfrow, etas, ...) {
      return(basethetas[1] * exp(covthetas[1] + etas[1]))
    }),
    functionListName = "PAR1",
    numNonFREMThetas,
    numFREMThetas    = length(grep("THETA", names(dfext))) - numNonFREMThetas,
    numSigmas        = length(grep("SIGMA", names(dfext))),
    numParCov        = NULL,
    parNames         = NULL,
    numSkipOm        = 0,
    availCov         = NULL,
    etas             = NULL,
    quiet            = FALSE,
    ncores           = 1,
    cstrPackages     = NULL,
    cstrExports      = NULL,
    numETASamples    = 100,
    seed             = NULL,
    ...) {

  if(type>0 & missing(data)) stop("data can not be missing with type 1-3.")

  fileNames <- getFileNames(runno = runno, modName = modName, modDevDir = modDevDir, ...)
  modFile   <- fileNames$mod
  extFile   <- fileNames$ext
  phiFile   <- fileNames$phi

  if (is.null(dfext)) {
    dfext   <- getExt(extFile = extFile)
  }

  if (nrow(dfext) > 1) dfext <- dfext[dfext$ITERATION == -1000000000, ]
  thetas <- as.numeric(dfext[2:(numNonFREMThetas + 1)])

  ## Check that cstrCovariates has the same length as the number of rows in dfCovs
  if (is.null(cstrCovariates)) {
    cstrCovariates <- paste0("COV", 1:nrow(dfCovs))
  } else {
    if (length(cstrCovariates) != nrow(dfCovs)) {
      stop("cstrCovariates must have the same length as the number of rows in dfCovs")
    }
  }

  if (is.null(parNames)) {
    if (is.null(numParCov)) {
      iNumOmega <- length(grep("OMEGA", names(dfext)))
      numTotEta <- -1 / 2 + sqrt(1 / 4 + 2 * iNumOmega)
      numParCov <- numTotEta - numSkipOm - numFREMThetas
    }
    parNames <- paste("Par", 1:numParCov, sep = "")
  }

  if (type == 1 && is.null(etas)) {
    dfPhi   <- getPhi(phiFile)
    etas    <- dfPhi[, 3:(2 + numParCov + numSkipOm)] # Include the structural model etas only
  }

  if (!is.null(seed))
    set.seed(seed)

  CN       <- getCovNames(modFile)
  fremCovs <- CN$polyCatCovs
  orgCovs  <- CN$orgCovNames
  covNames <- CN$covNames

  ## Sort out the logic for availCov. allCov will indicate the covariates to base TOTCOVVAR on
  if(is.null(availCov)) {
    allCov <- covNames
  } else {
    allCov <- availCov
  }


  # if (is.null(availCov)) availCov <- covNames

  # Function to get FREM covariate names from FFEM covariates
  getFREMCovNames <- function(currNames) {
    covrow <- NULL
    ffemCovs <- stringr::str_replace(fremCovs, "_[0-9]*", "")

    for (cov in c(currNames, fremCovs)) {
      myCov <- str_replace(cov, "_[0-9]*", "")
      index <- which(cov == fremCovs)
      # If a FREM binarized covariate
      if (!is.null(index) && length(index) > 0) {
        if (cov %in% currNames) covrow <- c(covrow, cov)
      } else {
        index <- which(myCov == ffemCovs)
        # If not a FREM binarized covariate
        if (is.null(index) || length(index) == 0) {
          covrow <- c(covrow, cov)
        } else {
          covrow <- c(covrow, fremCovs[index])
        }
      }
    }
    return(unique(covrow))
  }

  # Delta rule based derivation of explained variability
  if (type == 0) {
    # Define delta_rule function
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
      availCov = allCov, quiet = quiet, numParCov = numParCov, numSkipOm = numSkipOm
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


  if (type == 1 || type == 2 || type == 3) { # Assuming explained variability based on data + ETA values (or sampled ETA values)

    if (type == 2 || type == 3) { # Get the ETA samples from N(0,1) and then rescale to correct variance
      ETAsamples <- matrix(rnorm((numParCov + numSkipOm) * numETASamples), nrow = (numParCov + numSkipOm), ncol = numETASamples)
    }

    for (cov in fremCovs) {
      myCov <- str_replace(cov, "_[0-9]*", "")
      myCovNum <- str_replace(cov, paste0(myCov, "_"), "")
      if (!myCov %in% names(data)) {
        warning(paste0("Can't find ", myCov, " in the dataset, exiting!"))
      }
      data[[cov]] <- ifelse(data[[myCov]] == myCovNum, 1, 0)
    }
    dataI <- data[!duplicated(strID), ] # Get one row per subject and keep only covariates and ID

    ## Check that the number of etas is the same as the number of subjects in the data set
    if (type == 1 && (nrow(etas) != nrow(dataI))) stop("The number of etas should be the same as the number of sibjects in the data set.")


    ## Register to allow for parallel computing
    if (ncores > 1) registerDoParallel(cores = ncores)

    mapFun <- function(data, orgCovs) {
      for (cov in orgCovs) {
        if (data[1, cov] == -99 & length(grepl(cov, names(data))) > 1) {
          data[1, grepl(cov, names(data))] <- -99
        }
      }
      return(data)
    }

    if (ncores > 1) {
      dataI <- foreach(k = 1:nrow(dataI)) %dopar% {
        mapFun(data = dataI[k, ], orgCovs = orgCovs)
      }
      dataI <- data.frame(rbindlist(dataI))
    } else {
      dataI2 <- data.frame()
      for (k in 1:nrow(dataI)) dataI2 <- bind_rows(dataI2, mapFun(data = dataI[k, ], orgCovs = orgCovs))
      dataI <- dataI2
    }
    dataI$jxrtp47 <- -99 # Just a dummy thing

    #### Go through all dfCovs combinations to calculate the variability for each of them
    dfrest <- data.frame()
    for (i in 1:nrow(dfCovs)) {
      currentNames <- names(dfCovs[i, ,drop=FALSE])[as.numeric(dfCovs[i, ]) != -99]

      strCovsRow <- names(dfCovs[i, ])[as.numeric(dfCovs[i, ]) != -99] # Get the covariate that we should condition on

      if (type == 3 || type == 2) { # Get the type of
        ffemObjAllNoCov <- calcFFEM(
          numNonFREMThetas = numNonFREMThetas, numFREMThetas = numFREMThetas, numSigmas = numSigmas, dfext = dfext, covNames = covNames,
          availCov = NULL, quiet = quiet, numParCov = numParCov, numSkipOm = numSkipOm
        )
        Chol <- chol(ffemObjAllNoCov$FullVars) # Get the covariance matrix and then Cholesky decompose
        etasamples <- t(ETAsamples) %*% Chol # Transform the ETA samples to N(0,COV) matrix
      }

      internalCalc <- function(k) { # The calculation function

        # Get the FREM covariates that is used in each row of dfCovs
        tmpcovs      <- getFREMCovNames(currentNames)
        dftmp        <- data.frame()
        datatmp      <- dataI[k, covNames] # Get only covnames
        avcov        <- names(datatmp)[which(datatmp != -99)] # Get the non-missing covariates only
        data47_jxrtp <- datatmp
        coveffects   <- rep(0, length(parNames))

        # Calculate the FFEM based on some know covariates based on the row in dfCovs which are non-missing
        ffemObj <- calcFFEM(
          numNonFREMThetas = numNonFREMThetas, numFREMThetas = numFREMThetas, numSigmas = numSigmas, dfext = dfext, covNames = covNames,
          availCov = avcov[avcov %in% tmpcovs], quiet = quiet, numParCov = numParCov, numSkipOm = numSkipOm
        )

        for (j in 1:length(parNames)) {
          ffem_expr <- stringr::str_replace_all(ffemObj$Expr[j], pattern = "data\\$", replacement = "data47_jxrtp$")
          if (length(names(dfCovs[i, ,drop=F])[as.numeric(dfCovs[i, ,drop=F]) != -99]) != 0) coveffects[j] <- as.numeric(eval(parse(text = ffem_expr)))
        }

        if (i == 1) { # If first row in dfCovs, Calculate a FFEM for each individual to get the total covariate variability as well
          ffemObjAll <- calcFFEM(
            numNonFREMThetas = numNonFREMThetas, numFREMThetas = numFREMThetas, numSigmas = numSigmas, dfext = dfext, covNames = covNames,
            availCov = avcov[avcov %in% allCov], quiet = quiet, numParCov = numParCov, numSkipOm = numSkipOm
          )
          coveffectsAll <- rep(0, length(parNames))

          for (j in 1:length(parNames)) {
            ffem_expr_all <- stringr::str_replace_all(ffemObjAll$Expr[j], pattern = "data\\$", replacement = "data47_jxrtp$")
            if (length(avcov) != 0) {
              coveffectsAll[j] <- as.numeric(eval(parse(text = ffem_expr_all)))
            }
          }
          ## Call each parameters in the functionList to calculate, this is for covariate +
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
            valeta0 <- functionList[[j]](basethetas = thetas, covthetas = coveffectsAll, dfrow = dataI[k, ], etas = rep(0, 3*length(thetas)), ...) # Will use a multiple of 3 to handle situations when there are more etas than thetas.
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

        ## Call each parameters in the functionList to calculate
        n <- 1

        for (j in 1:length(functionList)) {
          datatmp   <- dataI[k, c(tmpcovs, "jxrtp47")]
          val       <- functionList[[j]](basethetas = thetas, covthetas = coveffects, dfrow = datatmp, etas = rep(0, 3*length(thetas)), ...) # Will use a multiple of 3 to handle situations when there are more etas than thetas.
          listcount <- length(val)

          for (l in 1:listcount) {
            dftmp <- dplyr::bind_rows(dftmp, data.frame(ITER = k, COVS = i, NAME = as.character(functionListName[n]), VALUE = val[[l]]))
            n     <- n + 1
          }
        }

        return(dftmp)
      }


      if (ncores > 1) {
        dftmp1 <- foreach (k = 1:nrow(dataI),
          .packages = cstrPackages,
          .export = cstrExports, .verbose = !quiet,
          .combine = bind_rows) %dopar% {
          internalCalc(k)
        }
      } else {
        dftmp1 <- data.frame()
        for (k in 1:nrow(dataI)) dftmp1 <- bind_rows(dftmp1, internalCalc(k))
      }
      dfrest <- dplyr::bind_rows(dfrest, dftmp1)
    }
  } # Type==1


  dfres <- data.frame()
  for (j in 1:length(functionListName)) {
    if (type == 3) {
      TOTVAR <- subset(dfrest, NAME == as.character(functionListName[j]) & COVS == 0)$VALUE # Get total variability, i.e. ITER == 0
    } else {
      if (type == 2) {
        # Take mean of variances
        TOTVAR <- mean(subset(dfrest, NAME == as.character(functionListName[j]) & COVS == 0)$VALUE) # Get total variability, i.e. ITER == 0
      } else {
        TOTVAR <- var(subset(dfrest, NAME == as.character(functionListName[j]) & COVS == 0)$VALUE) # Get total variability, i.e. ITER == 0
      }
    }
    TOTCOVVAR <- var(subset(dfrest, NAME == as.character(functionListName[j]) & COVS == -1)$VALUE) # Get total covariate variability, i.e. ITER == -1

    for (i in 1:nrow(dfCovs)) {
      dfres <- rbind(dfres,
        data.frame(COVNUM = i, COVNAME = cstrCovariates[i], PARAMETER = functionListName[j],
          TOTVAR = TOTVAR,
          TOTCOVVAR = TOTCOVVAR,
          COVVAR = var(subset(dfrest, NAME == as.character(functionListName[j]) & COVS == i)$VALUE)))
    }
  }
  if (ncores > 1) stopImplicitCluster()
  return(dfres)
}
