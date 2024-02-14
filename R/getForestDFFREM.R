#' getForestDFFREM
#'
#' @description Get a data frame with Forest border for each univariate or multivariate covariate (and value(s)) in the input data frame. If a list a data frame will be created from the list, see function dfCreateInputForestData
#'
#' @import doParallel
#' @import foreach
#' @import dplyr
#' @import stringr
#'
#'
#' @param dfCovs A data frame with covariates to include, if a covariate value is set -99 or NA,
#' they are assumed missing and will not be included in any FFEM transformations. If a dfCovs is
#' a list an attempt will be made to create the appropriate data frame with the createInputForestData function.
#' @param cdfCovsNames A string vector with names of the rows in dfCovs, if not used, names will be
#' automatically assigned based on the covariate values and column names in dfCovs.
#' @param functionList A list of functions with input (basethetas, covthetas,dfrow and ...) from which the change
#' from the reference value will be calculated. If the function returns a vector of values, each value will be used
#' but functionListName must contain the names with a length of all return for all functions in the functionList
#' @param functionListName A vector of strings (names) of the parameters for each function in the functionList
#' @param dfParameters A data frame with parameter samples from the uncertainty distribution.
#' The vector of final parameter estimates is assumed to be in the first row.
#' The column order is assumed the same as in the NONMEM ext file except the ITERATION and OBJ columns whichshould not be included.
#' @param quiet If output should be allowed during the function call, default= TRUE. (This option is mainly for debugging purposes.)
#' @param probs A vector of probabilities that should be computed for each of the parameters from functionList. These will be used as the
#' as the uncertainties in the Forest plots. The probs vector position one and two will be used for plotting the uncertanties (i.e. columns q1 and q2). Default is c(0.05, 0.95).
#' @param pointFunction The function used to calculate the point for each covariate in the forest plot. default=median
#' This function is also used for the reference covariate combination
#' @param dfRefRow A data frame  (one row or equal number of rows as dfCovs) with the covariate values that will be used as the reference, if NULL the typical subject is used as reference.
#' @param cGrouping A vector of numbers defining how to group the y-axis of the Forest plot, the length of the vector should match the number of rows in dfCovs.
#' If NULL (default) an educated guess of the grouping will be set
#' @param ncores the number of cores to use for the calculations, default = 1 which means no parallellization
#' @param cstrPackages a character vector with package names needed to run the calculations in parallel, default = NULL
#' @param cstrExports a character vector with variables needed to run the calculations in parallel, default = NULL
#' @param iMiss The missing value number. -99 by default.
#' @param ... additional variables to be forwarded to the the functionList functions
#' @param numNonFREMThetas Number of thetas that are not FREM covariates. These need to come before the FREM covariate thetas.
#' @param numSkipOm Number of diag omegas (variances) that should not be part of the FREM calculations. Such omegas has to come before the large FREM omega block.
#' @param parNames Names of the parameters
#' @param covNames Names of the covariates
#' @param availCov Names of the covariates to use in the calculation of the FFEM model.
#'
#'
#' @return A data frame with summary statistics for each parameters and covariate combinations:
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dfresFREM <-getForestDFFREM(dfCovs           = dfCovs,
#'                             cdfCovsNames     = covnames,
#'                             covNames         = PMXFrem::getCovNames(modFile),
#'                             functionList     = list(paramFunction),
#'                             functionListName = functionListName,
#'                             numSkipOm        = numSkipOm,
#'                             numNonFREMThetas = numNonFREMThetas,
#'                             dfParameters     = dfSamplesCOV,
#'                             probs            = c(0.05, 0.95),
#'                             dfRefRow         = NULL,
#'                             quiet            = TRUE,
#'                             ncores           = 1,
#'                             cstrPackages     = c("PMXFrem","dplyr"))
#' }

getForestDFFREM <- function(dfCovs,
                            cdfCovsNames     = NULL,
                            functionList     = list(function(basethetas,covthetas, dfrow, ...) {
                              return(basethetas[1] * exp(covthetas[1]))
                            }),
                            covNames,
                            functionListName = "PAR1",
                            numSkipOm        = 0,
                            numNonFREMThetas,
                            dfParameters,
                            parNames         = paste("Par",1:numParCov,sep = ""),
                            availCov         = covNames,
                            quiet            = FALSE,
                            probs            = c(0.05, 0.95),
                            pointFunction    = median,
                            dfRefRow         = NULL,
                            cGrouping        = NULL,
                            ncores           = 1,
                            cstrPackages     = NULL,
                            cstrExports      = NULL,
                            iMiss            = -99,
                            ...) {


  if(is.list(covNames)) stop("covNames should no longer be a list. Perhaps you want to use getCovNames(modFile)$covNames?")

  if (!is.null(dfRefRow) && nrow(dfRefRow)!=1 && nrow(dfRefRow)!=nrow(dfCovs)) {
    stop("The number of reference rows (dfRefRow) should be either NULL (missing used as reference), one (this row used as reference) or equal to dfCovs (change reference for each covariate combination)")
  }
  ## Try to make dfCovs into a data.frame if it isn't that already
  if (!is.data.frame(dfCovs)) {
    dfCovs <- createInputForestData(dfCovs)
  }
  ## Replace potential NAs in dfCovs with the missing value token
  dfCovs[is.na(dfCovs)] <- iMiss

  #If a needed covariate is not present in dfCovs, set it to missing
  if(!all(covNames %in% names(dfCovs))) {
    if (!quiet) warning("Not all covariates in frem model are present in dfCovs, setting them to missing")
    dfCovs[,covNames[!(covNames %in% names(dfCovs))]] <-iMiss
  }



  groupnames<-NULL #Store the temp groupnames
  if (any(names(dfCovs)=="COVARIATEGROUPS")) {
    groupnames<-dfCovs[,"COVARIATEGROUPS"]
    dfCovs[,"COVARIATEGROUPS"]<-NULL
  }

  ## Create function to sort out grouping
  getGroups <- function(df) {
    cGroups <- c()
    cUnique <- c()
    iGroup <- 0
    for (i in 1:nrow(df)) {
      tmp <- paste0(names(dfCovs[i, ])[as.numeric(dfCovs[i, ]) != iMiss], collapse = ",")
      if (tmp %in% cUnique) {
        tmpl <- which(tmp == cUnique)
        cGroups <- c(cGroups, tmpl)
      }else {
        iGroup <- iGroup + 1
        cGroups <- c(cGroups, iGroup)
        cUnique <- c(cUnique, tmp)
      }
    }
    return(cGroups)
  }

  if (is.null(cGrouping)) {
    cGrouping <- getGroups(dfCovs)
  }


  ## Register to allow for paralell computing
  if (ncores>1) registerDoParallel(cores = ncores)
  numParCov <- PMXFrem::calcNumParCov(cbind(first = 0, dfParameters),numNonFREMThetas, numSkipOm)

  ## Calculate the parameters
  internalCalc<-function(k) {
    dfext  <- cbind(first = 0, dfParameters[k, ])
    thetas <- as.numeric(dfext[2:(numNonFREMThetas + 1)])
    dfrest <- data.frame()

    for (i in 1:nrow(dfCovs)) {
      currentNames <- names(dfCovs[i, ])[as.numeric(dfCovs[i, ]) != iMiss]

      if (any(!currentNames %in% covNames) && !quiet) {
        warning(paste0("Can't find some of the covariates: ", currentNames, " in the FREM model, perhaps they are structural covariates!"))
      }

      ## Calculate the ffemObj in case reference covariates have been specified
      if (!is.null(dfRefRow)) {

        indi <- min(i,nrow(dfRefRow))

        ffemObjRef <- PMXFrem::calcFFEM(
          numSkipOm        = numSkipOm,
          numNonFREMThetas = numNonFREMThetas,
          # noBaseThetas   = noBaseThetas,
          # noCovThetas    = noCovThetas,
          # noSigmas       = noSigmas,
          dfext            = dfext,
          covNames         = covNames,
          availCov         = names(dfRefRow[indi,])[as.numeric(dfRefRow[indi,]) != iMiss][names(dfRefRow[indi,])[as.numeric(dfRefRow[indi,]) != iMiss] %in% covNames],
          quiet            = quiet
          # noSkipOm       = noSkipOm,
          # noParCov       = noParCov
        )
      }

      ## Calculate the ffemObj for each set of parameters
      ffemObj <- PMXFrem::calcFFEM(
        numSkipOm        = numSkipOm,
        numNonFREMThetas = numNonFREMThetas,
        # noBaseThetas = noBaseThetas,
        # noCovThetas  = noCovThetas,
        # noSigmas     = noSigmas,
        dfext        = dfext,
        covNames     = covNames,
        availCov     = names(dfCovs[i, ])[as.numeric(dfCovs[i,]) != iMiss][names(dfCovs[i, ])[as.numeric(dfCovs[i,]) != iMiss] %in% covNames],
        quiet        = quiet
        # noSkipOm     = noSkipOm,
        # noParCov     = noParCov
      )


      coveffects      <- rep(0, length(parNames))
      coveffects_base <- rep(0, length(parNames))
      data47_jxrtp    <- dfCovs[i, ]

      ## Compute the ffem expressions
      for (j in 1:length(parNames)) {
        ffem_expr <- str_replace_all(ffemObj$Expr[j], pattern = "data\\$", replacement = "data47_jxrtp$")

        if (length(names(dfCovs[i, ])[as.numeric(dfCovs[i, ]) != iMiss]) != 0) {
          coveffects[j] <- as.numeric(eval(parse(text = ffem_expr)))
        }

        if (!is.null(dfRefRow)) {
          data47_jxrtp_ref<-dfRefRow[indi,]
          ffem_expr_base <- str_replace_all(ffemObjRef$Expr[j],pattern = "data\\$", replacement = "data47_jxrtp_ref$")
          coveffects_base[j] <- as.numeric(eval(parse(text = ffem_expr_base)))
        }
      }

      ## Send the expresssions to the functions in functionList
      n <- 1
      for (j in 1:length(functionList)) {

        val <- functionList[[j]](thetas, coveffects,dfrow = dfCovs[i, ], ...)

        ## Do it for the reference
        if (!is.null(dfRefRow)) {
          valbase <- functionList[[j]](basethetas = thetas,covthetas = coveffects_base, dfrow = dfRefRow[indi,], ...)
        } else {
          dfmissing <- dfCovs[1, ]
          dfmissing[, ] <- iMiss
          valbase <- functionList[[j]](basethetas = thetas,covthetas = rep(0, length(parNames)), dfrow = dfmissing, ...)
        }

        ## Do it for the parameters
        listcount <- length(val)
        for (l in 1:listcount) {
          dfrest <- bind_rows(dfrest, data.frame(
            ITER = k,
            COVS = i, NAME = functionListName[n], VALUE = val[[l]],
            VALUEBASE = valbase[[l]], stringsAsFactors = FALSE
          ))
          n <- n + 1
        }
      }
    }

    return(dfrest)
  }

  if (ncores>1) {
  dfres <- foreach( k = 1:nrow(dfParameters), .packages = cstrPackages,
                    .export = cstrExports, .verbose = !quiet, .combine = bind_rows
  ) %dopar% {
      internalCalc(k)
    }
  }  else {
    dfres<-data.frame()
    for (k in 1:nrow(dfParameters)) dfres<-bind_rows(dfres,internalCalc(k))
  }


  ## Assemble the return data.frame
  getCovNameString <- function(dfrow) {
    strName <- ""
    colnames <- names(dfrow)

    for (i in 1:ncol(dfrow)) {
      if (dfrow[1, i] != iMiss) {
        if (strName == "") {
          strName <- paste0(colnames[i], "=", dfrow[1,i])
        } else {
          strName <- paste0(strName, ", ", colnames[i],"=", dfrow[1, i])
        }
      }
    }

    if (strName == "") {
      strName <- "Ref cov"
    }
    return(strName)
  }

  dfret <- data.frame()
  for (i in 1:nrow(dfCovs)) {

    if (is.null(cdfCovsNames)) {
      covname <- getCovNameString(dfCovs[i, ])
    } else {
      covname <- cdfCovsNames[i]
    }

    group <- cGrouping[i]
    groupname<-group
    if (!is.null(groupnames)) groupname<-groupnames[i]

    for (j in 1:length(functionListName)) {
      dft         <- dfres[dfres$COVS == i & dfres$NAME == functionListName[j], ]
      quant       <- quantile(dft$VALUE,probs = probs, names = FALSE, na.rm = T)
      #Calculate the point value of the forest plot
      FUNCVAL=pointFunction(dft$VALUE)

      #Define the relative without parameter uncertainty
      dft$RELINTERNAL <- dft$VALUE/dft$VALUEBASE
      #Get quantile and pointvalue when uncertainty is not taken into account
      quantrel <- quantile(dft$RELINTERNAL,probs = probs, names = FALSE,na.rm = T)
      FUNCNOVAR=pointFunction(dft$RELINTERNAL)

      #Calculate reference value based one the pointFunction
      func_base<-pointFunction(dft$VALUEBASE)
      true_base   <- dft$VALUEBASE[dft$ITER == 1]
      dfrow       <- cbind(dfCovs[i, ],
        data.frame(GROUP = group,
        GROUPNAME = groupname,
        COVNUM = i, COVNAME = covname, PARAMETER = functionListName[j],
        REFFUNC = func_base, REFFINAL = true_base, POINT=FUNCVAL, POINT_NOVAR_REL_REFFUNC=FUNCNOVAR,
        POINT_REL_REFFUNC = FUNCVAL/func_base, POINT_REL_REFFINAL = FUNCVAL/true_base,
        COVEFF = !all(dft$RELINTERNAL==1)))

      for (k in 1:length(probs)) {
        dfp <- data.frame(X1 = 1)
        dfp[[paste0("Q", k)]] <- quant[k]
        dfp[[paste0("Q",k,"_REL_REFFUNC")]] <-  quant[k]/func_base
        dfp[[paste0("Q",k,"_REL_REFFINAL")]] <-  quant[k]/true_base
        dfrow <- cbind(dfrow, dfp[, 2:4])
      }
      for (k in 1:length(probs)) {
        dfp <- data.frame(X1 = 1)
        dfp[[paste0("Q",k,"_NOVAR_REL_REFFUNC")]] <- quantrel[k]
        dfrow <- cbind(dfrow, dfp[, 2])
        names(dfrow)[ncol(dfrow)] <- paste0("Q", k,"_NOVAR_REL_REFFUNC")
      }
      dfret <- rbind(dfret, dfrow)
    }
  }

  if (ncores>1) stopImplicitCluster()

  ## Add a column with YES/NO depending on if refRow was provided or if it was set to the default NULL
  dfret <- dfret %>% mutate(REFROW = ifelse(is.null(dfRefRow),"NO","YES"))

  ## Make sure GROUPNAME, COVNAME, PARAMETER are factors
  dfret$GROUPNAME <- factor(dfret$GROUPNAME,levels=unique(dfret$GROUPNAME))
  dfret$COVNAME   <- factor(dfret$COVNAME,levels=unique(dfret$COVNAME))
  dfret$PARAMETER <- factor(dfret$PARAMETER,levels=unique(dfret$PARAMETER))

  return(dfret)
}


