#' Resolve FREM covariate names from FFEM covariates
#' @keywords internal
#' @noRd
.get_frem_cov_names <- function(currNames, fremCovs) {
  covrow <- character()
  ffemCovs <- stringr::str_replace(fremCovs, "_[0-9]*", "")
  
  for (cov in c(currNames, fremCovs)) {
    myCov <- stringr::str_replace(cov, "_[0-9]*", "")
    index <- which(cov == fremCovs)
    # If a FREM binarized covariate
    if (length(index) > 0) {
      if (cov %in% currNames) covrow <- c(covrow, cov)
    } else {
      index <- which(myCov == ffemCovs)
      # If not a FREM binarized covariate
      if (length(index) == 0) {
        covrow <- c(covrow, cov)
      } else {
        covrow <- c(covrow, fremCovs[index])
      }
    }
  }
  return(unique(covrow))
}

#' Delta rule based derivation of explained variability
#' @keywords internal
#' @noRd
.apply_delta_rule <- function(params, covmatrix, transform_fun, ...) {
  # Options to grad function
  ma <- list(eps = 1e-4, d = 0.0001, zero.tol = sqrt(.Machine$double.eps / 7e-7), r = 4, v = 2, show.details = FALSE)
  
  param_new <- transform_fun(params, ...) # Calculate the transformation
  new_var <- rep(0, length(param_new))    # Initialize the variance
  
  for (k in seq_along(param_new)) {
    tf1 <- function(params, ...) transform_fun(params, ...)[[k]]
    param_deriv <- numDeriv::grad(tf1, params, method = "Richardson", method.args = ma, ...)
    
    for (i in seq_along(params)) {
      for (j in seq_along(params)) {
        new_var[k] <- new_var[k] + param_deriv[i] * param_deriv[j] * covmatrix[i, j]
      }
    }
  }
  return(c(param_new, new_var)) 
}

#' Calculate First-Order (Delta Rule) Explained Variance
#' @keywords internal
#' @noRd
.calc_fo_variance <- function(dfCovs, functionList, functionListName, cstrCovariates,
                              thetas, dfext, numNonFREMThetas, numFREMThetas, numSigmas,
                              numParCov, numSkipOm, parNames, covNames, allCov, fremCovs, quiet, missVal,...) {
  
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
  
  res_list <- list()
  res_idx <- 1
  m <- 1 
  
  for (j in seq_along(functionList)) {
    TOTVAR <- .apply_delta_rule(
      params = rep(0, length(diag(ffemObjAllNoCov$FullVars))), covmatrix = ffemObjAllNoCov$FullVars, transform_fun = parf, basethetas = thetas,
      covthetas = rep(0, length(parNames)), dfrow = dfCovs[1, ], myfunc = functionList[[j]], ...
    )
    TOTVAR <- TOTVAR[(length(TOTVAR)/2 + 1):length(TOTVAR)]
    
    TOTCOVVAR <- .apply_delta_rule(
      params = rep(0, length(diag(ffemObjAllCov$FullVars))), covmatrix = ffemObjAllCov$FullVars, transform_fun = parf, basethetas = thetas,
      covthetas = rep(0, length(parNames)), dfrow = dfCovs[1, ], myfunc = functionList[[j]], ...
    )
    TOTCOVVAR <- TOTCOVVAR[(length(TOTCOVVAR)/2 + 1):length(TOTCOVVAR)]
    
    for (i in seq_len(nrow(dfCovs))) {
      currentNames <- names(dfCovs[i, ])[as.numeric(dfCovs[i, ]) != missVal]
      tmpcovs <- .get_frem_cov_names(currentNames, fremCovs)
      
      ffemObj <- calcFFEM(
        numNonFREMThetas = numNonFREMThetas, numFREMThetas = numFREMThetas, numSigmas = numSigmas, dfext = dfext, covNames = covNames,
        availCov = tmpcovs, quiet = quiet, numParCov = numParCov, numSkipOm = numSkipOm
      )
      
      COVVAR <- .apply_delta_rule(
        params = rep(0, length(diag(ffemObj$FullVars))), covmatrix = ffemObj$FullVars, transform_fun = parf, basethetas = thetas,
        covthetas = rep(0, length(parNames)), dfrow = dfCovs[1, ], myfunc = functionList[[j]], ...
      )
      COVVAR <- COVVAR[(length(COVVAR)/2 + 1):length(COVVAR)]
      
      res_list[[res_idx]] <- data.frame(
        COVNUM    = i, 
        COVNAME   = cstrCovariates[i],
        PARAMETER = functionListName[m:(m + length(TOTVAR) - 1)],
        TOTVAR    = TOTVAR,
        TOTCOVVAR = TOTVAR - TOTCOVVAR,
        COVVAR    = TOTVAR - COVVAR
      )
      res_idx <- res_idx + 1
    }
    m <- m + length(TOTVAR)
  }
  
  dfres <- dplyr::bind_rows(res_list)
  return(dfres[order(match(dfres$PARAMETER, functionListName)), ])
}

#' Calculate Empirical Explained Variance (Types 1, 2, 3)
#' @keywords internal
#' @noRd
.calc_empirical_variance <- function(type, data, dfCovs, dfext, strID, runno, modName, modDevDir,
                                     cstrCovariates, functionList, functionListName, numNonFREMThetas,
                                     numFREMThetas, numSigmas, numParCov, parNames, numSkipOm, allCov,
                                     etas, quiet, ncores, cstrPackages, cstrExports, numETASamples,
                                     seed, thetas, covNames, fremCovs, orgCovs, missVal,...) {
  
  if (type == 2 || type == 3) { 
    ETAsamples <- matrix(stats::rnorm((numParCov + numSkipOm) * numETASamples), nrow = (numParCov + numSkipOm), ncol = numETASamples)
  }
  
  for (cov in fremCovs) {
    myCov <- stringr::str_replace(cov, "_[0-9]*", "")
    myCovNum <- stringr::str_replace(cov, paste0(myCov, "_"), "")
    if (!myCov %in% names(data)) {
      stop(paste0("Can't find ", myCov, " in the dataset, exiting!"))
    }
    data[[cov]] <- ifelse(data[[myCov]] == myCovNum, 1, 0)
  }
  
  dataI <- data[!duplicated(data[[strID]]), ] 
  
  if (type == 1 && (nrow(etas) != nrow(dataI))) {
    stop("The number of etas should be the same as the number of subjects in the data set.")
  }
  
  if (ncores > 1) {
    doParallel::registerDoParallel(cores = ncores)
    on.exit(doParallel::stopImplicitCluster(), add = TRUE)
  }
  
  mapFun <- function(data_row, orgCovs) {
    for (cov in orgCovs) {
      if (data_row[[cov]][1] == missVal && sum(grepl(cov, names(data_row))) > 1) {
        data_row[1, grepl(cov, names(data_row))] <- missVal
      }
    }
    return(data_row)
  }
  
  if (ncores > 1) {
    dataI <- foreach::foreach(k = seq_len(nrow(dataI)), .packages = cstrPackages, .export = c(ls(environment()), cstrExports)) %dopar% {
      mapFun(data_row = dataI[k, , drop = FALSE], orgCovs = orgCovs)
    }
    dataI <- as.data.frame(data.table::rbindlist(dataI))
  } else {
    dataI_list <- vector("list", nrow(dataI))
    for (k in seq_len(nrow(dataI))) {
      dataI_list[[k]] <- mapFun(data_row = dataI[k, , drop = FALSE], orgCovs = orgCovs)
    }
    dataI <- dplyr::bind_rows(dataI_list)
  }
  dataI$jxrtp47 <- missVal 
  
  dfrest_list <- vector("list", nrow(dfCovs))
  
  for (i in seq_len(nrow(dfCovs))) {
    currentNames <- names(dfCovs[i, , drop = FALSE])[as.numeric(dfCovs[i, ]) != missVal]
    strCovsRow <- currentNames
    
    if (type == 3 || type == 2) { 
      ffemObjAllNoCov <- calcFFEM(
        numNonFREMThetas = numNonFREMThetas, numFREMThetas = numFREMThetas, numSigmas = numSigmas, dfext = dfext, covNames = covNames,
        availCov = NULL, quiet = quiet, numParCov = numParCov, numSkipOm = numSkipOm
      )
      Chol <- chol(ffemObjAllNoCov$FullVars) 
      etasamples <- t(ETAsamples) %*% Chol 
    }
    
    internalCalc <- function(k) { 
      tmpcovs <- .get_frem_cov_names(currentNames, fremCovs)
      datatmp <- dataI[k, covNames, drop = FALSE] 
      avcov <- names(datatmp)[which(datatmp != missVal)] 
      
      eval_env <- list(data = datatmp)
      coveffects <- rep(0, length(parNames))
      
      ffemObj <- calcFFEM(
        numNonFREMThetas = numNonFREMThetas, numFREMThetas = numFREMThetas, numSigmas = numSigmas, dfext = dfext, covNames = covNames,
        availCov = avcov[avcov %in% tmpcovs], quiet = quiet, numParCov = numParCov, numSkipOm = numSkipOm
      )
      
      for (j in seq_along(parNames)) {
        if (length(names(dfCovs[i, , drop = FALSE])[as.numeric(dfCovs[i, , drop = FALSE]) != missVal]) != 0) {
          coveffects[j] <- as.numeric(eval(parse(text = ffemObj$Expr[j]), envir = eval_env))
        }
      }
      
      res_list <- list()
      res_idx <- 1
      
      if (i == 1) { 
        ffemObjAll <- calcFFEM(
          numNonFREMThetas = numNonFREMThetas, numFREMThetas = numFREMThetas, numSigmas = numSigmas, dfext = dfext, covNames = covNames,
          availCov = avcov[avcov %in% allCov], quiet = quiet, numParCov = numParCov, numSkipOm = numSkipOm
        )
        coveffectsAll <- rep(0, length(parNames))
        
        for (j in seq_along(parNames)) {
          if (length(avcov) != 0) {
            coveffectsAll[j] <- as.numeric(eval(parse(text = ffemObjAll$Expr[j]), envir = eval_env))
          }
        }
        
        n <- 1
        for (j in seq_along(functionList)) {
          if (type == 2) {
            val <- 0
            tmpval <- 0
            for (m in seq_len(numETASamples)) { 
              val <- functionList[[j]](basethetas = thetas, covthetas = rep(0, length(coveffectsAll)), dfrow = dataI[k, ], etas = etasamples[m, ], ...) 
              if (m == 1) tmpval <- matrix(0, ncol = numETASamples, nrow = length(val))
              tmpval[, m] <- unlist(val)
            }
            for (m in seq_len(nrow(tmpval))) val[m] <- stats::var(tmpval[m, ]) 
          }
          if (type == 1) {
            val <- functionList[[j]](basethetas = thetas, covthetas = rep(0, length(coveffectsAll)), dfrow = dataI[k, ], etas = as.numeric(etas[k, ]), ...)
          }
          if (type == 3 && k == 1) { 
            tmpval <- 0
            val <- 0
            for (m in seq_len(numETASamples)) { 
              val <- functionList[[j]](basethetas = thetas, covthetas = rep(0, length(coveffectsAll)), dfrow = dataI[k, ], etas = etasamples[m, ], m, ...)
              if (m == 1) tmpval <- matrix(0, ncol = numETASamples, nrow = length(val))
              tmpval[, m] <- unlist(val)
            }
            for (m in seq_len(nrow(tmpval))) val[m] <- stats::var(tmpval[m, ]) 
          }
          
          valeta0 <- functionList[[j]](basethetas = thetas, covthetas = coveffectsAll, dfrow = dataI[k, ], etas = rep(0, 3 * length(thetas)), ...) 
          listcount <- length(valeta0)
          
          for (l in seq_len(listcount)) {
            if (type != 3 || (type == 3 && k == 1)) {
              res_list[[res_idx]] <- data.frame(ITER = k, COVS = 0, NAME = as.character(functionListName[n]), VALUE = val[[l]])
              res_idx <- res_idx + 1
            }
            res_list[[res_idx]] <- data.frame(ITER = k, COVS = -1, NAME = as.character(functionListName[n]), VALUE = valeta0[[l]])
            res_idx <- res_idx + 1
            n <- n + 1
          }
        }
      }
      
      n <- 1
      for (j in seq_along(functionList)) {
        datatmp_eval <- dataI[k, c(tmpcovs, "jxrtp47"), drop = FALSE]
        val <- functionList[[j]](basethetas = thetas, covthetas = coveffects, dfrow = datatmp_eval, etas = rep(0, 3 * length(thetas)), ...) 
        listcount <- length(val)
        
        for (l in seq_len(listcount)) {
          res_list[[res_idx]] <- data.frame(ITER = k, COVS = i, NAME = as.character(functionListName[n]), VALUE = val[[l]])
          res_idx <- res_idx + 1
          n <- n + 1
        }
      }
      return(dplyr::bind_rows(res_list))
    }
    
    if (ncores > 1) {
      # Use full environment export to bypass static analysis closures
      dftmp1 <- foreach::foreach(k = seq_len(nrow(dataI)), .packages = cstrPackages, .export = c(ls(environment()), cstrExports)) %dopar% {
        internalCalc(k)
      }
      dftmp1 <- dplyr::bind_rows(dftmp1)
    } else {
      dftmp1_list <- vector("list", nrow(dataI))
      for (k in seq_len(nrow(dataI))) dftmp1_list[[k]] <- internalCalc(k)
      dftmp1 <- dplyr::bind_rows(dftmp1_list)
    }
    dfrest_list[[i]] <- dftmp1
  } 
  
  dfrest <- dplyr::bind_rows(dfrest_list)
  dfres_final_list <- list()
  res_idx <- 1
  
  for (j in seq_along(functionListName)) {
    if (type == 3) {
      TOTVAR <- subset(dfrest, NAME == as.character(functionListName[j]) & COVS == 0)$VALUE 
    } else if (type == 2) {
      TOTVAR <- mean(subset(dfrest, NAME == as.character(functionListName[j]) & COVS == 0)$VALUE) 
    } else {
      TOTVAR <- stats::var(subset(dfrest, NAME == as.character(functionListName[j]) & COVS == 0)$VALUE) 
    }
    
    TOTCOVVAR <- stats::var(subset(dfrest, NAME == as.character(functionListName[j]) & COVS == -1)$VALUE) 
    
    for (i in seq_len(nrow(dfCovs))) {
      dfres_final_list[[res_idx]] <- data.frame(
        COVNUM = i, COVNAME = cstrCovariates[i], PARAMETER = functionListName[j],
        TOTVAR = TOTVAR, TOTCOVVAR = TOTCOVVAR,
        COVVAR = stats::var(subset(dfrest, NAME == as.character(functionListName[j]) & COVS == i)$VALUE)
      )
      res_idx <- res_idx + 1
    }
  }
  return(dplyr::bind_rows(dfres_final_list))
}
