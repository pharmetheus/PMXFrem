#' Remove Covariates from a FREM Model State
#'
#' This function takes a model state object and removes specified covariates.
#' It handles the removal of data from the FREM dataset, the re-numbering of
#' remaining FREMTYPEs, and the adjustment of corresponding parameter vectors
#' (THETA, THETAFIX) and the OMEGA matrix.
#'
#' @param currentState A list object representing the model's current state.
#'   It must contain: dfFREM, covnames, theta, omegaMatrix, thetaFix, numOmega,
#'   numTheta, numNonFREMThetas, numParCov, numSkipOm, and iFremTypeIncrease.
#' @param cstrRemoveCov A character vector of covariate names to remove. If a
#'   base name for a categorical covariate is provided (e.g., "SITE"), all its
#'   dichotomized versions (e.g., "SITE_1", "SITE_2") will be removed.
#' @param quiet A logical flag to suppress printed messages.
#'
#' @return An updated list object with the same structure as `currentState`,
#'   reflecting the state after covariate removal.
#'
removeFremCovariates <- function(currentState, cstrRemoveCov, quiet) {
  
  # A small helper function for printing, matching the parent function's style.
  printq <- function(str, quiet) {
    if (!quiet) print(str)
  }
  
  # Unpack the state for easier access
  dfFREM <- currentState$dfFREM
  covnames <- currentState$covnames
  THETA <- currentState$theta
  OM <- currentState$omegaMatrix
  THETAFIX <- currentState$thetaFix
  iNumOM <- currentState$numOmega
  iNumTHETA <- currentState$numTheta
  noBaseThetas <- currentState$numNonFREMThetas
  numParCov <- currentState$numParCov
  numSkipOm <- currentState$numSkipOm
  iFremTypeIncrease <- currentState$iFremTypeIncrease
  
  # If there's nothing to remove, return the state unchanged
  if (is.null(cstrRemoveCov)) {
    return(currentState)
  }
  
  # --- This is the logic block moved from updateFREMmodel.R ---
  cOrgCovsToRemove <- c()
  cCovNamesToRemove <- c()
  
  for (i in 1:length(cstrRemoveCov)) {
    for (j in 1:length(covnames$covNames)) {
      strCov <- covnames$covNames[j]
      strCovClean <- stringr::str_replace(strCov, "_.*", "")
      
      if (cstrRemoveCov[i] == strCov) {
        if (cstrRemoveCov[i] %in% covnames$orgCovNames) {
          printq(paste0("Found a continuous covariate to remove ", strCov, " with FREMTYPE=", j * iFremTypeIncrease), quiet = quiet)
        } else {
          printq(paste0("Found a categorical covariate to remove ", strCov, " with FREMTYPE=", j * iFremTypeIncrease), quiet = quiet)
        }
        cOrgCovsToRemove <- c(cOrgCovsToRemove, strCov)
        cCovNamesToRemove <- c(cCovNamesToRemove, strCov)
        
      } else {
        if (strCovClean == cstrRemoveCov[i]) {
          iCategory <- gsub(".+_([0-9]+)", "\\1", strCov)
          printq(paste0("Found a categorical covariate to remove ", strCovClean, " with category ", iCategory, " and FREMTYPE=", j * iFremTypeIncrease), quiet = quiet)
          cOrgCovsToRemove <- c(cOrgCovsToRemove, strCovClean)
          cCovNamesToRemove <- c(cCovNamesToRemove, strCov)
        }
      }
    }
  }
  
  iCovariateIndexToRemove <- which(covnames$covNames %in% cCovNamesToRemove)
  
  printq(paste0("Removing all unwanted fremtypes from FREM dataset, in total ", nrow(dfFREM[(dfFREM[["FREMTYPE"]] %in% (iCovariateIndexToRemove * iFremTypeIncrease)), ]), " observations"), quiet = quiet)
  dfFREM <- dfFREM[!(dfFREM[["FREMTYPE"]] %in% (iCovariateIndexToRemove * iFremTypeIncrease)), ]
  
  printq(paste0("Recoding all remaining FREMTYPEs in FREM dataset..."), quiet = quiet)
  remainingCovs <- covnames$covNames[which(!(covnames$covNames %in% cCovNamesToRemove))]
  iuniqueFREMTYPEs <- sort(unique(dfFREM[["FREMTYPE"]][dfFREM[["FREMTYPE"]] >= iFremTypeIncrease]))
  
  for (i in 1:length(iuniqueFREMTYPEs)) {
    if (iuniqueFREMTYPEs[i] != i * iFremTypeIncrease) {
      printq(paste0("Updating ", nrow(dfFREM[dfFREM[["FREMTYPE"]] == iuniqueFREMTYPEs[i], ]), " rows with FREMTYPE=", iuniqueFREMTYPEs[i], " (", remainingCovs[i], ") to FREMTYPE=", i * iFremTypeIncrease), quiet = quiet)
      dfFREM[dfFREM[["FREMTYPE"]] == iuniqueFREMTYPEs[i], "FREMTYPE"] <- i * iFremTypeIncrease
    }
  }
  
  printq(paste0("Recoding FREMTYPES, done!"), quiet = quiet)
  
  THETA <- THETA[-(iCovariateIndexToRemove + noBaseThetas)]
  OM <- OM[-(iCovariateIndexToRemove + numParCov + numSkipOm), -(iCovariateIndexToRemove + numParCov + numSkipOm)]
  THETAFIX <- THETAFIX[-(iCovariateIndexToRemove + noBaseThetas)]
  iNumOM <- iNumOM - length(iCovariateIndexToRemove)
  iNumTHETA <- iNumTHETA - length(iCovariateIndexToRemove)
  
  covnames$covNames <- covnames$covNames[!(covnames$covNames %in% cCovNamesToRemove)]
  covnames$orgCovNames <- covnames$orgCovNames[!(covnames$orgCovNames %in% unique(cOrgCovsToRemove))]
  covnames$polyCatCovs <- covnames$polyCatCovs[covnames$polyCatCovs %in% covnames$covNames]
  # --- End of moved logic block ---
  
  # Pack the modified state variables back into a list to return
  updatedState <- list(
    dfFREM = dfFREM,
    covnames = covnames,
    theta = THETA,
    omegaMatrix = OM,
    thetaFix = THETAFIX,
    numOmega = iNumOM,
    numTheta = iNumTHETA,
    numNonFREMThetas = noBaseThetas,
    numParCov = numParCov,
    numSkipOm = numSkipOm,
    iFremTypeIncrease = iFremTypeIncrease
  )
  
  return(updatedState)
}