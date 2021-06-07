#' getIndividualParameters 
#'
#' @description Compute the actual etas from a FREM model once the covariates have been taken into account, i.e. eta' (etaprim)
#'
#' @inheritParams getExt
#' @param dfext A data frame with the information from the NONMEM .ext file.
#' @param thePhiFile A data frame with the individual etas to process. The first column needs to be the ID number. If set to NULL the information will be read from the NONMEM .phi file.
#'
#' @return A data frame with ID number and ETAPRIMs, i.e. the eta values conditioned on the covariates included in the FREM calculations.
#' @export
#' @examples
#' \dontrun{
#' patab <- fread(file.path(modDevDir, paste0("xptab", finalRun)),
#' data.table = FALSE, check.names = TRUE)  %>%
#'   select(ID, matches("^ETA[1-9]"), matches("^ET[1-9][0-9]")) %>%
#'   distinct(ID, .keep_all = TRUE)
#' 
#' etaprimDf <- getIndividualParameters(
#'   runno        = finalRun,
#'   modDevDir    = modDevDir,
#'   noBaseThetas = 9,
#'   noCovThetas  = 14,
#'   noSigmas     = 2,
#'   noParCov     = 4,
#'   noSkipOm     = 2,
#'   thePhiFile   = patab
#' )
#' }
getIndividualParameters <- function(runno, modName = NULL, noBaseThetas, noSigmas, noSkipOm = 0, 
                                    noParCov = noBaseThetas, parNames = c("BASE", "PLMAX", "HLKON", "HLKOFF", "BASSL"),
                                    dataFile, newDataFile = paste("vpcData", runno, ".csv", sep = ""), availCov = NULL, idvar = "ID",
                                    modDevDir = NULL, quiet = TRUE, cores = 1, dfext = NULL, thePhiFile = NULL) {
  if (is.null(modDevDir)) {
    if (is.null(modName)) {
      modFile <- paste0("run", runno, ".mod")
      extFile <- paste0("run", runno, ".ext")
      if (is.null(phiFile)) phiFile <- paste0("run", runno, ".phi")
    } else {
      modFile <- paste0(modName, ".mod")
      extFile <- paste0(modName, ".ext")
      phiFile <- paste0(modName, ".phi")
    }
  } else {
    if (is.null(modName)) {
      modFile <- paste0(modDevDir, "/run", runno, ".mod")
      extFile <- paste0(modDevDir, "/run", runno, ".ext")
      phiFile <- paste0(modDevDir, "/run", runno, ".phi")
    } else {
      modFile <- paste0(modDevDir, "/", modName, ".mod")
      extFile <- paste0(modDevDir, "/", modName, ".ext")
      phiFile <- paste0(modDevDir, "/", modName, ".phi")
    }
  }

  covNames <- getCovNames(modFile)$covNames
  fremCovs <- getCovNames(modFile)$polyCatCovsa
  orgCovs <- getCovNames(modFile)$orgCovNames

  # It is much faster to send in extdf than to create it fo reach ID.
  # Only read it from file if it isn't passed via dfext
  if (is.null(dfext)) {
    theExtFile <- getExt(extFile)
  } else {
    theExtFile <- dfext
  }

  if (is.null(thePhiFile)) {
    thePhiFile <- getPhi(phiFile)

    # Calc num etas in phi-file
    numetas <- -3 / 2 + sqrt((3 / 2)^2 + 2 * (ncol(thePhiFile) - 3))
    ids <- thePhiFile$ID
    thePhiFile <- thePhiFile[, -c(1, 2, ncol(thePhiFile))]
    thePhiFile <- thePhiFile[, 1:numetas]
  } else {
    ids <- thePhiFile$ID
    thePhiFile <- thePhiFile[, -1]
  }

  ## Run this to get the omega matrix to use in the vpc
  if (is.null(availCov)) availCov <- covNames

  ## Run through all rows in the phi-file and calculate eta'
  dfetas <- data.frame(thePhiFile)
  for (i in 1:nrow(thePhiFile)) {
    tmp <- calcFFEM(
      noBaseThetas = noBaseThetas,
      noCovThetas = length(covNames),
      noSigmas = noSigmas,
      noSkipOm = noSkipOm,
      noParCov = noParCov,
      dfext = theExtFile,
      parNames = parNames,
      covNames = covNames,
      availCov = availCov,
      quiet = quiet,
      fremETA = as.numeric(thePhiFile[i, ])
    )
    if (i == 1) {
      etap <- rep(0, length(tmp$Eta_prim))
      names(etap) <- paste0("ETAPRIM", 1:length(etap))
      dfetas[, names(etap)] <- NA
    }
    dfetas[i, (ncol(dfetas) - length(tmp$Eta_prim) + 1):ncol(dfetas)] <- tmp$Eta_prim
  }

  dfetas2 <- bind_cols(
    ID = ids,
    dfetas %>% select(starts_with("ETAPRIM"))
  )
  return(dfetas2)
}
