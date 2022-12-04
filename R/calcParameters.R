#' calcParameters
#'
#' @description Collects the ETAs (parameter and covariate) from a FREM model and computes the ETA_prims.
#'
#' @inheritParams createFFEMdata
#' @param FFEMData An FFEMData object as obtained with the function `createFFEMData`.
#' @param covmodel A character string indicating if the covaite models were implemented linearly (additatively) in eth frem model or not. Default is "linear"
#'
#' @details The function collects the ETAs from the output of a FREM model, both for the parameters as well as the covariates. The corresponding ETA_prims for the parameter ETAs are computed
#' by extracting the corresponding individual covariate coefficient provided in the createFFEMdata object, which is a mandatory argument.
#'
#' If argument `covmodel` is `linear` the covariate ETAs will be added to the estimates of the mean covariate effects from the FREM model to generate individual covariate
#' values on the original covariate scale. If argument `covmodel` is not `linear` the covariate ETAs will be reported on the ETA scale. The names of the covariate ETAs will be set to
#' `getCovNames(modFile = modFile)$covNames` regardless of the value of `covmodel`.
#'
#' @seealso [createFFEMData()] for information about creating the FFEMData object
#' @return A data frame with the same number of rows as subjects in the data file used in the base model, with columns ID, ETA1-(number of etas in the base model),
#' ETA1-(number of etas in the base model)_PRIM and columns for the covariate etas (see above)
#' @export
#'
#' @examples
#' \dontrun{
#' ind_params <- calcParmeters(modName          = "run31",
#'                             modDevDir        = ".",
#'                             numSkipOm        = 2,
#'                             numNonFREMThetas = 7,
#'                             FFEMData         = FFEMDataObject)
#' }
calcParameters <- function(
    runno          = NULL,
    numNonFREMThetas,
    modName       = NULL,
    numSkipOm     = 0,
    idvar         = "ID",
    modDevDir     = NULL,
    FFEMData      = NULL,
    covmodel      = "linear",
    ...) {

  ## Get the filenames and data to work with
  fileNames <- getFileNames(runno=runno,modName=modName,modDevDir=modDevDir,...)
  modFile   <- fileNames$mod
  extFile   <- fileNames$ext
  phiFile   <- fileNames$phi

  dfphi <- getPhi(phiFile)
  dfExt <- getExt(extFile)

  dfone <- FFEMData$newData[!duplicated(FFEMData$newData[[idvar]]), c(idvar, FFEMData$indCovEf)]

  if (nrow(dfphi) != nrow(dfone)) error("Number of unique individuals in the dataset and phi file needs to be the same")

  ## Compute eta_prim
  etafrem <- dfphi[, 3:(2 + numSkipOm + nrow(FFEMData$Omega))]
  etaprim <- etafrem
  for (i in 1:length(FFEMData$indCovEff)) {
    etaprim[, (i + numSkipOm)] <- etafrem[, (i + numSkipOm)] - dfone[, i + 1]
  }
  names(etaprim) <- paste0(names(etaprim), "_PRIM")

  ## Get the covariate thetas
  if (nrow(dfExt) > 1) dfExt <- dfExt[dfExt$ITERATION == -1000000000, ]

  numFREMThetas <- length(grep("THETA", names(dfExt))) - numNonFREMThetas
  df_thm        <- as.numeric(dfExt[, (numNonFREMThetas + 2):(numNonFREMThetas + 1 + numFREMThetas)])

  ## Get the covariate ETAs
  covariates        <- dfphi[, (3 + numSkipOm + nrow(FFEMData$Omega)):((2 + numSkipOm + nrow(FFEMData$Omega)) + numFREMThetas)]
  names(covariates) <- getCovNames(modFile = modFile)$covNames

  ## Compute covariates on the original scale if the covmodel is linear
  if (covmodel == "linear") {
    for (i in 1:length(df_thm)) {
      covariates[, i] <- covariates[, i] + df_thm[i]
    }
  }

  ## Assign informative names
  retDf <- cbind(ID=dfphi[,2],etaprim,etafrem,covariates)
  names(retDf) <- gsub("\\.","",names(retDf))

  return(retDf)

}


