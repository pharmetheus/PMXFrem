#' Calculate conditional ETAs (ETA prim)
#'
#' @description Collects the ETAs (parameter and covariate) from a FREM model and
#'   computes the ETA_prims.
#'
#' @inheritParams createFFEMdata
#' @param FFEMData An FFEMData object as obtained with the function `createFFEMdata`. If NULL,
#'   the object will be created internally, requiring `dataFile`, `parNames`, and other
#'   arguments for `createFFEMdata` to be provided.
#' @param covmodel A character string indicating if the covariate models were implemented
#'   linearly (additatively) in the frem model or not. Default is "linear".
#' @param ... Additional arguments passed on to `createFFEMdata` when `FFEMData` is `NULL`.
#'
#' @details The function collects the ETAs from the output of a FREM model, both for the
#'   parameters as well as the covariates. The corresponding ETA_prims for the parameter
#'   ETAs are computed by extracting the corresponding individual covariate coefficient
#'   provided in the `FFEMData` object.
#'
#' @seealso [createFFEMdata()] for information about creating the FFEMData object
#'
#' @return
#' A `data.frame` with one row per subject, containing the individual parameter estimates.
#' The data frame includes the following columns:
#' \itemize{
#'   \item **ID**: The subject identifier.
#'   \item **ETA*_PRIM**: One column for each parameter's conditional ETA (ETA_prim). This is the
#'   random effect adjusted for the individual's specific covariate effects.
#'   \item **ETA***: One column for each parameter's raw ETA as reported in the FREM model output.
#'   \item **Covariate Columns**: Columns for each covariate, containing the individual covariate
#'   estimates. If `covmodel = "linear"`, these values are on the original covariate scale;
#'   otherwise, they are on the ETA scale.
#' }
#'
#' @export
#'
#' @examples
#'
#' # 1. Define the path to the model files included with the package
#' model_dir <- system.file("extdata/SimNeb/", package = "PMXFrem")
#'
#' # 2. Load and prepare the dataset also included with the package
#' data_path <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv",
#'                          package = "PMXFrem")
#'
#' my_data <- read.csv(data_path)
#'
#' # In this example dataset, BLQ=1 rows are excluded
#' my_data <- my_data[my_data$BLQ != 1, ]
#'
#' # 3. Call calcEtas, providing the data directly
#' # The function will create the FFEMData object internally.
#' individual_etas <- calcEtas(
#'   modName          = "run31",
#'   modDevDir        = model_dir,
#'   numNonFREMThetas = 7,
#'   numSkipOm        = 2,
#'   dataFile         = my_data,
#'   parNames         = c("CL", "V", "MAT")
#' )
#'
#' # 4. Display the first few rows of the resulting data frame
#' # The output contains subject IDs, ETAs, ETA_PRIMs, and covariate values.
#' head(individual_etas)
#'
calcEtas <- function(
    runno            = NULL,
    numNonFREMThetas,
    modName          = NULL,
    numSkipOm        = 0,
    idvar            = "ID",
    modDevDir        = NULL,
    FFEMData         = NULL,
    covmodel         = "linear",
    # ---- NEW: Add parameters needed for internal createFFEMdata call ----
    dataFile         = NULL,
    parNames         = NULL,
    quiet            = TRUE, # Default to TRUE for internal calls
    ...
) {
  
  # ---- NEW: Conditional block to create FFEMData if not provided ----
  if (is.null(FFEMData)) {
    # Validate that essential arguments are present
    if (is.null(dataFile) || is.null(parNames)) {
      stop("If `FFEMData` is NULL, you must provide `dataFile` and `parNames`.", call. = FALSE)
    }
    
    message("`FFEMData` object not provided. Creating it internally...")
    
    # Call createFFEMdata to generate the object
    FFEMData <- createFFEMdata(
      runno            = runno,
      modName          = modName,
      modDevDir        = modDevDir,
      numNonFREMThetas = numNonFREMThetas,
      numSkipOm        = numSkipOm,
      dataFile         = dataFile,
      parNames         = parNames,
      newDataFile      = NULL, # Ensure it returns a data frame, not writes to disk
      idvar            = idvar,
      quiet            = quiet,
      ...
    )
  }
  # ---- END OF NEW BLOCK ----
  
  ## Get the filenames and data to work with (This part remains the same)
  fileNames <- getFileNames(runno = runno, modName = modName, modDevDir = modDevDir, ...)
  modFile   <- fileNames$mod
  extFile   <- fileNames$ext
  phiFile   <- fileNames$phi
  
  dfphi <- getPhi(phiFile)
  dfExt <- getExt(extFile)
  
  # The rest of the function continues exactly as before,
  # now guaranteed to have a valid FFEMData object.
  dfone <- FFEMData$newData[!duplicated(FFEMData$newData[[idvar]]), c(idvar, FFEMData$indCovEff)]
  
  if (nrow(dfphi) != nrow(dfone)) stop("Number of unique individuals in the dataset and phi file needs to be the same")
  
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
  retDf <- cbind(ID = dfphi[, 2], etaprim, etafrem, covariates)
  names(retDf) <- gsub("\\.", "", names(retDf))
  
  return(retDf)
}