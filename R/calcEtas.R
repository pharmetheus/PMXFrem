### FILE: calcEtas.R ###

#' Calculate conditional ETAs (ETA prim)
#'
#' @description Collects the ETAs (parameter and covariate) from a FREM model and
#'   computes the ETA_prims. Optionally appends the true Empirical Bayes Estimates 
#'   (EBVs) from a specified FFEM run for diagnostic plotting.
#'
#' @inheritParams createFFEMdata
#' @param FFEMData An FFEMData object as obtained with the function `createFFEMdata`. If NULL,
#'   the object will be created internally, requiring `dataFile`, `parNames`, and other
#'   arguments for `createFFEMdata` to be provided.
#' @param covmodel A character string indicating if the covariate models were implemented
#'   linearly (additatively) in the frem model or not. Default is "linear".
#' @param ffemModName A character string specifying the model name of the FFEM run 
#'   (typically a MAXEVAL=0 run). If provided, the true posterior EBVs will be extracted 
#'   from its .phi file and appended to the output data frame. Default is `NULL`.
#' @param ... Additional arguments passed on to `createFFEMdata` when `FFEMData` is `NULL`.
#'
#' @details The function collects the ETAs from the output of a FREM model, both for the
#'   parameters as well as the covariates. The corresponding ETA_prims for the parameter
#'   ETAs are computed by extracting the corresponding individual covariate coefficient
#'   provided in the `FFEMData` object. If `ffemModName` is specified, the true structural 
#'   EBVs are extracted from the FFEM model and merged by `ID`.
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
#'   \item **EBV_ETA***: (If `ffemModName` is provided) The true Empirical Bayes Estimate from the FFEM model.
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
#' # We also specify ffemModName to extract true posterior EBVs for plotting.
#' individual_etas <- calcEtas(
#'   modName          = "run31",
#'   modDevDir        = model_dir,
#'   numNonFREMThetas = 7,
#'   numSkipOm        = 2,
#'   dataFile         = my_data,
#'   parNames         = c("CL", "V", "MAT"),
#'   ffemModName      = "run31max0"
#' )
#'
#' # 4. Display the first few rows of the resulting data frame
#' # The output contains subject IDs, ETAs, ETA_PRIMs, EBVs, and covariate values.
#' head(individual_etas)
#'
#' @family Diagnostics & Plotting
#' @concept diagnostics
calcEtas <- function(
    runno            = NULL,
    numNonFREMThetas,
    modName          = NULL,
    numSkipOm        = 0,
    idvar            = "ID",
    modDevDir        = NULL,
    FFEMData         = NULL,
    covmodel         = "linear",
    dataFile         = NULL,
    parNames         = NULL,
    quiet            = TRUE,
    ffemModName      = NULL,
    ...) {
  
  # Capture all ... arguments into a list
  dots <- list(...)
  
  # --- Argument filtering for getFileNames ---
  # Arguments for getFileNames are its formal args + any from ... that match
  getfiles_args_from_dots <- dots[names(dots) %in% names(formals(getFileNames))]
  getfiles_args <- c(
    list(runno = runno, modName = modName, modDevDir = modDevDir),
    getfiles_args_from_dots
  )
  fileNames <- do.call(getFileNames, getfiles_args)
  
  # --- Conditional creation of FFEMData ---
  if (is.null(FFEMData)) {
    if (is.null(dataFile) || is.null(parNames)) {
      stop("If `FFEMData` is NULL, you must provide `dataFile` and `parNames`.", call. = FALSE)
    }
    if (!quiet) message("`FFEMData` object not provided. Creating it internally...")
    
    # Arguments for createFFEMdata are its formal args + any from ... that match
    ffem_args_from_dots <- dots[names(dots) %in% names(formals(createFFEMdata))]
    ffem_args <- c(
      list(
        runno            = runno,
        modName          = modName,
        modDevDir        = modDevDir,
        numNonFREMThetas = numNonFREMThetas,
        numSkipOm        = numSkipOm,
        dataFile         = dataFile,
        parNames         = parNames,
        newDataFile      = NULL,
        idvar            = idvar,
        quiet            = quiet
      ),
      ffem_args_from_dots
    )
    FFEMData <- do.call(createFFEMdata, ffem_args)
  }
  
  modFile   <- fileNames$mod
  extFile   <- fileNames$ext
  phiFile   <- fileNames$phi
  
  dfphi <- getPhi(phiFile)
  dfExt <- getExt(extFile)
  
  dfone <- FFEMData$newData[!duplicated(FFEMData$newData[[idvar]]), c(idvar, FFEMData$indCovEff)]
  
  if (nrow(dfphi) != nrow(dfone)) stop("Number of unique individuals in the dataset and phi file needs to be the same")
  
  etafrem <- dfphi[, 3:(2 + numSkipOm + nrow(FFEMData$Omega))]
  etaprim <- etafrem
  for (i in 1:length(FFEMData$indCovEff)) {
    etaprim[, (i + numSkipOm)] <- etafrem[, (i + numSkipOm)] - dfone[, i + 1]
  }
  names(etaprim) <- paste0(names(etaprim), "_PRIM")
  
  if (nrow(dfExt) > 1) dfExt <- dfExt[dfExt$ITERATION == -1000000000, ]
  
  numFREMThetas <- length(grep("THETA", names(dfExt))) - numNonFREMThetas
  df_thm        <- as.numeric(dfExt[, (numNonFREMThetas + 2):(numNonFREMThetas + 1 + numFREMThetas)])
  
  covariates        <- dfphi[, (3 + numSkipOm + nrow(FFEMData$Omega)):((2 + numSkipOm + nrow(FFEMData$Omega)) + numFREMThetas)]
  names(covariates) <- getCovNames(modFile = modFile)$covNames
  
  if (covmodel == "linear") {
    for (i in 1:length(df_thm)) {
      covariates[, i] <- covariates[, i] + df_thm[i]
    }
  }
  
  retDf <- cbind(ID = dfphi[, 2], etaprim, etafrem, covariates)
  names(retDf) <- gsub("\\.", "", names(retDf))
  
  ## --- Append FFEM EBVs if requested ---
  if (!is.null(ffemModName)) {
    ffem_files <- getFileNames(modName = ffemModName, modDevDir = modDevDir)
    ffem_phi_file <- paste0(tools::file_path_sans_ext(ffem_files$mod), ".phi")
    
    if (!file.exists(ffem_phi_file)) {
      if (!quiet) warning(sprintf("Cannot find FFEM .phi file at '%s'. Skipping FFEM EBVs.", ffem_phi_file), call. = FALSE)
    } else {
      df_ffem_phi <- getPhi(ffem_phi_file)
      
      # We only want the structural ETAs that exist in the base model
      target_etas <- names(etafrem) 
      target_etas_clean <- gsub("\\.", "", target_etas)
      
      missing_etas <- setdiff(target_etas, names(df_ffem_phi))
      if (length(missing_etas) > 0) {
        if (!quiet) warning("Some structural ETAs from the base model are missing in the FFEM phi file. Skipping EBV merge.", call. = FALSE)
      } else {
        # Securely locate the ID column in the FFEM phi file
        phi_id_col <- grep("^\\s*ID\\s*$", names(df_ffem_phi), ignore.case = TRUE, value = TRUE)
        if (length(phi_id_col) == 0) phi_id_col <- names(df_ffem_phi)[2] 
        
        df_ebv <- df_ffem_phi[, c(phi_id_col[1], target_etas), drop = FALSE]
        
        # --- LENGTH VERIFICATION CHECK ---
        if (nrow(df_ebv) != nrow(retDf)) {
          stop(sprintf(
            "Subject count mismatch: The base FREM model has %d subjects, but the FFEM model '%s' has %d subjects. Cannot safely merge EBVs.", 
            nrow(retDf), ffemModName, nrow(df_ebv)
          ), call. = FALSE)
        }
        
        # Standardize ID column name and rename ETAs to EBVs for clarity
        names(df_ebv)[1] <- "ID"
        names(df_ebv)[-1] <- paste0("EBV_", target_etas_clean)
        
        # Merge seamlessly
        retDf <- merge(retDf, df_ebv, by = "ID", all.x = TRUE)
      }
    }
  }
  
  return(retDf)
}