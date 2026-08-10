#' Create a Complete FREM Model
#'
#' @description
#' The master orchestrator for generating a Full Random Effects Model (FREM) in NONMEM. 
#' This wrapper manages a two-phase pipeline: it bootstraps a minimal model with the first 
#' covariate (handling any necessary categorical Y-1 expansions), iteratively injects 
#' the remaining covariates, safely manages `FREMTYPE` sequencing between phases, and 
#' cleans up all intermediate files.
#'
#' @param runno Standard PMX run number.
#' @param modName Standard PMX model name.
#' @param modDevDir Standard PMX model development directory.
#' @param ffemDataFile Character. Path to the Full Fixed Effects Model (FFEM) dataset.
#' @param covariates Character vector. The full list of covariates to include in the FREM model. 
#'   The first element is processed during Phase 1 (bootstrapping), and the remainder during Phase 2.
#' @param numNonFREMThetas Numeric. The number of THETAs in the base model that are not part of the FREM structure.
#' @param outputDir Character. Directory where the final model and data files will be saved. Defaults to `modDevDir` or current working directory.
#' @param fremModName Character. The base name for the generated final model and data files (e.g., `"frem_model"`).
#' @param keepMinimalModel Logical. Should the intermediate minimal model files (Phase 1 output) be preserved? Defaults to `FALSE`.
#' @param catCovs Character vector. Names of the covariates in `covariates` that should be treated as categorical.
#' @param logtCovs Character vector. Names of continuous covariates in `covariates` that should be log-transformed (natural log).
#' @param IDvar Character. The name of the subject identifier column in the dataset. Defaults to `"ID"`.
#' @param missVal Numeric. The value representing missing data in the covariates. Defaults to `-99`.
#' @param fixTheta Logical. Should the initial THETA estimates for the covariates be fixed? Defaults to `TRUE`.
#' @param roundMeanTo Numeric. The number of decimal places to round the calculated baseline covariate means. Defaults to `2`.
#' @param useMuModeling Logical. Should MU-referencing be utilized when generating the NONMEM $PK and $ERROR blocks? Defaults to `TRUE`.
#' @param numSkipOm Numeric. Number of initial OMEGA parameters to skip when appending new FREM parameters. Defaults to `0`.
#' @param cstrKeepCols Character vector. Columns to strictly retain in the generated FREM dataset. Note: `"FREMTYPE"` is managed internally and injected between phases.
#' @param bRecodeDichotomous Logical. Should dichotomous covariates be automatically 
#'   recoded to 0/1? Defaults to FALSE. If FALSE, inputs must be strictly 0/1.
#' @param allowNon01 Logical. If TRUE, allows non-0/1 dichotomous covariates (like 1/2) 
#'   to pass through untouched for PsN compatibility. Defaults to FALSE.
#' @param quiet Logical. Should the function execute silently without console messages? Defaults to `FALSE`.
#' @param ... Additional arguments strictly passed to the Phase 1 bootstrapper (`createFREMmodel_phase1`).
#' @param keepDoseOnlySubjects Logical. If \code{FALSE} (default), subjects without any valid PK observations (e.g., 
#' only dosing records) are completely excluded from the generated dataset. If \code{TRUE}, 
#' these subjects are retained and their covariates are included as observations.
#'
#' @return An invisible list containing the file paths to the generated assets:
#' \itemize{
#'   \item \code{model}: Path to the final generated `.mod` file.
#'   \item \code{data}: Path to the final generated `.csv` data file.
#' }
#' 
#' @export
#'
#' @examples
#' 
#' # Setup paths using package extdata
#' modDevDir    <- system.file("extdata", "SimNeb", package = "PMXFrem")
#' modName      <- "run30"
#' ffemDataFile <- system.file("extdata", "SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")
#' 
#' 
#' td <- tempfile(pattern = "frem_example_")
#' dir.create(td)
#' 
#' # Define covariates and keep columns
#' covariates <- c("AGE","WT","SEX","RACEL")
#' catCovs    <- c("RACEL","SEX")
#' keep_cols  <- c("ID", "TIME", "AMT", "EVID", "RATE", "FOOD", "DV")
#' 
#' # Generate the full FREM model
#' generated_files <- createFREMmodel(
#'   modName            = modName,
#'   modDevDir          = modDevDir,
#'   ffemDataFile       = ffemDataFile,
#'   covariates         = covariates,
#'   logtCovs           = "WT",
#'   catCovs            = catCovs,         
#'   outputDir          = td,
#'   fremModName       = "frem_final",
#'   numNonFREMThetas   = 7,
#'   numSkipOm          = 2,
#'   cstrKeepCols       = keep_cols,
#'   bRecodeDichotomous = TRUE,
#'   quiet              = TRUE
#' )
#' 
#' # The generated files are available in the temporary directory
#' print(generated_files$model)
#' print(generated_files$data)
#' 
#' #Clean up the temporary directory
#' unlink(td, recursive = TRUE)
#' 
#' @family FREM model management
#' @concept frem_model_management

createFREMmodel <- function(runno                = NULL,
                            modName              = NULL,
                            modDevDir            = NULL,
                            ffemDataFile,
                            covariates,
                            numNonFREMThetas,
                            outputDir            = NULL,
                            fremModName         = "frem_model",
                            keepMinimalModel     = FALSE,
                            catCovs              = NULL,
                            logtCovs             = NULL,
                            IDvar                = "ID",
                            missVal              = -99,
                            fixTheta             = TRUE,
                            roundMeanTo          = 2,
                            useMuModeling        = TRUE,
                            numSkipOm            = 0,
                            cstrKeepCols         = c("ID", "TIME", "AMT", "II", "EVID", "SS", "RATE", "DV"),
                            quiet                = FALSE, 
                            bRecodeDichotomous   = FALSE,
                            allowNon01           = FALSE,
                            keepDoseOnlySubjects = FALSE,
                            ...) {
  
  # Default to modDevDir if outputDir isn't specified
  if (is.null(outputDir)) {
    outputDir <- if (!is.null(modDevDir)) modDevDir else getwd()
  }
  
  finalModelPath <- file.path(outputDir, paste0(fremModName, ".mod"))
  finalDataPath  <- file.path(outputDir, paste0(fremModName, "_data.csv"))
  
  # --- Overwrite Protection ---
  if (file.exists(finalModelPath) || file.exists(finalDataPath)) {
    stop(sprintf("Protection Error: The output files '%s' or '%s' already exist in the target directory (%s). Please use a different `fremModName`, specify a new `outputDir`, or manually remove the existing files.", 
                 basename(finalModelPath), basename(finalDataPath), outputDir), 
         call. = FALSE)
  }
  
  if (length(covariates) == 0) stop("At least one covariate must be provided.")
  
  # --- Base Model Diagnostics Check (Pre-flight Validation) ---
  fileNames <- getFileNames(runno = runno, modName = modName, modDevDir = modDevDir)
  if (file.exists(fileNames$mod)) {
    baseModelLines <- readLines(fileNames$mod, warn = FALSE)
    
    # Robustly parse $EST blocks (handling multi-line definitions and stripping comments)
    est_blocks <- list()
    in_est <- FALSE
    current_est <- ""
    
    for (l in baseModelLines) {
      l_clean <- gsub(";.*", "", l) # Strip inline comments
      if (grepl("^\\s*\\$[A-Za-z]+", l_clean)) {
        if (grepl("^\\s*\\$EST", l_clean, ignore.case = TRUE)) {
          if (in_est) {
            # Save the previous $EST block before initializing the new one
            est_blocks <- c(est_blocks, current_est)
          }
          in_est <- TRUE
          current_est <- trimws(l_clean)
        } else {
          if (in_est) {
            est_blocks <- c(est_blocks, current_est)
            in_est <- FALSE
          }
        }
      } else if (in_est) {
        # Only append if there is actual text remaining
        if (nchar(trimws(l_clean)) > 0) {
          current_est <- paste(current_est, trimws(l_clean))
        }
      }
    }
    if (in_est) est_blocks <- c(est_blocks, current_est)
    
    # Evaluate parsed $EST blocks against the safety rules
    if (length(est_blocks) > 0) {
      has_saem <- FALSE
      has_imp <- FALSE
      imp_niter_low <- FALSE
      final_phitype <- 0
      
      for (block in est_blocks) {
        if (grepl("\\bSAEM\\b", block, ignore.case = TRUE)) has_saem <- TRUE
        if (grepl("\\bIMP(MAP)?\\b", block, ignore.case = TRUE)) has_imp <- TRUE
        
        # Check NITER if IMP/IMPMAP is used
        if (grepl("\\bIMP(MAP)?\\b", block, ignore.case = TRUE)) {
          niter_match <- regmatches(block, regexpr("NITER\\s*=\\s*([0-9]+)", block, ignore.case = TRUE))
          if (length(niter_match) > 0) {
            niter_val <- as.numeric(gsub("(?i)NITER\\s*=\\s*", "", niter_match[1]))
            if (!is.na(niter_val) && niter_val < 150) {
              imp_niter_low <- TRUE
            }
          }
        }
        
        # Track PHITYPE state machine
        if (grepl("\\bPHITYPE\\s*=\\s*1\\b", block, ignore.case = TRUE)) {
          final_phitype <- 1
        } else if (grepl("\\bPHITYPE\\s*=\\s*0\\b", block, ignore.case = TRUE)) {
          final_phitype <- 0
        }
      }
      
      # Emit the requested warnings
      if (has_saem) {
        warning("SAEM currently can not handle missing covariate values correctly. If you have missing covariate values in the data, use IMP or IMPMAP instead.", call. = FALSE)
      }
      if (!has_imp) {
        warning("Consider using IMP or IMPMAP to increase the robustness of the estimation of the FREM model.", call. = FALSE)
      }
      if (has_imp && imp_niter_low) {
        warning("Consider increasing NITER to at least 150.", call. = FALSE)
      }
      if (final_phitype == 0) {
        warning("Set PHITYPE=1 to facilitate post-processing of the results", call. = FALSE)
      }
    }
  } else {
    warning(sprintf("Could not locate base model file at '%s' for pre-flight diagnostics.", fileNames$mod), call. = FALSE)
  }
  # --- End Base Model Diagnostics Check ---
  
  minModName <- paste0(fremModName, "_minimal")
  
  # --- 1. Phase 1: Bootstrap the Minimal Model ---
  if (!quiet) message("\nStarting Phase 1: Bootstrapping minimal model...")
  phase1_out <- createFREMmodel_phase1(
    runno              = runno, 
    modName            = modName, 
    modDevDir          = modDevDir,
    ffemDataFile       = ffemDataFile, 
    covariates         = covariates, 
    outputDir          = outputDir,
    minModName         = minModName, 
    keepMinimalModel   = keepMinimalModel, 
    catCovs            = catCovs,
    logtCovs           = logtCovs, 
    IDvar              = IDvar, 
    missVal            = missVal, 
    fixTheta           = fixTheta,
    roundMeanTo        = roundMeanTo, 
    useMuModeling      = useMuModeling, 
    numSkipOm          = numSkipOm,
    cstrKeepCols       = cstrKeepCols, 
    bRecodeDichotomous = bRecodeDichotomous,
    allowNon01         = allowNon01,
    quiet              = quiet, 
    keepDoseOnlySubjects = keepDoseOnlySubjects,
    ... 
  )
  
  finalModelPath <- file.path(outputDir, paste0(fremModName, ".mod"))
  finalDataPath  <- file.path(outputDir, paste0(fremModName, "_data.csv"))
  
  # --- 2. Phase 2: Add Remaining Covariates (if any) ---
  if (length(covariates) > 1) {
    if (!quiet) message("Starting Phase 2: Iterating remaining covariates...")
    
    if (!("FREMTYPE" %in% cstrKeepCols)) cstrKeepCols <- c(cstrKeepCols, "FREMTYPE")
    
    cov_to_add <- covariates[-1]
    contCovsToAdd <- cov_to_add[!(cov_to_add %in% catCovs)]
    catCovsToAdd  <- cov_to_add[cov_to_add %in% catCovs]
    
    update_out <- updateFREMmodel(
      strFREMModel         = phase1_out$minimalModelFile, 
      strFREMData          = phase1_out$minimalDataFile,
      strFFEMData          = phase1_out$validatedData, 
      cstrContCovsToAdd    = if (length(contCovsToAdd) > 0) contCovsToAdd else NULL,
      cstrCatCovsToAdd     = if (length(catCovsToAdd) > 0) catCovsToAdd else NULL,
      cstrCovsToAddOrder   = cov_to_add, 
      strNewFREMData       = finalDataPath,
      strUpdateType        = "DataAndModel", 
      strID                = IDvar, 
      missVal = missVal,
      numNonFREMThetas     = numNonFREMThetas,
      numSkipOm            = numSkipOm, 
      cstrKeepCols         = cstrKeepCols,
      bWriteMod            = FALSE,
      bRecodeDichotomous   = bRecodeDichotomous,
      allowNon01           = allowNon01,
      keepDoseOnlySubjects = keepDoseOnlySubjects,
      roundMeanTo          = roundMeanTo,
      fixTheta             = fixTheta,
      quiet                = quiet 
    )
    
    if ("model" %in% names(update_out)) writeLines(update_out$model, finalModelPath)
  } else {
    file.copy(phase1_out$minimalModelFile, finalModelPath, overwrite = TRUE)
    file.copy(phase1_out$minimalDataFile, finalDataPath, overwrite = TRUE)
  }
  
  # --- 2.5 STRICT DATA VALIDATION ---
  final_frem_data <- read.csv(finalDataPath)
  validateFremData(originalData = phase1_out$validatedData, fremData = final_frem_data, strID = IDvar, quiet = quiet)
  
  
  # --- 3. Cleanup ---
  if (!keepMinimalModel) {
    mockExtFile <- paste0(tools::file_path_sans_ext(phase1_out$minimalModelFile), ".ext")
    unlink(c(phase1_out$minimalModelFile, phase1_out$minimalDataFile, mockExtFile), force = TRUE)
  }
  
  if (!quiet) message("FREM model generation complete.")
  return(invisible(list(model = finalModelPath, data = finalDataPath)))
}
