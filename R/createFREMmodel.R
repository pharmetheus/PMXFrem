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
#' @param finalModName Character. The base name for the generated final model and data files (e.g., `"frem_model"`).
#' @param keepMinimalModel Logical. Should the intermediate minimal model files (Phase 1 output) be preserved? Defaults to `FALSE`.
#' @param catCovs Character vector. Names of the covariates in `covariates` that should be treated as categorical.
#' @param logtCovs Character vector. Names of continuous covariates in `covariates` that should be log-transformed (natural log).
#' @param IDvar Character. The name of the subject identifier column in the dataset. Defaults to `"ID"`.
#' @param missVal Numeric. The value representing missing data in the covariates. Defaults to `-99`.
#' @param fixTheta Logical. Should the initial THETA estimates for the covariates be fixed? Defaults to `TRUE`.
#' @param roundMeanTo Numeric. The number of decimal places to round the calculated baseline covariate means. Defaults to `1`.
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
#' \dontrun{
#' # Setup paths using package extdata
#' modDevDir    <- system.file("extdata", "SimNeb", package = "PMXFrem")
#' modName      <- "run30"
#' ffemDataFile <- system.file("extdata", "SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")
#' 
#' 
#' # Use a temporary directory for the output to comply with CRAN policies
#' outputDir <- tempdir()
#' 
#' # Define covariates and keep columns
#' covariates <- c("WT","SEX","RACEL")
#' catCovs    <- c("RACEL","SEX")
#' keep_cols  <- c("ID", "TIME", "AMT", "EVID", "RATE", "FOOD", "DV")
#' 
#' # Generate the full FREM model
#' generated_files <- createFREMmodel(
#'   modName            = modName,
#'   modDevDir          = modDevDir,
#'   ffemDataFile       = ffemDataFile,
#'   covariates         = covariates,
#'   catCovs            = catCovs,         
#'   outputDir          = outputDir,
#'   finalModName       = "frem_final",
#'   numNonFREMThetas   = 7,
#'   numSkipOm          = 2,
#'   cstrKeepCols       = keep_cols,
#'   bRecodeDichotomous = TRUE,
#'   quiet              = FALSE
#' )
#' 
#' # The generated files are available in the temporary directory
#' print(generated_files$model)
#' print(generated_files$data)
#' }

createFREMmodel <- function(runno                = NULL,
                            modName              = NULL,
                            modDevDir            = NULL,
                            ffemDataFile,
                            covariates,
                            numNonFREMThetas,
                            outputDir            = NULL,
                            finalModName         = "frem_model",
                            keepMinimalModel     = FALSE,
                            catCovs              = NULL,
                            logtCovs             = NULL,
                            IDvar                = "ID",
                            missVal              = -99,
                            fixTheta             = TRUE,
                            roundMeanTo          = 1,
                            useMuModeling        = TRUE,
                            numSkipOm            = 0,
                            cstrKeepCols         = c("ID", "TIME", "AMT", "II", "EVID", "SS", "RATE", "DV"),
                            quiet                = FALSE, 
                            bRecodeDichotomous   = FALSE,
                            allowNon01           = FALSE,
                            keepDoseOnlySubjects = FALSE,
                            ...) {
  
  if (is.null(outputDir)) {
    outputDir <- if (!is.null(modDevDir)) modDevDir else getwd()
  }
  
  if (length(covariates) == 0) stop("At least one covariate must be provided.")
  
  minModName <- paste0(finalModName, "_minimal")
  
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
  
  finalModelPath <- file.path(outputDir, paste0(finalModName, ".mod"))
  finalDataPath  <- file.path(outputDir, paste0(finalModName, "_data.csv"))
  
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
      numNonFREMThetas     = numNonFREMThetas,
      numSkipOm            = numSkipOm, 
      cstrKeepCols         = cstrKeepCols,
      bWriteMod            = FALSE,
      bRecodeDichotomous   = bRecodeDichotomous,
      allowNon01           = allowNon01,
      keepDoseOnlySubjects = keepDoseOnlySubjects,
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