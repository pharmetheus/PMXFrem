#' Bootstrap the Minimal FREM Model (Phase 1)
#'
#' @description
#' This is the internal bootstrapper function for the FREM generation pipeline. It reads
#' the base NONMEM model and the Full Fixed Effects Model (FFEM) dataset, processes strictly
#' the first covariate from the provided list, and generates a minimal FREM data file,
#' `.mod` file, and a mock `.ext` file. It inherently handles dynamic Y-1 categorical
#' expansions (e.g., splitting a 3-level categorical variable into two dummy variables)
#' and establishes the initial `FREMTYPE` sequencing.
#'
#' @param runno Standard PMX run number.
#' @param modName Standard PMX model name.
#' @param modDevDir Standard PMX model development directory.
#' @param ffemDataFile Character. Path to the base FFEM dataset.
#' @param covariates Character vector. The full list of covariates to be added to the FREM model. Phase 1 strictly processes the first element.
#' @param outputDir Character. Directory where the minimal files will be saved. Defaults to `modDevDir` or current working directory.
#' @param minModName Character. The base name for the generated minimal files. Defaults to `"minimal_model"`.
#' @param keepMinimalModel Logical. Should the minimal files be retained after the full pipeline completes? Passed back to the master wrapper. Defaults to `FALSE`.
#' @param cstrKeepCols Character vector. Columns to strictly retain in the generated FREM dataset.
#' @param numSkipOm Numeric. Number of initial OMEGA parameters to skip when appending new FREM parameters. Defaults to `0`.
#' @param IDvar Character. The name of the subject identifier column. Defaults to `"ID"`.
#' @param missVal Numeric. The value representing missing data in the covariates. Defaults to `-99`.
#' @param fixTheta Logical. Should the initial THETA estimates for the covariates be fixed? Defaults to `TRUE`.
#' @param roundMeanTo Numeric. The number of decimal places to round the calculated baseline covariate means. Defaults to `1`.
#' @param catCovs Character vector. Names of the covariates that should be treated as categorical.
#' @param logtCovs Character vector. Names of continuous covariates that should be log-transformed.
#' @param useMuModeling Logical. Should MU-referencing be utilized when generating the NONMEM $PK and $ERROR blocks? Defaults to `TRUE`.
#' @param bRecodeDichotomous Logical. Should dichotomous covariates be automatically
#'   recoded to 0/1? Defaults to FALSE. If FALSE, inputs must be strictly 0/1.
#' @param allowNon01 Logical. If TRUE, allows non-0/1 dichotomous covariates (like 1/2)
#'   to pass through untouched for PsN compatibility. Defaults to FALSE.
#' @param quiet Logical. Should the function execute silently without console messages? Defaults to `FALSE`.
#' @param userKeepCols Character vector or `NULL`. The `cstrKeepCols` the user
#'   supplied to [createFREMmodel()], if any; a column among them that the base
#'   model drops is an error. `NULL` (the default columns) skips that check.
#' @param ... Additional arguments passed down to internal helper functions (e.g., `createFREMData`, `prepareAndValidateData`).
#' @param keepDoseOnlySubjects Logical. If \code{FALSE} (default), subjects without any valid PK observations (e.g.,
#' only dosing records) are completely excluded from the generated dataset. If \code{TRUE},
#' these subjects are retained and their covariates are included as observations.
#'
#' @return An invisible list containing:
#' \itemize{
#'   \item \code{minimalModelFile}: Path to the generated minimal `.mod` file.
#'   \item \code{minimalDataFile}: Path to the generated minimal `.csv` data file.
#'   \item \code{mockExtFile}: Path to the generated mock `.ext` file.
#'   \item \code{keepMinimalModel}: Boolean flag passed back to the master wrapper for cleanup logic.
#'   \item \code{validatedData}: The filtered and validated FFEM dataset as a \code{data.frame}.
#' }
#'
#' @seealso \code{\link{createFREMmodel}}, \code{\link{updateFREMmodel}}
#' @family FREM model management Internal
#' @concept frem_model_management
#' @keywords internal
createFREMmodel_phase1 <- function(runno = NULL,
                                   modName = NULL,
                                   modDevDir = NULL,
                                   ffemDataFile,
                                   covariates,
                                   outputDir = NULL,
                                   minModName = "minimal_model",
                                   keepMinimalModel = FALSE,
                                   cstrKeepCols = c("ID", "TIME", "AMT", "II", "EVID", "SS", "RATE"),
                                   numSkipOm = 0,
                                   IDvar = "ID",
                                   missVal = -99,
                                   fixTheta = TRUE,
                                   roundMeanTo = 1,
                                   catCovs = NULL,
                                   logtCovs = NULL,
                                   useMuModeling = TRUE,
                                   bRecodeDichotomous = FALSE,
                                   allowNon01 = FALSE,
                                   keepDoseOnlySubjects = FALSE,
                                   quiet = FALSE,
                                   userKeepCols = NULL,
                                   ...) {
  # --- 1. Resolve Paths via Standard PMX Utilities ---
  fileNames <- getFileNames(runno = runno, modName = modName, modDevDir = modDevDir)
  baseModelFile <- fileNames$mod
  baseExtFile <- fileNames$ext

  if (is.null(outputDir)) {
    outputDir <- if (!is.null(modDevDir)) modDevDir else getwd()
  }

  if (!dir.exists(outputDir)) {
    if (!quiet) message("Output directory not found. Creating it: ", outputDir)
    dir.create(outputDir, recursive = TRUE)
  }

  # Set up minimal files dynamically using minModName
  minimalModelPath <- file.path(outputDir, paste0(minModName, ".mod"))
  minimalDataPath <- file.path(outputDir, paste0(minModName, "_data.csv"))
  mockExtPath <- file.path(outputDir, paste0(minModName, ".ext"))

  validationResult <- prepareAndValidateData(
    ffemDataFile         = ffemDataFile,
    baseModelFile        = baseModelFile,
    covariates           = covariates,
    quiet                = quiet,
    keepDoseOnlySubjects = keepDoseOnlySubjects,
    strID                = IDvar,
    cstrKeepCols         = userKeepCols
  )
  wideData <- validationResult$validatedData

  # --- 2. Extract First Covariate Info & Expand if Categorical ---
  firstCovariateName <- covariates[1]
  is_cat <- firstCovariateName %in% catCovs

  # Summarise the covariate as phase 2 (prepareNewCovariates()) and the FREM
  # data set do: each subject's first non-missing value, and a THETA fixed only
  # when every subject has one.
  covValues <- .fremCovFirstValues(wideData, firstCovariateName, strID = IDvar, missVal = missVal)
  shouldFix <- fixTheta && !.fremCovHasMissing(wideData, firstCovariateName, strID = IDvar, missVal = missVal)
  covInfo <- function(name, x, fremType) {
    v <- stats::var(x)
    list(
      name           = name,
      mean           = round(mean(x), digits = roundMeanTo),
      variance       = if (is.na(v) || v == 0) 1E-04 else v,
      shouldFixTheta = shouldFix,
      fremType       = fremType
    )
  }

  initialCovariateInfo <- list()
  frem_counter <- 100

  if (is_cat) {
    valid_vals <- sort(unique(covValues))

    if (length(valid_vals) > 2) {
      for (val in valid_vals[-1]) {
        initialCovariateInfo[[length(initialCovariateInfo) + 1]] <-
          covInfo(paste0(firstCovariateName, "_", val), as.numeric(covValues == val), frem_counter)
        frem_counter <- frem_counter + 100
      }
    } else if (length(valid_vals) == 2 && bRecodeDichotomous) {
      ## Recoded to 0/1 and named by the second level, as createFREMData() does.
      val <- valid_vals[2]
      initialCovariateInfo[[1]] <-
        covInfo(paste0(firstCovariateName, "_", val), as.numeric(covValues == val), frem_counter)
    } else if (length(valid_vals) == 2) {
      ## Not recoded: kept under its own name, as createFREMData() and phase 2 do.
      initialCovariateInfo[[1]] <- covInfo(firstCovariateName, covValues, frem_counter)
    } else {
      initialCovariateInfo[[1]] <- list(
        name = firstCovariateName, mean = 0, variance = 1E-04, shouldFixTheta = shouldFix, fremType = 100
      )
    }
  } else {
    if (!is.null(logtCovs) && firstCovariateName %in% logtCovs) {
      covValues <- log(covValues)
    }
    initialCovariateInfo[[1]] <- covInfo(firstCovariateName, covValues, 100)
  }

  # --- 3. Generate Minimal FREM Dataset ---
  if (!quiet) message("Generating minimal FREM dataset for covariate: ", firstCovariateName)
  createFREMData(
    strFFEMData          = wideData,
    strFREMDataFileName  = minimalDataPath,
    strID                = IDvar,
    cstrKeepCols         = cstrKeepCols,
    cstrContCovs         = if (!is_cat) firstCovariateName else NULL,
    cstrCatCovs          = if (is_cat) firstCovariateName else NULL,
    logtCovs             = logtCovs,
    bRecodeDichotomous   = bRecodeDichotomous,
    allowNon01           = allowNon01,
    keepDoseOnlySubjects = keepDoseOnlySubjects,
    quiet                = quiet,
    ...
  )
  minimalDataHeaders <- names(data.table::fread(minimalDataPath, header = TRUE, nrows = 0))

  # --- 4. Generate Minimal FREM Model ---
  if (!quiet) message("Generating minimal FREM model file...")
  baseModelInfo <- parseBaseModel(baseModelFile, numSkipOm = numSkipOm)
  num_sigmas <- if (!is.null(baseModelInfo$sigmaBlock)) {
    nrow(parseMatrixBlockToMatrix(baseModelInfo$sigmaBlock))
  } else {
    0
  }

  minimalModelLines <- createMinimalFremModel(
    baseModelInfo        = baseModelInfo,
    initialCovariateInfo = initialCovariateInfo,
    fremDataPath         = basename(minimalDataPath),
    fremDataHeaders      = minimalDataHeaders,
    covEpsNum            = num_sigmas + 1,
    useMuModeling        = useMuModeling
  )
  writeLines(minimalModelLines, minimalModelPath)

  # --- 5. Generate Mock .ext File ---
  if (!quiet) message("Generating mock .ext file...")
  if (file.exists(baseExtFile)) {
    createMockExt(
      baseExtFile          = baseExtFile,
      mockExtFile          = mockExtPath,
      initialCovariateInfo = initialCovariateInfo
    )
    if (!quiet) message("Mock .ext file generated: ", mockExtPath)
  } else {
    warning("Base .ext file not found at: ", baseExtFile, "\nCannot generate mock .ext.")
  }

  if (!quiet) message("\nPhase 1 complete.")

  # Pass keepMinimalModel back to the wrapper so it knows what to do!
  return(invisible(list(
    minimalModelFile = minimalModelPath,
    minimalDataFile  = minimalDataPath,
    mockExtFile      = mockExtPath,
    keepMinimalModel = keepMinimalModel,
    validatedData    = wideData # <-- PASS-BACK: Feed the pristine data to Phase 2
  )))
}
