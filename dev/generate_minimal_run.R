# ------------------------------------------------------------------------------
# Development Script: Generate Minimal FREM Model for Validation
# ------------------------------------------------------------------------------
# ... (script header remains the same) ...
# ------------------------------------------------------------------------------

# --- 1. Setup ---
devtools::load_all()

# --- 2. User Configuration ---
message("Defining input parameters...")
baseModelFile <- system.file("extdata", "SimNeb", "run30.mod", package = "PMXFrem") # NOTE: Replace with your package name
ffemDataFile  <- system.file("extdata", "SimNeb", "DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem") # NOTE: Replace with your package name
outputDir <- file.path(getwd(), "dev", "run_minimal_wt")
# covariates <- c("RACEL","WT", "AGE", "BMI", "SEX", "SMOK", "NCIL")
# catCovs <- c("RACEL","SEX", "SMOK", "NCIL")
covariates <- c("WT","RACEL")
catCovs <- c("RACEL")
logtCovs <- NULL

# Define the columns to keep from the dataset, as II and SS are not present
# in DAT-2-MI-PMX-2-onlyTYPE2-new.csv
keep_cols <- c("ID", "TIME", "AMT", "EVID", "RATE","FOOD","DAY","BLQ")
# sort_cols <- c("ID", "DAY","TIME", "FREMTYPE")

# --- 3. Execution ---
message("\nCalling createFREMmodel_phase1...")

if (!exists("createFREMmodel_phase1")) {
  stop("Please run devtools::load_all() or source the necessary function files.")
}

# Run the function, now with the cstrKeepCols argument specified
generated_files <- createFREMmodel_phase1(
  baseModelFile = baseModelFile,
  ffemDataFile  = ffemDataFile,
  covariates    = covariates,
  outputDir     = outputDir,
  cstrKeepCols  = keep_cols, # Specify the columns to keep
  numSkipOm     = 2, 
  IDvar         = "ID",
  missVal       = -99,
  fixTheta      = TRUE,
  roundMeanTo   = 1, 
  catCovs       = catCovs,
  logtCovs      = logtCovs,
  useMuModeling = TRUE
)

# --- 4. Conclusion ---
message("\n--------------------------------------------------------------")
message("Script finished. The following files are ready for NONMEM:")
message("\n  Model File:   ", generated_files$minimalModelFile)
message("  Data File:    ", generated_files$minimalDataFile)
message("\nNext Step: Run the generated model file with NONMEM.")
message("--------------------------------------------------------------")