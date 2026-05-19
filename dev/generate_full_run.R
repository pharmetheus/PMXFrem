# ------------------------------------------------------------------------------
# Development Script: Generate FULL FREM Model for Validation
# ------------------------------------------------------------------------------

# --- 1. Setup ---
devtools::load_all()

# --- 2. User Configuration ---
# Use the new PMX argument structure instead of a hardcoded baseModelFile
modDevDir <- system.file("extdata", "SimNeb", package = "PMXFrem") 
modName   <- "run30" # The function will automatically look for run30.mod and run30.ext

ffemDataFile <- system.file("extdata", "SimNeb", "DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem") 
outputDir    <- file.path(getwd(), "dev", "run_full_frem") 

covariates <- c("WT", "AGE", "BMI", "SEX", "SMOK", "NCIL","RACEL")
catCovs <- c("SEX", "SMOK", "NCIL","RACEL")
# covariates <- c("RACEL", "WT")
# catCovs    <- c("RACEL")
logtCovs   <- NULL

keep_cols <- c("ID", "TIME", "AMT", "EVID", "RATE", "FOOD", "DV")
# sort_cols <- c("ID", "TIME", "FREMTYPE")

# --- 3. Execution ---
message("\nCalling master createFREMmodel...")

if (!exists("createFREMmodel")) {
  stop("Please run devtools::load_all() or source the necessary function files.")
}

# Run the master wrapper function using the harmonized arguments
generated_files <- createFREMmodel(
  modName          = modName,           # <-- Updated
  modDevDir        = modDevDir,         # <-- Updated
  ffemDataFile     = ffemDataFile,
  covariates       = covariates,
  catCovs          = catCovs,
  outputDir        = outputDir,
  finalModName     = "frem_final",      # Optional: Name your final model file
  keepMinimalModel = FALSE,             # Optional: Set to TRUE to inspect the Phase 1 output
  numNonFREMThetas = 7,                 # NOTE: Double-check this matches run30.mod!
  numSkipOm        = 2,  
  cstrKeepCols     = keep_cols, 
  IDvar            = "ID",
  missVal          = -99,
  fixTheta         = TRUE,
  roundMeanTo      = 1, 
  logtCovs         = logtCovs,
  useMuModeling    = TRUE,
  quiet = TRUE
)

message("\nDone! Check the output directory: ", outputDir)

# --- 4. Conclusion ---
message("\n--------------------------------------------------------------")
message("Script finished. The following files are ready for NONMEM:")
message("\n  Final Model File:   ", generated_files$final_model)
message("  Final Data File:    ", generated_files$final_data)
message("\nNext Step: Run the generated model file with NONMEM.")
message("--------------------------------------------------------------")