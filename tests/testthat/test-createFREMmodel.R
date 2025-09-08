# tests/testthat/test-createFREMmodel.R

test_that("createFREMmodel input validation catches empty covariates", {
  expect_error(
    createFREMmodel(covariates = character(0)), 
    "At least one covariate must be provided."
  )
})

test_that("createFREMmodel works for a single covariate (Phase 1 only)", {
  # Setup paths
  modDevDir    <- system.file("extdata", "SimNeb", package = "PMXFrem")
  ffemDataFile <- file.path(modDevDir, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")
  outDir       <- file.path(tempdir(), "frem_test_single")
  dir.create(outDir, showWarnings = FALSE)
  
  # Run for just ONE covariate
  res <- createFREMmodel(
    modName          = "run30",
    modDevDir        = modDevDir,
    ffemDataFile     = ffemDataFile,
    covariates       = c("WT"),
    numNonFREMThetas = 7,
    cstrKeepCols    = c("ID", "TIME", "AMT", "EVID", "RATE","FOOD","DAY","BLQ"),
    numSkipOm        = 2,
    outputDir        = outDir,
    finalModName     = "test_single",
    quiet            = TRUE
  )
  
  # Check that final files exist
  expect_true(file.exists(res$model))
  expect_true(file.exists(res$data))
  
  # Check that default cleanup removed the minimal files
  expect_false(file.exists(file.path(outDir, "test_single_minimal.mod")))
  expect_false(file.exists(file.path(outDir, "test_single_minimal.ext")))
  expect_false(file.exists(file.path(outDir, "test_single_minimal_data.csv")))
  
  # Teardown
  unlink(outDir, recursive = TRUE)
})

test_that("createFREMmodel orchestrates Phase 1 & 2 for multiple covariates", {
  # Setup paths
  modDevDir    <- system.file("extdata", "SimNeb", package = "PMXFrem")
  ffemDataFile <- file.path(modDevDir, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")
  outDir       <- file.path(tempdir(), "frem_test_multi")
  dir.create(outDir, showWarnings = FALSE)
  
  # Run for MULTIPLE covariates (triggers Phase 1 -> Phase 2 handoff)
  res <- createFREMmodel(
    modName          = "run30",
    modDevDir        = modDevDir,
    ffemDataFile     = ffemDataFile,
    covariates       = c("RACEL", "WT"),
    catCovs          = c("RACEL"),
    numNonFREMThetas = 7,
    cstrKeepCols    = c("ID", "TIME", "AMT", "EVID", "RATE","FOOD","DAY","BLQ"),
    numSkipOm        = 2,
    outputDir        = outDir,
    finalModName     = "test_multi",
    quiet            = TRUE
  )
  
  # Check that final files exist
  expect_true(file.exists(res$model))
  expect_true(file.exists(res$data))
  
  # The dataset should have the FREMTYPE column injected
  final_df <- read.csv(res$data)
  expect_true("FREMTYPE" %in% names(final_df))
  
  # Check that default cleanup removed the minimal files
  expect_false(file.exists(file.path(outDir, "test_multi_minimal.mod")))
  expect_false(file.exists(file.path(outDir, "test_multi_minimal.ext")))
  expect_false(file.exists(file.path(outDir, "test_multi_minimal_data.csv")))
  
  # Teardown
  unlink(outDir, recursive = TRUE)
})

test_that("createFREMmodel respects keepMinimalModel = TRUE", {
  # Setup paths
  modDevDir    <- system.file("extdata", "SimNeb", package = "PMXFrem")
  ffemDataFile <- file.path(modDevDir, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")
  outDir       <- file.path(tempdir(), "frem_test_keep")
  dir.create(outDir, showWarnings = FALSE)
  
  res <- createFREMmodel(
    modName          = "run30",
    modDevDir        = modDevDir,
    ffemDataFile     = ffemDataFile,
    covariates       = c("RACEL", "WT"),
    catCovs          = c("RACEL"),
    numNonFREMThetas = 7,
    cstrKeepCols    = c("ID", "TIME", "AMT", "EVID", "RATE","FOOD","DAY","BLQ"),
    numSkipOm        = 2,
    outputDir        = outDir,
    finalModName     = "test_keep",
    keepMinimalModel = TRUE, # Override default cleanup
    quiet            = TRUE
  )
  
  # Final files exist
  expect_true(file.exists(res$model))
  expect_true(file.exists(res$data))
  
  # Minimal files MUST ALSO exist
  expect_true(file.exists(file.path(outDir, "test_keep_minimal.mod")))
  expect_true(file.exists(file.path(outDir, "test_keep_minimal.ext")))
  expect_true(file.exists(file.path(outDir, "test_keep_minimal_data.csv")))
  
  # Teardown
  unlink(outDir, recursive = TRUE)
})

test_that("createFREMmodel strictly enforces $DATA IGNORE statements across all phases", {
  # Setup self-contained environment
  td <- withr::local_tempdir()
  out_dir <- file.path(td, "frem_ignore_test")
  dir.create(out_dir, showWarnings = FALSE)
  
  base_mod_path <- file.path(td, "run_ignore.mod")
  data_path <- file.path(td, "data.csv")
  
  # 1. Mock Data: Subject 2 has BLQ == 1 and MUST be dropped
  dummy_data <- data.frame(
    ID    = c(1, 1, 2, 2, 3, 3),
    TIME  = c(0, 1, 0, 1, 0, 1),
    AMT   = c(100, 0, 100, 0, 100, 0),
    DV    = c(0, 5, 0, 6, 0, 7),
    WT    = c(70, 70, 80, 80, 90, 90),
    RACEL = c(1, 1, 2, 2, 3, 3),
    BLQ   = c(0, 0, 1, 1, 0, 0) 
  )
  write.csv(dummy_data, data_path, row.names = FALSE, quote = FALSE)
  
  # 2. Mock Model with a strict IGNORE statement
  writeLines(c(
    "$PROBLEM Base Model",
    "$INPUT ID TIME AMT DV WT RACEL BLQ",
    paste0("$DATA ", basename(data_path), " IGNORE=(BLQ.EQ.1)"),
    "$PK", " CL = THETA(1) * EXP(ETA(1))",
    "$ERROR", " Y = F + EPS(1)",
    "$THETA 10",
    "$OMEGA BLOCK(1) 0.1",
    "$SIGMA 1 FIX"
  ), base_mod_path)
  
  # 3. Mock Ext (required for the Phase 2 update)
  base_ext_path <- file.path(td, "run_ignore.ext")
  writeLines(c(
    "TABLE NO. 1",
    " ITERATION  THETA1  SIGMA(1,1)  OMEGA(1,1)  OBJ",
    " -1000000000  1.00000E+01  1.00000E+00  1.00000E-01  1.00000E+02",
    " -1000000006  0.00000E+00  1.00000E+00  0.00000E+00  0.00000E+00"
  ), base_ext_path)
  
  # 4. Action: Trigger the full pipeline (multiple covariates forces Phase 2)
  res <- createFREMmodel(
    modName          = "run_ignore",
    modDevDir        = td,
    ffemDataFile     = data_path,
    covariates       = c("WT", "RACEL"), 
    catCovs          = c("RACEL"),
    numNonFREMThetas = 1,
    numSkipOm        = 0,
    cstrKeepCols     = c("ID", "TIME", "AMT", "DV", "BLQ"),
    outputDir        = out_dir,
    finalModName     = "test_ignore_final",
    bRecodeDichotomous = TRUE,
    quiet            = TRUE
  )
  
  # 5. Assertions: Inspect the final compiled dataset
  final_data <- read.csv(res$data)
  
  # The subject with BLQ == 1 (ID 2) should be completely eradicated
  expect_false(2 %in% final_data$ID)
  
  # The BLQ flag of 1 should nowhere be present
  expect_false(any(final_data$BLQ == 1))
  
  # Only 2 unique IDs should remain (ID 1 and ID 3)
  expect_equal(length(unique(final_data$ID)), 2)
})