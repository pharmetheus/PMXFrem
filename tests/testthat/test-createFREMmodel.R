# tests/testthat/test-createFREMmodel.R

test_that("createFREMmodel input validation catches empty covariates", {
  expect_error(
    createFREMmodel(covariates = character(0)), 
    "At least one covariate must be provided."
  )
})

test_that("createFREMmodel works for a single covariate (Phase 1 only)", {
  modDevDir    <- system.file("extdata", "SimNeb", package = "PMXFrem")
  ffemDataFile <- file.path(modDevDir, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")
  outDir       <- file.path(tempdir(), "frem_test_single")
  dir.create(outDir, showWarnings = FALSE)
  
  res <- suppressWarnings(createFREMmodel(
    modName          = "run30",
    modDevDir        = modDevDir,
    ffemDataFile     = ffemDataFile,
    covariates       = c("WT"),
    numNonFREMThetas = 7,
    cstrKeepCols     = c("ID", "TIME", "AMT", "EVID", "RATE","FOOD","DAY","BLQ"),
    numSkipOm        = 2,
    outputDir        = outDir,
    fremModName      = "test_single",
    quiet            = TRUE
  ))
  
  expect_true(file.exists(res$model))
  expect_true(file.exists(res$data))
  
  expect_false(file.exists(file.path(outDir, "test_single_minimal.mod")))
  expect_false(file.exists(file.path(outDir, "test_single_minimal.ext")))
  expect_false(file.exists(file.path(outDir, "test_single_minimal_data.csv")))
  
  unlink(outDir, recursive = TRUE)
})

test_that("createFREMmodel orchestrates Phase 1 & 2 for multiple covariates", {
  modDevDir    <- system.file("extdata", "SimNeb", package = "PMXFrem")
  ffemDataFile <- file.path(modDevDir, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")
  outDir       <- file.path(tempdir(), "frem_test_multi")
  dir.create(outDir, showWarnings = FALSE)
  
  res <- suppressWarnings(createFREMmodel(
    modName          = "run30",
    modDevDir        = modDevDir,
    ffemDataFile     = ffemDataFile,
    covariates       = c("RACEL", "WT"),
    catCovs          = c("RACEL"),
    numNonFREMThetas = 7,
    cstrKeepCols     = c("ID", "TIME", "AMT", "EVID", "RATE","FOOD","DAY","BLQ"),
    numSkipOm        = 2,
    outputDir        = outDir,
    fremModName      = "test_multi",
    quiet            = TRUE
  ))
  
  expect_true(file.exists(res$model))
  expect_true(file.exists(res$data))
  
  final_df <- read.csv(res$data)
  expect_true("FREMTYPE" %in% names(final_df))
  
  expect_false(file.exists(file.path(outDir, "test_multi_minimal.mod")))
  expect_false(file.exists(file.path(outDir, "test_multi_minimal.ext")))
  expect_false(file.exists(file.path(outDir, "test_multi_minimal_data.csv")))
  
  unlink(outDir, recursive = TRUE)
})

test_that("createFREMmodel respects keepMinimalModel = TRUE", {
  modDevDir    <- system.file("extdata", "SimNeb", package = "PMXFrem")
  ffemDataFile <- file.path(modDevDir, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")
  outDir       <- file.path(tempdir(), "frem_test_keep")
  dir.create(outDir, showWarnings = FALSE)
  
  res <- suppressWarnings(createFREMmodel(
    modName          = "run30",
    modDevDir        = modDevDir,
    ffemDataFile     = ffemDataFile,
    covariates       = c("RACEL", "WT"),
    catCovs          = c("RACEL"),
    numNonFREMThetas = 7,
    cstrKeepCols     = c("ID", "TIME", "AMT", "EVID", "RATE","FOOD","DAY","BLQ"),
    numSkipOm        = 2,
    outputDir        = outDir,
    fremModName      = "test_keep",
    keepMinimalModel = TRUE, 
    quiet            = TRUE
  ))
  
  expect_true(file.exists(res$model))
  expect_true(file.exists(res$data))
  
  expect_true(file.exists(file.path(outDir, "test_keep_minimal.mod")))
  expect_true(file.exists(file.path(outDir, "test_keep_minimal.ext")))
  expect_true(file.exists(file.path(outDir, "test_keep_minimal_data.csv")))
  
  unlink(outDir, recursive = TRUE)
})

test_that("createFREMmodel strictly enforces $DATA IGNORE statements across all phases", {
  td <- withr::local_tempdir()
  out_dir <- file.path(td, "frem_ignore_test")
  dir.create(out_dir, showWarnings = FALSE)
  
  base_mod_path <- file.path(td, "run_ignore.mod")
  data_path <- file.path(td, "data.csv")
  
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
  
  base_ext_path <- file.path(td, "run_ignore.ext")
  writeLines(c(
    "TABLE NO. 1",
    " ITERATION  THETA1  SIGMA(1,1)  OMEGA(1,1)  OBJ",
    " -1000000000  1.00000E+01  1.00000E+00  1.00000E-01  1.00000E+02",
    " -1000000006  0.00000E+00  1.00000E+00  0.00000E+00  0.00000E+00"
  ), base_ext_path)
  
  res <- createFREMmodel(
    modName            = "run_ignore",
    modDevDir          = td,
    ffemDataFile       = data_path,
    covariates         = c("WT", "RACEL"), 
    catCovs            = c("RACEL"),
    numNonFREMThetas   = 1,
    numSkipOm          = 0,
    cstrKeepCols       = c("ID", "TIME", "AMT", "DV", "BLQ"),
    outputDir          = out_dir,
    fremModName        = "test_ignore_final",
    bRecodeDichotomous = TRUE,
    quiet              = TRUE
  )
  
  final_data <- read.csv(res$data)
  expect_false(2 %in% final_data$ID)
  expect_false(any(final_data$BLQ == 1))
  expect_equal(length(unique(final_data$ID)), 2)
})

# -------------------------------------------------------------------------
# NEW DIAGNOSTIC TESTS
# -------------------------------------------------------------------------

setup_diagnostic_env <- function(td, est_lines) {
  data_path <- file.path(td, "data.csv")
  base_mod_path <- file.path(td, "run_diag.mod")
  base_ext_path <- file.path(td, "run_diag.ext")
  
  write.csv(
    data.frame(ID = 1, TIME = 0, AMT = 100, DV = 0, WT = 70, RACEL = 1), 
    data_path, row.names = FALSE, quote = FALSE
  )
  
  writeLines(c(
    "$PROBLEM Diagnostic Test",
    "$INPUT ID TIME AMT DV WT RACEL",
    paste0("$DATA ", basename(data_path)),
    "$PK", " CL = THETA(1) * EXP(ETA(1))",
    "$ERROR", " Y = F + EPS(1)",
    "$THETA 10",
    "$OMEGA BLOCK(1) 0.1",
    "$SIGMA 1 FIX",
    est_lines
  ), base_mod_path)
  
  writeLines(c(
    "TABLE NO. 1",
    " ITERATION  THETA1  SIGMA(1,1)  OMEGA(1,1)  OBJ",
    " -1000000000  1.00000E+01  1.00000E+00  1.00000E-01  1.00000E+02",
    " -1000000006  0.00000E+00  1.00000E+00  0.00000E+00  0.00000E+00"
  ), base_ext_path)
  
  return(data_path)
}

test_that("createFREMmodel warns when SAEM is used", {
  td <- withr::local_tempdir()
  data_path <- setup_diagnostic_env(td, c(
    "$ESTIMATION METHOD=SAEM INTER AUTO=1 NBURN=1000 NITER=500",
    "$ESTIMATION METHOD=IMP NITER=150 PHITYPE=1" 
  ))
  
  expect_warning(
    createFREMmodel(
      modName          = "run_diag",
      modDevDir        = td,
      ffemDataFile     = data_path,
      covariates       = c("WT", "RACEL"), 
      numNonFREMThetas = 1,
      cstrKeepCols     = c("ID", "TIME", "AMT", "DV"), 
      quiet            = TRUE
    ),
    "SAEM currently can not handle missing covariate values correctly"
  )
})

test_that("createFREMmodel warns when IMP/IMPMAP is missing", {
  td <- withr::local_tempdir()
  data_path <- setup_diagnostic_env(td, c(
    "$ESTIMATION METHOD=1 INTER MAXEVAL=9999 PRINT=5",
    "$ESTIMATION METHOD=1 PHITYPE=1"
  ))
  
  expect_warning(
    createFREMmodel(
      modName          = "run_diag",
      modDevDir        = td,
      ffemDataFile     = data_path,
      covariates       = c("WT", "RACEL"), 
      numNonFREMThetas = 1,
      cstrKeepCols     = c("ID", "TIME", "AMT", "DV"),
      quiet            = TRUE
    ),
    "Consider using IMP or IMPMAP to increase the robustness"
  )
})

test_that("createFREMmodel enforces NITER >= 150 for IMP/IMPMAP", {
  td <- withr::local_tempdir()
  data_path_low <- setup_diagnostic_env(td, c(
    "$ESTIMATION METHOD=IMP INTER NITER=100 ISAMPLE=300 PRINT=1 PHITYPE=1"
  ))
  
  expect_warning(
    createFREMmodel(
      modName          = "run_diag",
      modDevDir        = td,
      ffemDataFile     = data_path_low,
      covariates       = c("WT", "RACEL"), 
      numNonFREMThetas = 1,
      cstrKeepCols     = c("ID", "TIME", "AMT", "DV"),
      quiet            = TRUE
    ),
    "Consider increasing NITER to at least 150."
  )
  
  td2 <- withr::local_tempdir()
  data_path_pass <- setup_diagnostic_env(td2, c(
    "$ESTIMATION METHOD=IMP INTER NITER=150 ISAMPLE=300 PRINT=1 PHITYPE=1"
  ))
  
  warnings_emitted <- capture_warnings(
    createFREMmodel(
      modName          = "run_diag",
      modDevDir        = td2,
      ffemDataFile     = data_path_pass,
      covariates       = c("WT", "RACEL"), 
      numNonFREMThetas = 1,
      cstrKeepCols     = c("ID", "TIME", "AMT", "DV"),
      quiet            = TRUE
    )
  )
  expect_false(any(grepl("Consider increasing NITER", warnings_emitted)))
})

test_that("createFREMmodel correctly parses multiline and consecutive $EST blocks (PHITYPE logic)", {
  td <- withr::local_tempdir()
  
  data_path <- setup_diagnostic_env(td, c(
    "$ESTIMATION METHOD=IMP INTER MAXEVAL=9999 ; First step",
    "            NITER=200 ISAMPLE=300",
    "            PHITYPE=0",
    "$ESTIMATION METHOD=IMP EONLY=1 NITER=150 ISAMPLE=1000",
    "            PHITYPE=1 ; Final step corrects PHITYPE"
  ))
  
  expect_silent(
    createFREMmodel(
      modName          = "run_diag",
      modDevDir        = td,
      ffemDataFile     = data_path,
      covariates       = c("WT", "RACEL"), 
      numNonFREMThetas = 1,
      cstrKeepCols     = c("ID", "TIME", "AMT", "DV"),
      quiet            = TRUE
    )
  )
})

test_that("createFREMmodel selectively applies FIX to fully observed covariates (-99 logic)", {
  td <- withr::local_tempdir()
  out_dir <- file.path(td, "frem_fix_test")
  dir.create(out_dir, showWarnings = FALSE)
  
  base_mod_path <- file.path(td, "run_fix.mod")
  data_path <- file.path(td, "data.csv")
  
  # WT is fully observed. AGE has a missing value (-99).
  dummy_data <- data.frame(
    ID   = 1:3,
    TIME = c(0, 0, 0),
    AMT  = c(100, 100, 100),
    DV   = c(0, 0, 0),
    WT   = c(70, 80, 90),     # Fully observed
    AGE  = c(30, -99, 50)     # Missing data present
  )
  write.csv(dummy_data, data_path, row.names = FALSE, quote = FALSE)
  
  writeLines(c(
    "$PROBLEM Fix Test",
    "$INPUT ID TIME AMT DV WT AGE",
    paste0("$DATA ", basename(data_path)),
    "$PK", " CL = THETA(1) * EXP(ETA(1))",
    "$ERROR", " Y = F + EPS(1)",
    "$THETA 10",
    "$OMEGA BLOCK(1) 0.1",
    "$SIGMA 1 FIX"
  ), base_mod_path)
  
  base_ext_path <- file.path(td, "run_fix.ext")
  writeLines(c(
    "TABLE NO. 1",
    " ITERATION  THETA1  SIGMA(1,1)  OMEGA(1,1)",
    " -1000000000  1.0E+01  1.0E+00  1.0E-01",
    " -1000000006  0.0E+00  1.0E+00  0.0E+00"
  ), base_ext_path)
  
  res <- suppressWarnings(createFREMmodel(
    modName          = "run_fix",
    modDevDir        = td,
    ffemDataFile     = data_path,
    covariates       = c("WT", "AGE"), 
    numNonFREMThetas = 1,
    numSkipOm        = 0,
    outputDir        = out_dir,
    fremModName      = "test_fix_model",
    fixTheta         = TRUE,
    quiet            = TRUE
  ))
  
  mod_lines <- readLines(res$model)
  theta_block <- PMXFrem:::findrecord(mod_lines, "\\$THETA", quiet = TRUE)
  
  # WT should be FIXED
  wt_line <- theta_block[grep("TV_WT", theta_block)]
  expect_true(grepl("FIX", wt_line, ignore.case = TRUE), info = "Fully observed covariate WT must be FIXED.")
  
  # AGE should NOT be FIXED
  age_line <- theta_block[grep("TV_AGE", theta_block)]
  expect_false(grepl("FIX", age_line, ignore.case = TRUE), info = "Covariate AGE with missing data must NOT be FIXED.")
})