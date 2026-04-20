test_that("fremParameterTable input validation", {

  # --- Setup for error checking ---
  runno            <- 31
  modDevDir        <- system.file("extdata/SimNeb/",package = "PMXFrem")
  numNonFREMThetas <- 7

  # --- Tests for each stop() condition ---

  # Missing runno and modName
  expect_error(
    fremParameterTable(runno = NULL, modName = NULL, numNonFREMThetas = numNonFREMThetas,
                       thetaNum = 1, omegaNum = 1, sigmaNum = 1),
    regexp = "Either runno or modName has to be specified"
  )

  # Missing availCov
  expect_error(
    fremParameterTable(runno = runno, modDevDir = modDevDir, numNonFREMThetas = numNonFREMThetas,
                       thetaNum = 1, omegaNum = 1, sigmaNum = 1, availCov = NULL),
    regexp = "availCov must be part of the FREM model or 'all'"
  )

  # Non-existent model or ext file
  expect_error(
    fremParameterTable(runno = 99, modDevDir = modDevDir, numNonFREMThetas = numNonFREMThetas,
                       thetaNum = 1, omegaNum = 1, sigmaNum = 1),
    regexp = "Can not find model file"
  )

  # Non-existent RSE file
  expect_error(
    fremParameterTable(runno = runno, modDevDir = modDevDir, numNonFREMThetas = numNonFREMThetas,
                       thetaNum = 1, omegaNum = 1, sigmaNum = 1,
                       includeRSE = TRUE, bsFile = "non_existent_file.csv"),
    regexp = "Can not find the file for RSE calculations"
  )

  # Mismatched label lengths
  expect_error(
    fremParameterTable(runno = runno, modDevDir = modDevDir, numNonFREMThetas = numNonFREMThetas,
                       thetaNum = 1:2, omegaNum = 1, sigmaNum = 1, thetaLabels = "one"),
    regexp = "number of theta labels must be the same"
  )
  expect_error(
    fremParameterTable(runno = runno, modDevDir = modDevDir, numNonFREMThetas = numNonFREMThetas,
                       thetaNum = 1, omegaNum = 1:2, sigmaNum = 1, omegaLabels = "one"),
    regexp = "number of omega labels must be the same"
  )
  expect_error(
    fremParameterTable(runno = runno, modDevDir = modDevDir, numNonFREMThetas = numNonFREMThetas,
                       thetaNum = 1, omegaNum = 1, sigmaNum = 1:2, sigmaLabels = "one"),
    regexp = "number of sigma labels must be the same"
  )

  # Invalid availCov value
  expect_error(
    fremParameterTable(runno = runno, modDevDir = modDevDir, numNonFREMThetas = numNonFREMThetas,
                       thetaNum = 1, omegaNum = 1, sigmaNum = 1, availCov = "NOT_A_COV"),
    regexp = "availCov must be part of the FREM model or 'all'"
  )

})


test_that("fremParameterTable works for standard outputs", {

  set.seed(123)
  runno            <- 31
  modDevDir        <- system.file("extdata/SimNeb/",package = "PMXFrem")
  bsFile           <- system.file("extdata/SimNeb/bs31.dir/raw_results_run31.csv",package = "PMXFrem")
  numNonFREMThetas <- 7
  numSkipOm        <- 2

  r_version_variant <- paste(R.version$major, R.version$minor, sep = ".")

  # All covariates - NO RSE
  expect_snapshot_value(stabilize(fremParameterTable(runno = runno,
                                                     modDevDir             = modDevDir,
                                                     thetaNum              = 1:7,
                                                     omegaNum              = 1:5,
                                                     sigmaNum              = 1:2,
                                                     numNonFREMThetas      = numNonFREMThetas,
                                                     numSkipOm             = numSkipOm,
                                                     availCov              = "all",
                                                     quiet                 = TRUE)), style = "serialize")

  # Selection of covariates - NO RSE
  expect_snapshot_value(stabilize(fremParameterTable(runno= runno,
                                                     modDevDir            = modDevDir,
                                                     thetaNum             = 1:7,
                                                     omegaNum             = 1:5,
                                                     sigmaNum             = 1:2,
                                                     numNonFREMThetas     = numNonFREMThetas,
                                                     numSkipOm            = numSkipOm,
                                                     availCov             = c("SEX","WT"),
                                                     quiet                = TRUE)), style = "serialize")

  # Test providing a pre-read ext file
  pre_read_ext <- getExt(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"))
  expect_snapshot_value(
    stabilize(fremParameterTable(runno = runno,
                                 modDevDir = modDevDir,
                                 dfext = pre_read_ext,
                                 numNonFREMThetas = numNonFREMThetas,
                                 numSkipOm = numSkipOm,
                                 thetaNum = 1:7,
                                 omegaNum = 1:5,
                                 sigmaNum = 1:2,
                                 availCov = "all",
                                 quiet = TRUE)),
    style = "serialize"
  )

  # Test RSE calculation with default SD transformations.
  # The expect_warning is removed, as the function is now robust and does not warn.
  expect_snapshot_value(
    stabilize(fremParameterTable(runno = runno,
                                 modDevDir = modDevDir,
                                 bsFile = bsFile,
                                 includeRSE = TRUE,
                                 omegaSD = TRUE,
                                 sigmaSD = TRUE,
                                 numNonFREMThetas = numNonFREMThetas,
                                 numSkipOm = numSkipOm,
                                 thetaNum = 1:7,
                                 omegaNum = 1:5,
                                 sigmaNum = 1:2,
                                 availCov = "all",
                                 quiet = TRUE)),
    variant = r_version_variant,
    style = "serialize"
  )

  # No SD transformation (Existing test, keep for regression)
  # The expect_warning is removed, as the function is now robust and does not warn.
  expect_snapshot_value(
    stabilize(fremParameterTable(runno            = runno,
                                 modDevDir        = modDevDir,
                                 thetaNum         = 2:7,
                                 omegaNum         = c(1,3,4,5),
                                 sigmaNum         = 1,
                                 includeRSE       = TRUE,
                                 omegaSD          = FALSE,
                                 sigmaSD          = FALSE,
                                 numNonFREMThetas = numNonFREMThetas,
                                 numSkipOm        = numSkipOm,
                                 availCov         = "all",
                                 quiet            = TRUE)),
    variant = r_version_variant,
    style = "serialize"
  )
})

test_that("fremParameterTable orchestrates unified base and coefficient tables", {
  
  modDevDir <- system.file("extdata", "SimNeb", package = "PMXFrem")
  run_frem  <- 31
  bs_file   <- file.path(modDevDir, "bs31.dir/raw_results_run31.csv")
  
  # --- 1. Input Validation for New Arguments ---
  expect_error(
    fremParameterTable(
      runno = run_frem, modDevDir = modDevDir, numNonFREMThetas = 7,
      thetaNum = 2:7, omegaNum = 1:4, sigmaNum = 1,
      availCov = c("AGE", "SEX"), covLabels = c("Age Only")
    ),
    regexp = "covLabels must have the same length as the evaluated covariates"
  )
  
  # --- 2. Fast-Path (Point Estimates Only) ---
  res_point <- fremParameterTable(
    runno            = run_frem,
    modDevDir        = modDevDir,
    numNonFREMThetas = 7,
    numSkipOm        = 2,
    thetaNum         = 2:7,
    omegaNum         = c(1, 3, 4, 5),
    sigmaNum         = 1,
    includeRSE       = FALSE,
    quiet            = TRUE
  )
  
  # Assertions
  expect_true(all(c("parameterTable", "coefficientTable_long", "coefficientTable_wide") %in% names(res_point)))
  expect_true("Par1" %in% names(res_point$coefficientTable_wide)) # Default parameter name fallback
  expect_false("RSE (%)" %in% names(res_point$coefficientTable_long))
  expect_false(grepl("\\(", res_point$coefficientTable_wide$Par1[1])) # Should just be a number string
  
  # --- 3. Full Engine (With RSE, Custom Labels, and Subsetting) ---
  res_full <- fremParameterTable(
    runno            = run_frem,
    modDevDir        = modDevDir,
    numNonFREMThetas = 7,
    numSkipOm        = 2,
    thetaNum         = 2:7,
    omegaNum         = c(1, 3, 4, 5),
    sigmaNum         = 1,
    includeRSE       = TRUE,
    bsFile           = bs_file,
    n                = 5, # Low for fast CI/CD
    availCov         = c("AGE", "SEX"),
    parNames         = c("CL_L_h", "V_L", "MAT_h"),
    covLabels        = c("Age_yrs", "Sex"),
    seed             = 42, # Testing ... pass-through
    quiet            = TRUE
  )
  
  # Assertions for custom labels and dimensions
  expect_equal(names(res_full$coefficientTable_wide), c("Covariate", "CL_L_h", "V_L", "MAT_h"))
  expect_equal(res_full$coefficientTable_wide$Covariate, c("Age_yrs", "Sex"))
  
  # Assertions for uncertainty and formatting
  expect_true("RSE (%)" %in% names(res_full$coefficientTable_long))
  expect_type(res_full$coefficientTable_wide$CL_L_h, "character")
  expect_true(grepl("\\(", res_full$coefficientTable_wide$CL_L_h[1])) # Asserts string concatenation: "0.145 (12.4%)"
  
  # --- 4. Reproducibility Check ---
  res_repro <- fremParameterTable(
    runno            = run_frem,
    modDevDir        = modDevDir,
    numNonFREMThetas = 7,
    numSkipOm        = 2,
    thetaNum         = 2:7,
    omegaNum         = c(1, 3, 4, 5),
    sigmaNum         = 1,
    includeRSE       = TRUE,
    bsFile           = bs_file,
    n                = 5,
    availCov         = c("AGE", "SEX"),
    parNames         = c("CL_L_h", "V_L", "MAT_h"),
    covLabels        = c("Age_yrs", "Sex"),
    seed             = 42, 
    quiet            = TRUE
  )
  
  # Asserting the seed successfully controlled the PMXForest::getSamples call
  expect_equal(res_full$Samples, res_repro$Samples)
})