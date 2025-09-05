test_that("calcParameterEsts correctly extracts and combines parameters", {

  # --- Setup ---
  extFile <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
  parVector <- subset(getExt(extFile = extFile), ITERATION == "-1000000000")

  # Define a standard set of inputs
  thetaNum <- 2:7
  omegaNum <- 1:5
  sigmaNum <- 1:2
  numNonFREMThetas <- 7
  numSkipOm <- 2

  # --- Test Cases ---

  # 1. "Happy Path": Test the standard case with all parameter types included.
  ests_full <- calcParameterEsts(
    parVector = parVector,
    thetaNum = thetaNum,
    omegaNum = omegaNum,
    sigmaNum = sigmaNum,
    numNonFREMThetas = numNonFREMThetas,
    numSkipOm = numSkipOm,
    quiet = TRUE
  )

  expect_type(ests_full, "double")
  expect_length(ests_full, 13) # 6 thetas + 5 omegas + 2 sigmas
  expect_snapshot_value(stabilize(ests_full), style = "serialize")


  # 2. Edge Case: Test with `sigmaNum = NULL`.
  ests_no_sigma <- calcParameterEsts(
    parVector = parVector,
    thetaNum = thetaNum,
    omegaNum = omegaNum,
    sigmaNum = NULL,
    numNonFREMThetas = numNonFREMThetas,
    numSkipOm = numSkipOm,
    quiet = TRUE
  )

  expect_length(ests_no_sigma, 11) # 6 thetas + 5 omegas
  expect_snapshot_value(stabilize(ests_no_sigma), style = "serialize")


  # 3. Edge Case: Test with no external (non-FREM) omegas.
  ests_frem_only_omega <- calcParameterEsts(
    parVector = parVector,
    thetaNum = thetaNum,
    omegaNum = 3:5,
    sigmaNum = sigmaNum,
    numNonFREMThetas = numNonFREMThetas,
    numSkipOm = numSkipOm,
    quiet = TRUE
  )

  expect_length(ests_frem_only_omega, 11) # 6 thetas + 3 omegas + 2 sigmas
  expect_snapshot_value(stabilize(ests_frem_only_omega), style = "serialize")


  # 4. Input Validation: Test that it warns if a requested column is missing.
  parVector_missing_col <- parVector
  parVector_missing_col$THETA3 <- NULL

  expect_warning(
    calcParameterEsts(
      parVector = parVector_missing_col,
      thetaNum = thetaNum, # Still requests THETA3
      omegaNum = omegaNum,
      sigmaNum = sigmaNum,
      numNonFREMThetas = numNonFREMThetas,
      numSkipOm = numSkipOm,
      quiet = TRUE
    ),
    regexp = "Unknown columns"
  )
})
