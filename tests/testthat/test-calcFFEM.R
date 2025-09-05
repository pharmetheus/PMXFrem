test_that("calcFFEM output is stable across R versions", {

  # --- Setup (remains the same) ---
  modFile <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
  extFile <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
  phiFile <- system.file("extdata/SimNeb/run31.phi", package = "PMXFrem")

  dfExt <- getExt(extFile = extFile)
  dfPhi <- getPhi(phiFile = phiFile)

  # --- Test Case 1: Basic usage ---
  calcFFEMtestout <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = TRUE)

  expect_snapshot_value(stabilize(calcFFEMtestout$Coefficients), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout$FullVars), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout$Expr), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout$Vars), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout$UpperVars), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout$Eta_prim), style = "serialize")

  expect_snapshot_value({
    res <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = TRUE) # Changed to TRUE
    stable_res <- stabilize(res)
    stable_res$Expr <- stabilize(res$Expr)
    stable_res
  }, style = "serialize")

  # --- Test Case 2: Compute eta_prim ---
  calcFFEMtestout2 <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = TRUE,
                               etaFREM = as.numeric(dfPhi[1,]))

  expect_snapshot_value(stabilize(calcFFEMtestout2$Coefficients), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout2$FullVars), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout2$Expr), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout2$Vars), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout2$UpperVars), style = "serialize")

  expect_snapshot_value({
    res <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = TRUE, etaFREM = as.numeric(dfPhi[1,])) # Changed to TRUE
    stable_res <- stabilize(res)
    stable_res$Expr <- stabilize(res$Expr)
    stable_res
  }, style = "serialize")

  # --- Test Case 3: Specify availCov with named covariates ---
  covNames <- getCovNames(modFile)$covNames
  calcFFEMtestout3 <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = TRUE,
                               covNames = covNames, availCov = c("SEX", "WT"))

  expect_snapshot_value(stabilize(calcFFEMtestout3$Coefficients), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout3$FullVars), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout3$Expr), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout3$Vars), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout3$UpperVars), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout3$Eta_prim), style = "serialize")

  expect_snapshot_value({
    res <- calcFFEMtestout3
    stable_res <- stabilize(res)
    stable_res$Expr <- stabilize(res$Expr)
    stable_res
  }, style = "serialize")


  # --- Test Case 4: Specify availCov with generic covariate names ---
  calcFFEMtestout4 <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = TRUE,
                               availCov = c("Cov1", "Cov2"))

  expect_snapshot_value(stabilize(calcFFEMtestout4$Coefficients), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout4$FullVars), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout4$Expr), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout4$Vars), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout4$UpperVars), style = "serialize")
  expect_snapshot_value(stabilize(calcFFEMtestout4$Eta_prim), style = "serialize")

  expect_snapshot_value({
    res <- calcFFEMtestout4
    stable_res <- stabilize(res)
    stable_res$Expr <- stabilize(res$Expr)
    stable_res
  }, style = "serialize")
})


test_that("calcFFEM handles the numParCov = 1 edge case", {
  # Setup: Use the standard run31 ext file
  extFile <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
  dfExt   <- getExt(extFile = extFile)
  modFile <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
  covNames <- getCovNames(modFile)$covNames

  # Test 1: numParCov = 1 with a subset of available covariates
  # This covers the red lines in the first block (lines 130-134)
  result_partial <- calcFFEM(dfExt,
                             numNonFREMThetas = 7,
                             numSkipOm = 2,
                             numParCov = 1, # Force the edge case
                             covNames = covNames,
                             availCov = c("SEX", "WT"),
                             quiet = TRUE)

  expect_snapshot_value(stabilize(result_partial), style = "serialize")

  # Test 2: numParCov = 1 with all available covariates
  # This covers the red lines in the second block (lines 146-148)
  result_all <- calcFFEM(dfExt,
                         numNonFREMThetas = 7,
                         numSkipOm = 2,
                         numParCov = 1, # Force the edge case
                         covNames = covNames,
                         availCov = covNames,
                         quiet = TRUE)

  expect_snapshot_value(stabilize(result_all), style = "serialize")
})

test_that("calcFFEM calculates eta_prim correctly with numSkipOm = 0", {
  # Setup: Use the standard run31 ext and phi files
  extFile <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
  dfExt   <- getExt(extFile = extFile)
  modFile <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
  phiFile <- system.file("extdata/SimNeb/run31.phi", package = "PMXFrem")
  dfPhi   <- getPhi(phiFile = phiFile, warn = FALSE)

  # Get a single individual's ETAs to pass to the function
  fremETA_vector <- as.numeric(subset(dfPhi, ID == 1)[, -1])
  covNames <- getCovNames(modFile)$covNames

  # Test 1: numSkipOm = 0 with a subset of covariates
  # This covers the `if (exists("missCov"))` branch
  result_partial <- calcFFEM(dfExt,
                             numNonFREMThetas = 7,
                             numSkipOm = 0, # Target for this test
                             fremETA = fremETA_vector, # Trigger eta_prim calculation
                             covNames = covNames,
                             availCov = c("SEX", "WT"),
                             quiet = TRUE)

  expect_snapshot_value(stabilize(result_partial), style = "serialize")

  # Test 2: numSkipOm = 0 with all available covariates
  # This covers the `else` branch
  result_all <- calcFFEM(dfExt,
                         numNonFREMThetas = 7,
                         numSkipOm = 0, # Target for this test
                         fremETA = fremETA_vector, # Trigger eta_prim calculation
                         covNames = covNames,
                         availCov = covNames,
                         quiet = TRUE)

  expect_snapshot_value(stabilize(result_all), style = "serialize")
})

test_that("calcFFEM calculates eta_prim correctly with numSkipOm > 0", {
  # Setup: Use the standard run31 ext and phi files
  extFile <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
  dfExt   <- getExt(extFile = extFile)
  modFile <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
  phiFile <- system.file("extdata/SimNeb/run31.phi", package = "PMXFrem")
  dfPhi   <- getPhi(phiFile = phiFile, warn = FALSE)

  # Get a single individual's ETAs to pass to the function
  fremETA_vector <- as.numeric(subset(dfPhi, ID == 1)[, -1])
  covNames <- getCovNames(modFile)$covNames

  # Test 1: numSkipOm > 0 with a subset of covariates
  # This covers the `if (exists("missCov"))` branch
  result_partial <- calcFFEM(dfExt,
                             numNonFREMThetas = 7,
                             numSkipOm = 2, # Target for this test
                             fremETA = fremETA_vector, # Trigger eta_prim calculation
                             covNames = covNames,
                             availCov = c("SEX", "WT"),
                             quiet = TRUE)

  expect_snapshot_value(stabilize(result_partial), style = "serialize")

  # Test 2: numSkipOm > 0 with all available covariates
  # This covers the `else` branch
  result_all <- calcFFEM(dfExt,
                         numNonFREMThetas = 7,
                         numSkipOm = 2, # Target for this test
                         fremETA = fremETA_vector, # Trigger eta_prim calculation
                         covNames = covNames,
                         availCov = covNames,
                         quiet = TRUE)

  expect_snapshot_value(stabilize(result_all), style = "serialize")
})

test_that("calcFFEM covers final quiet=FALSE and empty availCov paths", {
  # Setup: Use the standard run31 ext file
  extFile <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
  dfExt   <- getExt(extFile = extFile)
  modFile <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
  covNames <- getCovNames(modFile)$covNames

  # Test 1: quiet = FALSE to test console output
  expect_output(
    calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = FALSE),
    regexp = "Par1:" # A simple check to confirm something was printed
  )

  # Test 2: Writing to eqFile and omFile
  td <- withr::local_tempdir()
  eq_path <- file.path(td, "eq.txt")
  om_path <- file.path(td, "om.txt")

  calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = FALSE,
           eqFile = eq_path, omFile = om_path)

  expect_true(file.exists(eq_path))
  expect_true(file.info(eq_path)$size > 0)
  expect_true(file.exists(om_path))
  expect_true(file.info(om_path)$size > 0)

  # Test 3: Empty availCov vector
  result_empty_cov <- calcFFEM(dfExt,
                               numNonFREMThetas = 7,
                               numSkipOm = 2,
                               covNames = covNames,
                               availCov = character(0), # Test the empty set case
                               quiet = TRUE)

  expect_snapshot_value(stabilize(result_empty_cov), style = "serialize")
})
