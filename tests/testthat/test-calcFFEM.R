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
