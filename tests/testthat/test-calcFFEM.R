test_that("calcFFEM output is stable across R versions", {

  # --- Setup (remains the same) ---
  modFile <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
  extFile <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
  phiFile <- system.file("extdata/SimNeb/run31.phi", package = "PMXFrem")

  dfExt <- getExt(extFile = extFile)
  dfPhi <- getPhi(phiFile = phiFile)

  # --- Test Case 1: Basic usage ---
  calcFFEMtestout <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = T)

  # The wrapper function simplifies every call!
  expect_snapshot(stabilize(calcFFEMtestout$Coefficients))
  expect_snapshot(stabilize(calcFFEMtestout$FullVars))
  expect_snapshot(stabilize(calcFFEMtestout$Expr)) # Chooses the text stabilizer automatically
  expect_snapshot(stabilize(calcFFEMtestout$Vars))
  expect_snapshot(stabilize(calcFFEMtestout$UpperVars))
  expect_snapshot(stabilize(calcFFEMtestout$Eta_prim))

  # For the full object, we still need the hybrid approach (see note below)
  expect_snapshot({
    res <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = F)
    stable_res <- stabilize(res) # Stabilizes numeric parts
    stable_res$Expr <- stabilize(res$Expr) # Specifically stabilize the text part
    stable_res
  })

  # --- Test Case 2: Compute eta_prim ---
  calcFFEMtestout2 <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = T,
                               etaFREM = as.numeric(dfPhi[1,]))

  expect_snapshot(stabilize(calcFFEMtestout2$Coefficients))
  expect_snapshot(stabilize(calcFFEMtestout2$FullVars))
  expect_snapshot(stabilize(calcFFEMtestout2$Expr))
  expect_snapshot(stabilize(calcFFEMtestout2$Vars))
  expect_snapshot(stabilize(calcFFEMtestout2$UpperVars))

  expect_snapshot({
    res <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = F, etaFREM = as.numeric(dfPhi[1,]))
    stable_res <- stabilize(res)
    stable_res$Expr <- stabilize(res$Expr)
    stable_res
  })

  # --- Test Case 3: Specify availCov with named covariates ---
  covNames <- getCovNames(modFile)$covNames
  calcFFEMtestout3 <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = T,
                               covNames = covNames, availCov = c("SEX", "WT"))

  expect_snapshot(stabilize(calcFFEMtestout3$Coefficients))
  expect_snapshot(stabilize(calcFFEMtestout3$FullVars))
  expect_snapshot(stabilize(calcFFEMtestout3$Expr))
  expect_snapshot(stabilize(calcFFEMtestout3$Vars))
  expect_snapshot(stabilize(calcFFEMtestout3$UpperVars))
  expect_snapshot(stabilize(calcFFEMtestout3$Eta_prim))

  expect_snapshot({
    res <- calcFFEMtestout3
    stable_res <- stabilize(res)
    stable_res$Expr <- stabilize(res$Expr)
    stable_res
  })


  # --- Test Case 4: Specify availCov with generic covariate names ---
  calcFFEMtestout4 <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = T,
                               availCov = c("Cov1", "Cov2"))

  expect_snapshot(stabilize(calcFFEMtestout4$Coefficients))
  expect_snapshot(stabilize(calcFFEMtestout4$FullVars))
  expect_snapshot(stabilize(calcFFEMtestout4$Expr))
  expect_snapshot(stabilize(calcFFEMtestout4$Vars))
  expect_snapshot(stabilize(calcFFEMtestout4$UpperVars))
  expect_snapshot(stabilize(calcFFEMtestout4$Eta_prim))

  expect_snapshot({
    res <- calcFFEMtestout4
    stable_res <- stabilize(res)
    stable_res$Expr <- stabilize(res$Expr)
    stable_res
  })
})

