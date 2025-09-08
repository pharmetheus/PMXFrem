test_that("removeFremCovariates correctly removes a continuous covariate", {
  # Create a sample FREM dataset
  sample_dfFREM <- data.frame(
    ID = rep(1, 4),
    FREMTYPE = c(0, 100, 200, 300) # DV, WT, AGE, SEX
  )
  
  # Create a sample initial state
  initialState <- list(
    dfFREM = sample_dfFREM,
    covnames = list(
      covNames = c("WT", "AGE", "SEX"),
      orgCovNames = c("WT", "AGE", "SEX"),
      polyCatCovs = character(0)
    ),
    theta = 1:6, # 3 base + 3 cov
    omegaMatrix = diag(1:6),
    thetaFix = rep(0, 6),
    numOmega = 6,
    numTheta = 6,
    numNonFREMThetas = 3,
    numParCov = 3, # Let's assume 3 base params in omega
    numSkipOm = 0,
    iFremTypeIncrease = 100
  )
  
  # Call the function to remove "AGE"
  resultState <- removeFremCovariates(initialState, cstrRemoveCov = "AGE", quiet = TRUE)
  
  # --- CORRECTED EXPECTATIONS ---
  # 1. The original FREMTYPE for SEX (300) should be gone.
  expect_false(300 %in% resultState$dfFREM$FREMTYPE)
  # 2. The FREMTYPE for SEX should now be 200 due to re-numbering.
  #    This replaces the original FREMTYPE 200 that belonged to AGE.
  expect_true(200 %in% resultState$dfFREM$FREMTYPE)
  
  # Covariate names list should be updated
  expect_equal(resultState$covnames$covNames, c("WT", "SEX"))
  # Parameter vectors/matrices should be smaller
  expect_length(resultState$theta, 5)
  expect_equal(dim(resultState$omegaMatrix), c(5, 5))
  
  # Snapshot the entire result for comprehensive checking
  expect_snapshot_value(resultState, style = "serialize")
})

test_that("removeFremCovariates does nothing if cstrRemoveCov is NULL", {
  initialState <- list(dfFREM = data.frame(ID = 1)) # Simple state
  
  resultState <- removeFremCovariates(initialState, cstrRemoveCov = NULL, quiet = TRUE)
  
  # The returned state should be identical to the initial state
  expect_equal(resultState, initialState)
})