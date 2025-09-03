test_that("buildmatrix works", {

  # --- Setup ---
  fremmodelext    <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
  dfExt           <- getExt(extFile = fremmodelext)
  calcFFEMtestout <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = TRUE)

  # Stabilize the output as before
  stable_output <- stabilize_text_snapshot(buildmatrix(calcFFEMtestout$FullVars))

  # --- Expectation ---
  # Use expect_snapshot_value with the robust "serialize" style.
  testthat::expect_snapshot_value(stable_output, style = "serialize")

})
