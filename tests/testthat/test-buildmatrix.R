test_that("buildmatrix works", {

  numbasethetas   <- 7
  numskipom       <- 2
  fremmodelext    <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
  dfExt           <- getExt(extFile = fremmodelext)
  calcFFEMtestout <- calcFFEM(dfExt, numNonFREMThetas = numbasethetas, numSkipOm = numskipom, quiet = T)

  # Get the raw, text-based output
  raw_output <- buildmatrix(calcFFEMtestout$FullVars)

  # Stabilize it using our new text-processing helper
  stable_output <- stabilize(raw_output)

  # Snapshot the stable text. No variant needed!
  expect_snapshot(stable_output)
})
