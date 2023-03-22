PMXRenv::activate.unqualified.packages()
test_that("buildmatrix works", {

  numbasethetas   <- 7
  numskipom       <- 2
  fremmodelext    <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
  dfExt           <- getExt(extFile = fremmodelext)
  calcFFEMtestout <- calcFFEM(dfExt,numNonFREMThetas = numbasethetas,numSkipOm = numskipom,quiet = T)

  expect_snapshot(buildmatrix(calcFFEMtestout$FullVars))
})
