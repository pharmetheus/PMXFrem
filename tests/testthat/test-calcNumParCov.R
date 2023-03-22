PMXRenv::activate.unqualified.packages()
test_that("calcNumParCov works", {

  extFile <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
  dfExt   <- getExt(extFile = extFile)

  n <- calcNumParCov(dfExt,numNonFREMThetas = 7,numSkipOm = 2)

  expect_equal(n,3)
})
