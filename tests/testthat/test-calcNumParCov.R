test_that("calcNumParCov works", {

  extFile <- system.file("extdata/SimVal/run9.ext", package = "PMXFrem")
  dfExt   <- getExt(extFile = extFile)
  
  n <- calcNumParCov(dfExt,numNonFREMThetas = 9,numSkipOm = 2)
  
  expect_equal(n,4)
})
