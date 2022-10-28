test_that("getCovNames works", {
  
  modFile <- system.file("extdata/SimVal/run9.mod",package="PMXFrem")
  covs <- getCovNames(modFile)
  
  expect_equal("list" %in% class(covs),TRUE)
  expect_equal(length(covs),3)
  expect_equal(names(covs)[[1]],"covNames")
  expect_equal(names(covs)[[2]],"polyCatCovs")
  expect_equal(names(covs)[[3]],"orgCovNames")
  
  
})
