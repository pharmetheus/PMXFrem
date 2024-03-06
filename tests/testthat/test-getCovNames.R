
test_that("getCovNames works", {

  ## Non-FREM model
  modFileNF <- system.file("extdata/SimNeb/run30.mod",package="PMXFrem")

  expect_error(getCovNames(modFileNF))

  modFile <- system.file("extdata/SimNeb/run31.mod",package="PMXFrem")
  covs <- getCovNames(modFile)

  expect_equal("list" %in% class(covs),TRUE)
  expect_equal(length(covs),3)
  expect_equal(names(covs)[[1]],"covNames")
  expect_equal(names(covs)[[2]],"polyCatCovs")
  expect_equal(names(covs)[[3]],"orgCovNames")


})
