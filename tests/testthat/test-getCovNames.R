
test_that("getCovNames works", {
  ## Non-FREM model
  modFileNF <- system.file("extdata/SimNeb/run30.mod", package = "PMXFrem")

  expect_error(getCovNames(modFileNF))

  modFile <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
  covs <- getCovNames(modFile)

  expect_equal("list" %in% class(covs), TRUE)
  expect_equal(length(covs), 3)
  expect_equal(names(covs)[[1]], "covNames")
  expect_equal(names(covs)[[2]], "polyCatCovs")
  expect_equal(names(covs)[[3]], "orgCovNames")
})

test_that("a covariate whose own name holds an underscore is not taken for a binarized level", {
  td <- local_run31_underscore()
  cn <- getCovNames(file.path(td, "run31.mod"))
  expect_true("BL_BILI" %in% cn$orgCovNames)
  expect_false("BL" %in% cn$orgCovNames)
  expect_false("BL_BILI" %in% cn$polyCatCovs)
  expect_setequal(cn$polyCatCovs, c("RACEL_2", "RACEL_3", "NCIL_1", "NCIL_2"))
})
