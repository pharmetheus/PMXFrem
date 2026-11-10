# Where createFREMmodel() writes the FREM data set, and what its $DATA says
# (PRRS-327): by default <fremModName>_data.csv next to the model; with
# fremDataFile, that path, written into $DATA as given.

fremDataRecord <- function(covariates, outputDir, ...) {
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  res <- suppressWarnings(createFREMmodel(
    modName = "run30", modDevDir = src,
    dataFile = file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"),
    covariates = covariates, numSkipOm = 2,
    cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "RATE", "FOOD", "DV"),
    outputDir = outputDir, fremModName = "frem", quiet = TRUE, ...
  ))
  mod <- readLines(res$model)
  list(res = res, data = grep("^\\$DATA", mod, value = TRUE))
}

test_that("by default the FREM data set is <fremModName>_data.csv next to the model", {
  td <- withr::local_tempdir()
  out <- fremDataRecord(c("AGE", "WT"), td)
  expect_equal(out$data, "$DATA frem_data.csv IGNORE=@")
  expect_true(file.exists(file.path(td, "frem_data.csv")))
})

test_that("a relative fremDataFile is written relative to the model and into $DATA as given", {
  for (covs in list("AGE", c("AGE", "WT"))) {
    td <- withr::local_tempdir()
    out <- fremDataRecord(covs, file.path(td, "Model"), fremDataFile = "../Data/frem.csv")
    expect_equal(out$data, "$DATA ../Data/frem.csv IGNORE=@", info = length(covs))
    expect_true(file.exists(file.path(td, "Data", "frem.csv")), info = length(covs))
    expect_false(file.exists(file.path(td, "Model", "frem_data.csv")), info = length(covs))
    expect_equal(normalizePath(out$res$data), normalizePath(file.path(td, "Data", "frem.csv")))
  }
})

test_that("an absolute fremDataFile is used as it is", {
  td <- withr::local_tempdir()
  target <- file.path(withr::local_tempdir(), "frem.csv")
  out <- fremDataRecord(c("AGE", "WT"), td, fremDataFile = target)
  expect_equal(out$data, paste("$DATA", target, "IGNORE=@"))
  expect_true(file.exists(target))
})
