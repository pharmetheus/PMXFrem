# The initial value of a covariate's mean THETA (PRRS-327). Rounded to 2
# decimals, a small mean was written as 0; by default it now keeps 3
# significant digits, and never fewer than 2 decimals.

## Run createFREMmodel() on run30 with BILI scaled down to a mean near 0.005,
## and return the mean THETAs by covariate. The means are over the subjects
## that pass the filters of run30; a run with roundMeanTo = 6 gives them.
fremMeans <- function(covariates, ...) {
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  dat <- read.csv(file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"))
  dat$BILI <- ifelse(dat$BILI == -99, -99, dat$BILI / 2000)
  td <- withr::local_tempdir()
  dataFile <- file.path(td, "data.csv")
  write.csv(dat, dataFile, row.names = FALSE, quote = FALSE)

  res <- suppressWarnings(createFREMmodel(
    modName = "run30", modDevDir = src, ffemDataFile = dataFile,
    covariates = covariates, numSkipOm = 2,
    cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "RATE", "FOOD", "DV"),
    outputDir = td, fremModName = "frem", quiet = TRUE, ...
  ))
  th <- grep("^\\$THETA.*; *[0-9]+ TV_", readLines(res$model), value = TRUE)
  written <- stats::setNames(
    as.numeric(sub("^\\$THETA\\s+\\(?(-?[0-9.eE+-]+).*$", "\\1", th)),
    sub(".*TV_", "", th)
  )
  written
}

test_that("a small covariate mean keeps 3 significant digits, as the first covariate or a later one", {
  for (covs in list(c("BILI", "AGE"), c("AGE", "BILI"))) {
    exact <- fremMeans(covs, roundMeanTo = 6)
    written <- fremMeans(covs)
    expect_equal(written[["BILI"]], signif(exact[["BILI"]], 3), info = covs[1])
    expect_true(written[["BILI"]] != 0)
  }
})

test_that("a large covariate mean keeps 2 decimals", {
  exact <- fremMeans(c("AGE", "BILI"), roundMeanTo = 6)
  expect_equal(fremMeans(c("AGE", "BILI"))[["AGE"]], round(exact[["AGE"]], 2))
})

test_that("roundMeanTo given as a number still means decimals", {
  exact <- fremMeans(c("AGE", "BILI"), roundMeanTo = 6)
  written <- fremMeans(c("AGE", "BILI"), roundMeanTo = 1)
  expect_equal(written[["AGE"]], round(exact[["AGE"]], 1))
  expect_equal(written[["BILI"]], 0)
})
