# Which covariate THETAs createFREMmodel() fixes (T35). A covariate's THETA is
# fixed when no subject lacks a value for it, wherever the covariate sits in
# `covariates` - phase 1 (the first covariate) and phase 2 (the rest) must
# agree on the rule and on the covariate's name.

## Run createFREMmodel() on run30 and return the covariate THETA lines as a
## named logical: TRUE where the THETA is fixed, named by covariate.
fremFixed <- function(covariates, catCovs = NULL, dataFile = NULL, bRecodeDichotomous = TRUE) {
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  if (is.null(dataFile)) dataFile <- file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")
  td <- withr::local_tempdir()
  res <- suppressWarnings(createFREMmodel(
    modName = "run30", modDevDir = src, ffemDataFile = dataFile,
    covariates = covariates, catCovs = catCovs, numSkipOm = 2,
    cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "RATE", "FOOD", "DV"),
    bRecodeDichotomous = bRecodeDichotomous,
    outputDir = td, fremModName = "frem", quiet = TRUE
  ))
  th <- grep("^\\$THETA.*; *[0-9]+ TV_", readLines(res$model), value = TRUE)
  stats::setNames(grepl("\\bFIX\\b", th), sub(".*TV_", "", th))
}

test_that("a categorical first covariate with missing values is estimated, not fixed", {
  ## 39 subjects have no RACEL.
  fixed <- fremFixed(c("RACEL", "AGE"), catCovs = "RACEL")
  expect_false(fixed[["RACEL_2"]])
  expect_false(fixed[["RACEL_3"]])
})

test_that("each covariate is fixed or estimated the same whatever its position", {
  covs <- c("AGE", "RACEL", "SEX")
  forward <- fremFixed(covs, catCovs = c("RACEL", "SEX"))
  backward <- fremFixed(rev(covs), catCovs = c("RACEL", "SEX"))
  expect_equal(forward[sort(names(forward))], backward[sort(names(backward))])
  ## SEX has a value for every subject; AGE and RACEL do not.
  expect_true(forward[["SEX_2"]])
  expect_false(forward[["AGE"]])
})

test_that("a subject with one missing record but a value elsewhere does not count as missing", {
  ## The FREM data takes each subject's first non-missing value, so a stray
  ## -99 on a later record changes nothing the model sees.
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  dat <- read.csv(file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"))
  firstId <- dat$ID[1]
  rows <- which(dat$ID == firstId)
  dat$SEX[rows[2]] <- -99
  dataFile <- file.path(withr::local_tempdir(), "data.csv")
  write.csv(dat, dataFile, row.names = FALSE, quote = FALSE)

  expect_true(fremFixed(c("AGE", "SEX"), catCovs = "SEX", dataFile = dataFile)[["SEX_2"]])
  expect_true(fremFixed(c("SEX", "AGE"), catCovs = "SEX", dataFile = dataFile)[["SEX_2"]])
})

test_that("a 0/1 covariate that is not recoded keeps its name in phase 1 as in phase 2", {
  ## Phase 1 used to call it SMOK_1, while the data column - and phase 2 -
  ## call it SMOK.
  first <- fremFixed(c("SMOK", "AGE"), catCovs = "SMOK", bRecodeDichotomous = FALSE)
  later <- fremFixed(c("AGE", "SMOK"), catCovs = "SMOK", bRecodeDichotomous = FALSE)
  expect_true("SMOK" %in% names(first))
  expect_false("SMOK_1" %in% names(first))
  expect_equal(first[["SMOK"]], later[["SMOK"]])
})

test_that(".fremCovHasMissing() counts subjects without any value", {
  df <- data.frame(ID = c(1, 1, 2, 2), X = c(-99, 5, 3, 3), Y = c(1, 1, -99, -99))
  expect_false(.fremCovHasMissing(df, "X", strID = "ID", missVal = -99))
  expect_true(.fremCovHasMissing(df, "Y", strID = "ID", missVal = -99))
  df$X[1:2] <- NA
  expect_true(.fremCovHasMissing(df, "X", strID = "ID", missVal = -99))
})
