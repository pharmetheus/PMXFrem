# A non-default missVal is honoured when a dichotomous covariate is recoded
# to 0/1 (T45). The recoding wrote a literal -99 for a missing value, which
# a later `!= missVal` filter does not recognise as missing.

test_that("createFREMData() recodes with a non-default missVal", {
  dfFFEM <- data.frame(
    ID = c(1, 1, 2, 2, 3, 3), TIME = c(0, 1, 0, 1, 0, 1), EVID = c(1, 0, 1, 0, 1, 0),
    AMT = c(100, 0, 100, 0, 100, 0), DV = c(0, 5, 0, 6, 0, 7), SEX = c(1, 1, 2, 2, -999, -999)
  )
  out <- file.path(withr::local_tempdir(), "frem.csv")
  suppressWarnings(createFREMData(
    strFFEMData = dfFFEM, strFREMDataFileName = out, strID = "ID",
    cstrKeepCols = c("ID", "TIME", "EVID", "AMT", "DV"), cstrCatCovs = "SEX",
    missVal = -999, bRecodeDichotomous = TRUE, quiet = TRUE
  ))
  frem <- read.csv(out)
  covRows <- frem[frem$FREMTYPE > 0, ]
  expect_setequal(covRows$DV, c(0, 1))
  expect_false(3 %in% covRows$ID)
})

test_that("recoding a dichotomous covariate keeps a non-default missVal missing", {
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  dat <- read.csv(file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"))
  dat[dat == -99] <- -999
  ## SEX has a value for every subject; take it away from the first three.
  ids <- unique(dat$ID)[1:3]
  dat$SEX[dat$ID %in% ids] <- -999
  expect_false(any(dat == -99))
  dataFile <- file.path(withr::local_tempdir(), "data.csv")
  write.csv(dat, dataFile, row.names = FALSE, quote = FALSE)

  for (covs in list(c("AGE", "SEX"), c("SEX", "AGE"))) {
    td <- withr::local_tempdir()
    res <- suppressWarnings(createFREMmodel(
      modName = "run30", modDevDir = src, ffemDataFile = dataFile,
      covariates = covs, catCovs = "SEX", missVal = -999, bRecodeDichotomous = TRUE,
      numSkipOm = 2, cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "RATE", "FOOD", "DV"),
      outputDir = td, fremModName = "frem", quiet = TRUE
    ))
    frem <- read.csv(res$data)
    ## No covariate observation may be a stray -99.
    expect_false(any(frem$DV[frem$FREMTYPE > 0] == -99), label = paste(covs, collapse = ","))
    sexTheta <- grep("TV_SEX_2", readLines(res$model), value = TRUE)
    mean <- as.numeric(sub("^\\$THETA\\s+\\(?([-0-9.eE]+).*", "\\1", sexTheta))
    expect_true(mean >= 0 && mean <= 1, label = paste("TV_SEX_2 =", mean, "with", paste(covs, collapse = ",")))
  }
})
