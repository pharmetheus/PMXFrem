test_that("createFREMdata works", {

  strFFEMData <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")

  expect_error(createFREMData())

  # Check the cFremtype argument
  expect_error(
    createFREMData(
      strFFEMData = strFFEMData,
      quiet=TRUE,
      cstrCatCovs = c("NCIL","SEX"), cstrContCovs = "WT", cFremtypes=1:3)
  )

  ## Check that it works with the minimal number of arguments
  tmp1 <- head(createFREMData(strFFEMData = strFFEMData, quiet=TRUE))
  expect_snapshot(stabilize(tmp1))

  ## Check that it works with continuous covariates

  # One continuous covariate
  tmp2_a <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrContCovs = "WT")
  expect_snapshot(stabilize(tmp2_a))

  # One continuous covariate with other arguments
  tmp2_b <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=FALSE,
    cstrKeepCols = c("ID","TIME","AMT","EVID","RATE","FREMTYPE"),
    cSortCols=c("ID","TIME","FREMTYPE"),
    cstrContCovs = "WT")
  expect_snapshot(stabilize(tmp2_b))

  # Two continuous covariates
  tmp3 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrContCovs = c("WT","BMI"))
  expect_snapshot(stabilize(tmp3))

  # When one continuous covariate does not exist
  tmp4 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrContCovs = c("WT","BMI","TEST"))
  expect_snapshot(stabilize(tmp4))

  ## Check that it works with categorical covariates

  # One categorical covariate
  tmp5 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = "SEX")
  expect_snapshot(stabilize(tmp5))

  # Two categorical covariates
  tmp6 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = c("SEX","SMOK"))
  expect_snapshot(stabilize(tmp6))

  # When one categorical covariate does not exist
  tmp7 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = c("SEX","SMOK","TEST"))
  expect_snapshot(stabilize(tmp7))

  # Polycothomous categorical covariate
  tmp8 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = c("NCIL"))
  expect_snapshot(stabilize(tmp8))

  ## Check a combination of covariate types
  tmp10 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = c("SEX","SMOK","NCIL"),
    cstrContCovs = c("WT","BMI","AGE"))
  expect_snapshot(stabilize(tmp10))

  ## Check the multiple DV feature
  tmp9 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=FALSE,
    cstrDV =c("DV","LNDV"))
  expect_snapshot(stabilize(tmp9))

  ## Check the multiple DV feature with many covariates
  tmp11 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=FALSE,
    cstrDV =c("DV","LNDV"),
    cstrCatCovs = c("SEX","SMOK","NCIL"),
    cstrContCovs = c("WT","BMI","AGE"))
  expect_snapshot(stabilize(tmp11))

})
