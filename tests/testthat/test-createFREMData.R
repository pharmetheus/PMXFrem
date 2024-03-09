test_that("createFREMdata works", {

  strFFEMData <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")
# tmpData <- fread(strFFEMData)
  # dfData <- read.table(strFFEMData,sep=",",header=T)
  expect_error(createFREMData())

  # Check the cFremtype argument
    expect_error(
      createFREMData(
        strFFEMData = strFFEMData,
        quiet=TRUE,
        cstrCatCovs = c("NCIL","SEX"),cstrContCovs = "WT",cFremtypes=1:3)
    )

  ## Check that it works with the minimal number of arguments
  tmp1 <- head(createFREMData(strFFEMData = strFFEMData,quiet=TRUE))
  expect_snapshot(tmp1)

  ## Check that it works with continuous covariates

  ## One
  tmp2 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrContCovs = "WT")

  tmp2 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=FALSE,
    cstrKeepCols = c("ID","TIME","AMT","EVID","RATE","FREMTYPE"),
    cSortCols=c("ID","TIME","FREMTYPE"),
    cstrContCovs = "WT")

  tmp2 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=FALSE,
    cstrKeepCols = c("ID","TIME","AMT","EVID","RATE","FREMTYPE"),
    cSortCols=c("ID","TIME","FREMTYPE"),
    cstrCatCovs = "NCI")

  ## Two
  tmp3 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrContCovs = c("WT","BMI"))

  ## When one does not exist
  tmp4 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrContCovs = c("WT","BMI","TEST"))

  expect_snapshot(tmp2)
  expect_snapshot(tmp3)
  expect_snapshot(tmp4)

  ## Check that it works with categorical covariates

  ## One
  tmp5 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = "SEX")

  ## Two
  tmp6 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = c("SEX","SMOK"))

  ## When one does not exist
  tmp7 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = c("SEX","SMOK","TEST"))

  ## Polycothomous
  tmp8 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = c("NCIL"))

  expect_snapshot(tmp5)
  expect_snapshot(tmp6)
  expect_snapshot(tmp7)
  expect_snapshot(tmp8)

  ## Check a combination of covariate types
  tmp10 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = c("SEX","SMOK","NCIL"),
    cstrContCovs = c("WT","BMI","AGE"))

  expect_snapshot(tmp10)

  ## Check the multiple DV feature
  tmp9 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=FALSE,
    cstrDV =c("DV","LNDV"))

  expect_snapshot(tmp9)

  ## Check the multiple DV feature with many covariates
  tmp11 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=FALSE,
    cstrDV =c("DV","LNDV"),
    cstrCatCovs = c("SEX","SMOK","NCIL"),
    cstrContCovs = c("WT","BMI","AGE"))

  expect_snapshot(tmp11)

})
