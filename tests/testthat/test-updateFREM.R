test_that("updateFREM can remove covariates from FREM models", {

  tmp <- updateFREM(
    strFREMModel      = system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"),
    strFREMData       = system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem"),
    strFFEMData       = system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"),
    cstrRemoveCov     = c("SEX"),
    cstrCatCovsToAdd  = NULL,
    cstrContCovsToAdd = NULL,
    strID             = "ID",
    strNewFREMData    = "frem_dataset_noSEX.csv",
    numNonFREMThetas  = 7,
    numSkipOm         = 2,
    bWriteData        = FALSE,
    bWriteMod         = TRUE,
    quiet             = F,
    bWriteFIX         = TRUE,
    sortFREMDataset  = c("ID","TIME","FREMTYPE"),
    cstrKeepCols = c("ID","TIME","AMT","EVID","RATE","DV","FOOD","FREMTYPE"))

  expect_snapshot(tmp)
})

test_that("updateFREM can add covariates to FREM models", {

  tmp <- updateFREM(
    strFREMModel      = system.file("extdata/SimNeb/run31_new.mod", package = "PMXFrem"),
    strFREMData       = system.file("extdata/SimNeb/frem_dataset_noSEX.csv", package = "PMXFrem"),
    strFFEMData       = system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"),
    cstrRemoveCov     = NULL,
    cstrCatCovsToAdd  = "SEX",
    cstrContCovsToAdd = NULL,
    strID             = "ID",
    strNewFREMData    = "frem_dataset_withSEX.csv",
    numNonFREMThetas  = 7,
    numSkipOm         = 2,
    numParCov         = 3,
    bWriteData        = FALSE,
    bWriteMod         = FALSE,
    quiet             = F,
    bWriteFIX         = TRUE,
    sortFREMDataset  = c("ID","TAD","FREMTYPE"),
    cstrKeepCols = c("ID","TAD","AMT","EVID","RATE","DV","FREMTYPE"))

  expect_snapshot(tmp)
})

test_that("updateFREM can add update initial estimates in FREM models", {

  tmp <- updateFREM(strFREMModel      = system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"),
                    numNonFREMThetas  = 7,
                    numSkipOm         = 2,
                    bWriteData        = FALSE,
                    bWriteMod         = FALSE,
                    quiet             = F,
                    strUpdateType     = "NoData")

  expect_snapshot(tmp)
})

