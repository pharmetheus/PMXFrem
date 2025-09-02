test_that("updateFREMmodel can remove covariates from FREM models", {
  # Setup a self-contained temporary environment for the test
  td <- withr::local_tempdir()

  # Copy necessary input files into the temporary directory
  file.copy(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"), td)

  # Define paths relative to the temporary directory
  model_path <- file.path(td, "run31.mod")
  frem_data_path <- file.path(td, "frem_dataset.dta")
  ffem_data_path <- file.path(td, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")
  new_data_path <- file.path(td, "frem_dataset_noSEX.csv")
  new_model_path <- file.path(td, "run31_new.mod")

  tmp <- updateFREMmodel(
    strFREMModel      = model_path,
    strFREMData       = frem_data_path,
    strFFEMData       = ffem_data_path,
    cstrRemoveCov     = c("SEX","WT"),
    strNewFREMData    = new_data_path,
    numNonFREMThetas  = 7,
    numSkipOm         = 2,
    bWriteData        = TRUE,
    bWriteMod         = TRUE,
    quiet             = TRUE,
    bWriteFIX         = TRUE,
    sortFREMDataset  = c("ID","TIME","FREMTYPE"),
    cstrKeepCols = c("ID","TIME","AMT","EVID","RATE","DV","FOOD","FREMTYPE"))

  # Explicitly test that the side-effect (writing files) worked as expected
  expect_true(file.exists(new_data_path))
  expect_true(file.exists(new_model_path))

  expect_snapshot_value(stabilize_model_paths(tmp), style = "serialize")
})

test_that("updateFREMmodel can add covariates to FREM models", {
  # Setup a self-contained temporary environment
  td <- withr::local_tempdir()

  # Copy necessary input files
  file.copy(system.file("extdata/SimNeb/run31_new.mod", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/run31_new.ext", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/frem_dataset_noSEX.csv", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"), td)

  # Define paths relative to the temporary directory
  model_path <- file.path(td, "run31_new.mod")
  frem_data_path <- file.path(td, "frem_dataset_noSEX.csv")
  ffem_data_path <- file.path(td, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")

  tmp <- updateFREMmodel(
    strFREMModel      = model_path,
    strFREMData       = frem_data_path,
    strFFEMData       = ffem_data_path,
    cstrRemoveCov     = NULL,
    cstrCatCovsToAdd  = "SEX",
    cstrContCovsToAdd = "WT",
    strID             = "ID",
    strNewFREMData    = "frem_dataset_withSEX.csv",
    numNonFREMThetas  = 7,
    numSkipOm         = 2,
    numParCov         = 3,
    bWriteData        = FALSE,
    bWriteMod         = FALSE,
    quiet             = TRUE,
    bWriteFIX         = TRUE,
    sortFREMDataset  = c("ID","TAD","FREMTYPE"),
    cstrKeepCols = c("ID","TAD","AMT","EVID","RATE","DV","FREMTYPE"))

  expect_snapshot_value(stabilize(tmp), style = "serialize")
})

test_that("updateFREMmodel can update initial estimates in FREM models", {
  # Setup a self-contained temporary environment
  td <- withr::local_tempdir()

  # Copy necessary input files
  file.copy(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"), td)

  # Define path relative to the temporary directory
  model_path <- file.path(td, "run31.mod")

  tmp <- updateFREMmodel(strFREMModel      = model_path,
                         numNonFREMThetas  = 7,
                         numSkipOm         = 2,
                         bWriteData        = FALSE,
                         bWriteMod         = FALSE,
                         quiet             = TRUE,
                         strUpdateType     = "NoData")

  expect_snapshot_value(stabilize(tmp), style = "serialize")
})
