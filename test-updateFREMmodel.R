test_that("updateFREMmodel can remove covariates from FREM models", {

  strNewFREMData_path <- tempfile(fileext = ".csv")

  tmp <- updateFREMmodel(
    strFREMModel      = system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"),
    strFREMData       = system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem"),
    strFFEMData       = system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"),
    cstrRemoveCov     = c("SEX","WT"),
    strNewFREMData    = strNewFREMData_path,
    numNonFREMThetas  = 7,
    numSkipOm         = 2,
    bWriteData        = TRUE,
    bWriteMod         = TRUE,
    quiet             = TRUE,
    bWriteFIX         = TRUE,
    sortFREMDataset  = c("ID","TIME","FREMTYPE"),
    cstrKeepCols = c("ID","TIME","AMT","EVID","RATE","DV","FOOD","FREMTYPE"))

  expect_snapshot_value(stabilize_model_paths(tmp), style = "serialize")
})

test_that("updateFREMmodel can add covariates to FREM models", {

  tmp <- updateFREMmodel(
    strFREMModel      = system.file("extdata/SimNeb/run31_new.mod", package = "PMXFrem"),
    strFREMData       = system.file("extdata/SimNeb/frem_dataset_noSEX.csv", package = "PMXFrem"),
    strFFEMData       = system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"),
    cstrCatCovsToAdd  = "SEX",
    cstrContCovsToAdd = "WT",
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

  tmp <- updateFREMmodel(strFREMModel      = system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"),
                         numNonFREMThetas  = 7,
                         numSkipOm         = 2,
                         bWriteData        = FALSE,
                         bWriteMod         = FALSE,
                         quiet             = TRUE,
                         strUpdateType     = "NoData")

  expect_snapshot_value(stabilize(tmp), style = "serialize")
})


test_that("updateFREMmodel input checks and edge cases", {

  # Setup paths and data
  mod31_path <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
  frem_data_path <- system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem")
  ffem_data_path <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")

  frem_data <- as.data.frame(data.table::fread(frem_data_path, showProgress = FALSE))
  ffem_data <- as.data.frame(readr::read_csv(ffem_data_path, show_col_types = FALSE))

  ffem_data$ODV <- 0

  # Define a "safe" set of columns that we know exist in our test data
  safe_keep_cols <- c("ID", "TIME", "AMT", "DV", "EVID", "RATE", "FREMTYPE")

  # Test for input validation
  expect_error(updateFREMmodel(strFREMModel = mod31_path, strFREMData = ""), regexp = "must be set to dataset")
  expect_error(updateFREMmodel(strFREMModel = mod31_path, strFREMData = frem_data, strFFEMData = ""), regexp = "must be set to dataset")
  expect_error(updateFREMmodel(strFREMModel = "non_existent_model.mod", strUpdateType = "NoData"), regexp = "demands that .*\\.ext.* exists")

  # Test model parsing when .ext is missing
  temp_mod_path <- tempfile(fileext = ".mod")
  file.copy(mod31_path, temp_mod_path)
  expect_error(
    updateFREMmodel(
      strFREMModel = temp_mod_path, strFREMData = frem_data, strFFEMData = ffem_data,
      numNonFREMThetas = 7, numParCov = NULL
    ),
    regexp = "numParCov.* needs to be specified"
  )

  # Test for non-existent data files
  expect_error(
    updateFREMmodel(strFREMModel = mod31_path, strFREMData = "fake.csv", strFFEMData = ffem_data, numNonFREMThetas = 7),
    regexp = "Cannot find FREM dataset"
  )
  expect_error(
    updateFREMmodel(strFREMModel = mod31_path, strFREMData = frem_data, strFFEMData = "fake.csv", numNonFREMThetas = 7),
    regexp = "Cannot find FFEM dataset"
  )

  # Test `overrideExistingCheck = TRUE`
  expect_output(
    updateFREMmodel(
      strFREMModel = mod31_path, strFREMData = frem_data, strFFEMData = ffem_data,
      numNonFREMThetas = 7, cstrCatCovsToAdd = "NCIL", overrideExistingCheck = TRUE,
      quiet = FALSE, bWriteData = FALSE, bWriteMod = FALSE,
      cstrDV = c("DV", "ODV"), cstrKeepCols = safe_keep_cols
    ),
    regexp = "Identifying new covariate: NCIL_1"
  )

  # Test remove polychotomous categorical covariate
  expect_output(
    updateFREMmodel(strFREMModel = mod31_path, strFREMData = frem_data, strFFEMData = ffem_data,
                    numNonFREMThetas = 7, cstrRemoveCov = "NCIL", quiet = FALSE,
                    bWriteData = FALSE, bWriteMod = FALSE,
                    cstrDV = c("DV", "ODV"), cstrKeepCols = safe_keep_cols),
    regexp = "Found a categorical covariate to remove NCIL"
  )

  # Test adding a new DV when one already exists
  ffem_data_with_odv <- ffem_data
  ffem_data_with_odv$ODV <- log(ffem_data$DV + 1)

  res_add_dv <- updateFREMmodel(
    strFREMModel = mod31_path, strFREMData = frem_data, strFFEMData = ffem_data_with_odv,
    numNonFREMThetas = 7, cstrDV = c("DV", "ODV"),
    bWriteData = FALSE, bWriteMod = FALSE, cstrKeepCols = safe_keep_cols
  )
  expect_true(any(res_add_dv$data$FREMTYPE == 1))

  # Test no observed values for an existing covariate in new individuals
  ffem_data_new_ids <- ffem_data
  ffem_data_new_ids$ID <- ffem_data_new_ids$ID + max(ffem_data$ID)
  ffem_data_new_ids$WT <- -99

  expect_output(
    updateFREMmodel(
      strFREMModel = mod31_path, strFREMData = frem_data, strFFEMData = ffem_data_new_ids,
      numNonFREMThetas = 7, quiet = FALSE, bWriteData = FALSE, bWriteMod = FALSE,
      cstrDV = c("DV", "ODV"), cstrKeepCols = safe_keep_cols
    ),
    regexp = "No observed covariate values for WT"
  )

  # Test add new individual with existing categorical covariate data
  ffem_data_new_id_cat <- ffem_data
  ffem_data_new_id_cat$ID <- ffem_data_new_id_cat$ID + max(ffem_data$ID)

  res_new_id_cat <- updateFREMmodel(
    strFREMModel = mod31_path, strFREMData = frem_data, strFFEMData = ffem_data_new_id_cat,
    numNonFREMThetas = 7, quiet = TRUE, bWriteData = FALSE, bWriteMod = FALSE,
    cstrDV = c("DV", "ODV"), cstrKeepCols = safe_keep_cols
  )
  expect_true(max(res_new_id_cat$data$ID) > max(frem_data$ID))
  expect_true(any(res_new_id_cat$data$FREMTYPE == 1000))

  # Test warning when FREMTYPE is not in cstrKeepCols
  temp_csv_path <- tempfile()
  expect_warning(
    updateFREMmodel(
      strFREMModel = mod31_path, strFREMData = frem_data, strFFEMData = ffem_data,
      numNonFREMThetas = 7, bWriteData = TRUE, bWriteMod = FALSE, strNewFREMData = temp_csv_path,
      cstrKeepCols = c("ID", "DV"), cstrDV = c("DV", "ODV")
    ),
    regexp = "No FREMTYPE available in dataset"
  )

})
