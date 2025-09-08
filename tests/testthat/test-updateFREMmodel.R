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
  new_data_path <- file.path(td, "frem_dataset_noSEXWT.csv")
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

test_that("refactored updateFREMmodel output is self-consistent and parsable", {
  # This test verifies that the output from the refactored function is a valid
  # input for other functions in the package, by removing a covariate and then
  # using the output files to add new individuals.

  # Setup a self-contained temporary environment
  td <- withr::local_tempdir()

  # Copy necessary input files
  file.copy(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem"), td)
  ffem_data_path <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")
  file.copy(ffem_data_path, td)

  # Define initial paths
  model_path <- file.path(td, "run31.mod")
  frem_data_path <- file.path(td, "frem_dataset.dta")
  ffem_data_path_local <- file.path(td, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")

  # --- Round 1: Remove covariates ---
  result1 <- updateFREMmodel(
    strFREMModel      = model_path,
    strFREMData       = frem_data_path,
    strFFEMData       = ffem_data_path_local,
    cstrRemoveCov     = c("SEX", "WT"),
    numNonFREMThetas  = 7,
    numSkipOm         = 2,
    bWriteData        = FALSE,
    bWriteMod         = FALSE,
    quiet             = TRUE,
    cstrKeepCols = c("ID","TIME","AMT","EVID","RATE","DV","FOOD","FREMTYPE")
  )

  # --- Test 1: Check if getCovNames can parse the output model ---
  round1_model_path <- file.path(td, "round1_output.mod")
  writeLines(result1$model, round1_model_path)
  covs_from_output <- getCovNames(modFile = round1_model_path)
  expect_false("WT" %in% covs_from_output$covNames)
  expect_true("AGE" %in% covs_from_output$covNames)

  # --- Test 2: Use Round 1 output to add new individuals ---

  # DEFINITIVE FIX: As you suggested, create a dummy .ext file for the second run
  # by copying the original. This provides the necessary initial estimates.
  round1_ext_path <- file.path(td, "round1_output.ext")
  file.copy(file.path(td, "run31.ext"), round1_ext_path)

  ffem_df <- data.table::fread(ffem_data_path_local)
  ffem_new_ids <- ffem_df[1:100, ]
  ffem_new_ids$ID <- ffem_new_ids$ID + max(result1$data$ID)

  result2 <- NULL
  expect_no_error({
    result2 <- updateFREMmodel(
      strFREMModel      = round1_model_path,
      strFREMData       = result1$data,
      strFFEMData       = ffem_new_ids,
      strNewFREMData    = file.path(td, "round2_data.csv"),
      numNonFREMThetas  = 7,
      numSkipOm         = 2,
      # numParCov is no longer needed, as it will be read from the .ext file
      bWriteData        = FALSE,
      bWriteMod         = FALSE,
      quiet             = TRUE,
      cstrKeepCols      = c("ID","TIME","AMT","EVID","RATE","DV","FOOD","FREMTYPE")
    )
  })

  # Final check: verify that the new individuals were actually added
  expect_true(any(result2$data$ID > max(result1$data$ID)))
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
  new_data_path <- file.path(td, "frem_dataset_withSEX.csv")
  ffem_data_path <- file.path(td, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")

  tmp <- updateFREMmodel(
    strFREMModel      = model_path,
    strFREMData       = frem_data_path,
    strFFEMData       = ffem_data_path,
    cstrRemoveCov     = NULL,
    cstrCatCovsToAdd  = "SEX",
    cstrContCovsToAdd = "WT",
    strID             = "ID",
    strNewFREMData    = new_data_path,
    numNonFREMThetas  = 7,
    numSkipOm         = 2,
    numParCov         = 3,
    bWriteData        = TRUE,
    bWriteMod         = TRUE,
    quiet             = TRUE,
    bWriteFIX         = TRUE,
    sortFREMDataset  = c("ID","TIME","FREMTYPE"),
    cstrKeepCols = c("ID","TIME","AMT","EVID","RATE","DV","FOOD","FREMTYPE"))

  expect_snapshot_value(stabilize_model_paths(tmp), style = "serialize")
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

test_that("updateFREMmodel handles missing files correctly", {
  # Setup a self-contained temporary environment
  td <- withr::local_tempdir()

  # Copy only the .mod file (no .ext file) to trigger the parsing path
  mod31_path <- file.path(td, "run31.mod")
  file.copy(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), mod31_path)

  # Test for stop() when .ext is missing and numParCov is not specified
  expect_error(
    updateFREMmodel(
      strFREMModel = mod31_path,
      strFREMData = data.frame(),
      strFFEMData = data.frame(),
      # FIX: Add strNewFREMData to satisfy the new validation check
      strNewFREMData = "new_data.csv",
      numNonFREMThetas = 7,
      numParCov = NULL # This is the error we actually want to test
    ),
    regexp = "If no \\*.ext file exist, the number of parameters.*needs to be specified!"
  )

  # Test for stop() when the FREM data file does not exist
  expect_error(
    updateFREMmodel(
      strFREMModel = mod31_path,
      strFREMData = file.path(td, "non_existent_frem_data.csv"),
      strFFEMData = system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"),
      numNonFREMThetas = 7,
      numParCov = 3
    ),
    regexp = "Cannot find FREM dataset"
  )
})

test_that("updateFREMmodel correctly removes a polycotomous covariate", {
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

  # <<< FIX: Define and use safe column vectors to prevent warnings >>>
  safe_keep_cols <- c("ID", "TIME", "AMT", "DV", "EVID", "RATE", "FREMTYPE")
  safe_set_to_zero <- c("AMT", "EVID", "RATE")

  # Call the function to remove the 'NCIL' covariate by its base name
  result <- updateFREMmodel(
    strFREMModel      = model_path,
    strFREMData       = frem_data_path,
    strFFEMData       = ffem_data_path,
    cstrRemoveCov     = "NCIL", # Target for this test
    numNonFREMThetas  = 7,
    numSkipOm         = 2,
    bWriteData        = FALSE,
    bWriteMod         = FALSE,
    quiet             = TRUE,
    cstrKeepCols      = safe_keep_cols,
    cstrSetToZero     = safe_set_to_zero
  )

  # A snapshot is the best way to verify the complex changes to both
  # the data and the model text
  expect_snapshot_value(stabilize_model_paths(result), style = "serialize")
})

test_that("updateFREMmodel correctly adds a polycotomous covariate", {
  # Setup a self-contained temporary environment
  td <- withr::local_tempdir()

  # Copy necessary input files
  file.copy(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem"), td)

  # Load the FFEM data so we can modify it
  ffem_data_path <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")
  ffem_df <- data.table::fread(ffem_data_path)

  # Create a new 3-level categorical covariate for the test
  ffem_df$SITE <- rep(c(101, 102, 103), length.out = nrow(ffem_df))

  # Define paths relative to the temporary directory
  model_path <- file.path(td, "run31.mod")
  frem_data_path <- file.path(td, "frem_dataset.dta")

  # <<< FIX: Define and use safe column vectors to prevent warnings >>>
  safe_keep_cols <- c("ID", "TIME", "AMT", "DV", "EVID", "RATE", "FREMTYPE")
  safe_set_to_zero <- c("AMT", "EVID", "RATE")

  # Call the function to add the new 'SITE' covariate
  result <- updateFREMmodel(
    strFREMModel      = model_path,
    strFREMData       = frem_data_path,
    strFFEMData       = ffem_df, # Use the modified data frame
    cstrCatCovsToAdd  = "SITE", # Target for this test
    numNonFREMThetas  = 7,
    numSkipOm         = 2,
    bWriteData        = FALSE,
    bWriteMod         = FALSE,
    quiet             = TRUE,
    cstrKeepCols      = safe_keep_cols,
    cstrSetToZero     = safe_set_to_zero
  )

  # A snapshot is the best way to verify the complex changes to the model text
  expect_snapshot_value(stabilize_model_paths(result), style = "serialize")
})

test_that("updateFREMmodel is robust to data.table inputs", {
  # This test verifies that the function works correctly when passed
  # data.table objects instead of just file paths or data.frames.
  td <- withr::local_tempdir()

  # Copy necessary input files
  file.copy(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"), td)

  # Load data explicitly as data.tables
  frem_data_path <- system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem")
  frem_dt <- data.table::fread(frem_data_path)

  ffem_data_path <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")
  ffem_dt <- data.table::fread(ffem_data_path)

  # <<< FIX: Define and use safe column vectors to prevent warnings >>>
  safe_keep_cols <- c("ID", "TIME", "AMT", "DV", "EVID", "RATE", "FREMTYPE")
  safe_set_to_zero <- c("AMT", "EVID", "RATE")

  # Call the function with data.table objects
  # The main call in this test
  result <- updateFREMmodel(
    strFREMModel      = file.path(td, "run31.mod"),
    strFREMData       = frem_dt,
    strFFEMData       = ffem_dt,
    # FIX: Add strNewFREMData to satisfy the new validation check
    strNewFREMData    = file.path(td, "robust_data_new.csv"),
    cstrRemoveCov     = "SEX",
    numNonFREMThetas  = 7,
    numSkipOm         = 2,
    bWriteData        = FALSE,
    bWriteMod         = FALSE,
    quiet             = TRUE,
    cstrKeepCols      = safe_keep_cols,
    cstrSetToZero     = safe_set_to_zero
  )
  # A simple check to ensure the function completed and returned the correct structure
  expect_type(result, "list")
  expect_named(result, c("data", "model"))
  expect_s3_class(result$data, "data.frame")
})

test_that("updateFREMmodel correctly adds new individuals", {
  # Setup a self-contained temporary environment
  td <- withr::local_tempdir()

  # Copy necessary input files
  file.copy(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"), td)

  frem_data_path <- file.path(td, "frem_dataset.dta")
  file.copy(system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem"), frem_data_path)

  # Load the FREM and FFEM data
  frem_df <- data.table::fread(frem_data_path)
  ffem_data_path <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")
  ffem_df <- data.table::fread(ffem_data_path)

  # To simulate new individuals, we'll take the first 100 rows of the FFEM data
  # and give them new, unique IDs that don't exist in the FREM data.
  ffem_new_ids <- ffem_df[1:100, ]
  ffem_new_ids$ID <- ffem_new_ids$ID + max(frem_df$ID)

  # Combine the original FFEM data with the new-ID data
  ffem_combined <- rbind(ffem_df, ffem_new_ids)

  # <<< FIX: Add the ODV column to match the structure of the other test data
  ffem_combined$ODV <- 0

  # Define paths relative to the temporary directory
  model_path <- file.path(td, "run31.mod")

  safe_keep_cols <- c("ID", "TIME", "AMT", "DV", "EVID", "RATE", "FREMTYPE")
  safe_set_to_zero <- c("AMT", "EVID", "RATE")

  # Call the function with the data containing new individuals
  result <- updateFREMmodel(
    strFREMModel      = model_path,
    strFREMData       = frem_df,
    strFFEMData       = ffem_combined,
    # FIX: Add the required output file name argument
    strNewFREMData    = file.path(td, "frem_data_new_ids.csv"),
    numNonFREMThetas  = 7,
    numSkipOm         = 2,
    bWriteData        = FALSE,
    bWriteMod         = FALSE,
    quiet             = TRUE,
    cstrKeepCols      = safe_keep_cols,
    cstrSetToZero     = safe_set_to_zero
  )

  # Verify that new individuals were actually added
  expect_true(max(result$data$ID) > max(frem_df$ID))

  # A snapshot is the best way to verify the complex changes to the data
  expect_snapshot_value(stabilize_model_paths(result), style = "serialize")
})


test_that("updateFREMmodel correctly adds a new DV type", {
  # Setup a self-contained temporary environment
  td <- withr::local_tempdir()

  # Copy necessary input files
  file.copy(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"), td)

  frem_data_path <- file.path(td, "frem_dataset.dta")
  file.copy(system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem"), frem_data_path)

  # Load the FFEM data so we can add a new DV column to it
  ffem_data_path <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")
  ffem_df <- data.table::fread(ffem_data_path)

  # Create a new DV column (e.g., LNDV for a log-transformed DV)
  ffem_df$LNDV <- log(ffem_df$DV + 1e-6)

  # Define paths relative to the temporary directory
  model_path <- file.path(td, "run31.mod")

  # <<< FIX: Add the ODV column to match the structure of the other test data
  ffem_df$ODV <- 0

  safe_keep_cols <- c("ID", "TIME", "AMT", "DV", "EVID", "RATE", "FREMTYPE")
  safe_set_to_zero <- c("AMT", "EVID", "RATE")

  # Call the function, specifying both the original DV and the new LNDV
  result <- updateFREMmodel(
    strFREMModel      = model_path,
    strFREMData       = frem_data_path,
    strFFEMData       = ffem_df, # Use the modified data frame
    numNonFREMThetas  = 7,
    numSkipOm         = 2,
    bWriteData        = FALSE,
    bWriteMod         = FALSE,
    quiet             = TRUE,
    cstrKeepCols      = safe_keep_cols,
    cstrSetToZero     = safe_set_to_zero,
    cstrDV            = c("DV", "LNDV") # Key argument for this test
  )

  # Check that new rows with FREMTYPE=1 (for the new DV) have been added
  expect_true(any(result$data$FREMTYPE == 1))

  # Snapshot the result to verify the new data structure
  expect_snapshot_value(stabilize_model_paths(result), style = "serialize")
})

test_that("updateFREMmodel sorting and final writing logic is covered", {
  # Setup a self-contained temporary environment
  td <- withr::local_tempdir()

  # Copy necessary input files
  file.copy(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"), td)

  frem_data_path <- file.path(td, "frem_dataset.dta")
  file.copy(system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem"), frem_data_path)

  ffem_data_path <- file.path(td, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")
  file.copy(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"), ffem_data_path)

  model_path <- file.path(td, "run31.mod")

  safe_keep_cols <- c("ID", "TIME", "AMT", "DV", "EVID", "RATE", "FREMTYPE")
  safe_set_to_zero <- c("AMT", "EVID", "RATE")

  # Test 1: Custom sort with cstrKeepCols = NULL
  result_custom_sort <- updateFREMmodel(
    strFREMModel      = model_path,
    strFREMData       = frem_data_path,
    strFFEMData       = ffem_data_path,
    numNonFREMThetas  = 7,
    numSkipOm         = 2,
    bWriteData        = FALSE,
    bWriteMod         = FALSE,
    quiet             = TRUE,
    sortFREMDataset   = c("DV"), # Custom sort by DV
    cstrKeepCols      = NULL     # Keep all columns
  )
  # A snapshot of the head will confirm the sort order
  expect_snapshot_value(stabilize(head(result_custom_sort$data)), style = "serialize")


  # Test 2: Verify that sortFREMDataset = NULL is now an error
  expect_error(
    results <- updateFREMmodel(
      strFREMModel      = model_path,
      strFREMData       = frem_data_path,
      strFFEMData       = ffem_data_path,
      numNonFREMThetas  = 7,
      numSkipOm         = 2,
      quiet             = TRUE,
      sortFREMDataset   = NULL, # This should now cause a controlled error
      # Add safe cstrKeepCols to prevent unrelated warnings
      cstrKeepCols      = c("ID","TIME","AMT","EVID","RATE","DV","FREMTYPE")
    ),
    # Check for the new, specific error message from our validation check
    regexp = "'sortFREMDataset' must be a character vector"
  )


  # Test 3: Warning when FREMTYPE is excluded from written data
  new_data_path <- file.path(td, "data_without_fremtype.csv")

  expect_warning(
    updateFREMmodel(
      strFREMModel      = model_path,
      strFREMData       = frem_data_path,
      strFFEMData       = ffem_data_path,
      numNonFREMThetas  = 7,
      numSkipOm         = 2,
      bWriteData        = TRUE, # Must be TRUE to trigger the check
      bWriteMod         = FALSE,
      quiet             = TRUE,
      strNewFREMData    = new_data_path,
      cstrKeepCols      = c("ID", "TIME", "DV") # Explicitly exclude FREMTYPE
    ),
    regexp = "No FREMTYPE available in dataset"
  )
})

