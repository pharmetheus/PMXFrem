test_that("createFREMdata works", {

  strFFEMData <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")
  
  # Suppress the warning about default II/SS columns missing, we just want the error
  suppressWarnings(expect_error(createFREMData()))
  
  # Check the cFremtype argument
  suppressWarnings(expect_error(
    createFREMData(
      strFFEMData = strFFEMData,
      quiet = TRUE,
      cstrKeepCols = "ID", # <-- Add this so it doesn't look for the default II/SS
      cstrCatCovs = c("NCIL","SEX"), 
      cstrContCovs = "WT", 
      cFremtypes = 1:3
    )
  ))
  
  ## Check that it works with the minimal number of arguments
  tmp1 <- createFREMData(strFFEMData = strFFEMData, quiet=TRUE,cstrKeepCols = "ID")
  expect_snapshot_value(stabilize(as.data.frame(head(tmp1))), style = "serialize")

  ## Check that it works with continuous covariates

  # One continuous covariate
  tmp2_a <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrContCovs = "WT",cstrKeepCols = "ID")
  expect_snapshot_value(stabilize(as.data.frame(head(tmp2_a))), style = "serialize")

  # One continuous covariate with other arguments
  tmp2_b <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE, # Changed to TRUE
    cstrKeepCols = c("ID","TIME","AMT","EVID","RATE"),
    cSortCols=c("ID","TIME","FREMTYPE"),
    cstrContCovs = "WT")
  expect_snapshot_value(stabilize(as.data.frame(head(tmp2_b))), style = "serialize")

  # Two continuous covariates
  tmp3 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrContCovs = c("WT","BMI"),cstrKeepCols = "ID")
  expect_snapshot_value(stabilize(as.data.frame(head(tmp3))), style = "serialize")

  # When one continuous covariate does not exist
  tmp4 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrContCovs = c("WT","BMI","TEST"),cstrKeepCols = "ID")
  expect_snapshot_value(stabilize(as.data.frame(head(tmp4))), style = "serialize")

  ## Check that it works with categorical covariates

  # One categorical covariate
  tmp5 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    bRecodeDichotomous = TRUE, # <-- Added to override strict validation
    cstrCatCovs = "SEX",cstrKeepCols = "ID")
  expect_snapshot_value(stabilize(as.data.frame(head(tmp5))), style = "serialize")
  
  # Two categorical covariates
  tmp6 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    bRecodeDichotomous = TRUE, # <-- Added
    cstrCatCovs = c("SEX","SMOK"),cstrKeepCols = "ID")
  expect_snapshot_value(stabilize(as.data.frame(head(tmp6))), style = "serialize")
  
  # When one categorical covariate does not exist
  tmp7 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    bRecodeDichotomous = TRUE, # <-- Added
    cstrCatCovs = c("SEX","SMOK","TEST"),cstrKeepCols = "ID")
  expect_snapshot_value(stabilize(as.data.frame(head(tmp7))), style = "serialize")
  
  # Polycothomous categorical covariate
  tmp8 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = c("NCIL"),cstrKeepCols = "ID")
  expect_snapshot_value(stabilize(as.data.frame(head(tmp8))), style = "serialize")
  
  ## Check a combination of covariate types
  tmp10 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    bRecodeDichotomous = TRUE, # <-- Added
    cstrCatCovs = c("SEX","SMOK","NCIL"),
    cstrContCovs = c("WT","BMI","AGE"),cstrKeepCols = "ID")
  expect_snapshot_value(stabilize(as.data.frame(head(tmp10))), style = "serialize")
  
  ## Check the multiple DV feature
  tmp9 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE, 
    cstrDV =c("DV","LNDV"),cstrKeepCols = "ID")
  expect_snapshot_value(stabilize(as.data.frame(head(tmp9))), style = "serialize")
  
  ## Check the multiple DV feature with many covariates
  tmp11 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE, 
    bRecodeDichotomous = TRUE, # <-- Added
    cstrDV =c("DV","LNDV"),
    cstrCatCovs = c("SEX","SMOK","NCIL"),
    cstrContCovs = c("WT","BMI","AGE"),cstrKeepCols = "ID")
  expect_snapshot_value(stabilize(as.data.frame(head(tmp11))), style = "serialize")

})

################################################################################
# NEW TESTS TO COVER RED LINES
################################################################################

test_that("createFREMData handles edge cases and errors", {

  strFFEMData <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")

  # Test error when input file doesn't exist (L92)
  expect_error(
    createFREMData(strFFEMData = "non_existent_file.csv"),
    "Cannot find FFEM dataset"
  )

  # Test error for mismatched cFremtypes length (L137)
  expect_error(
    createFREMData(strFFEMData = strFFEMData, cstrContCovs = "WT", cFremtypes = c(0),cstrKeepCols = "ID"), # Asks for 1 DV and 1 Cov, but provides only 1 fremtype
    "The number of fremtypes are not the same as the number of frem variables"
  )

  # Test file writing functionality (L252)
  tmp_file <- tempfile(fileext = ".csv")
  createFREMData(strFFEMData = strFFEMData, strFREMDataFileName = tmp_file,cstrKeepCols = "ID")
  expect_true(file.exists(tmp_file))
  unlink(tmp_file) # Clean up

  # Test verbose output for missing covariates (L171 & L197)
  expect_output(
    createFREMData(strFFEMData = strFFEMData, cstrContCovs = "FAKE_CONT", quiet = FALSE,cstrKeepCols = "ID"),
    "not found in FFEM dataset"
  )
  expect_output(
    createFREMData(strFFEMData = strFFEMData, cstrCatCovs = "FAKE_CAT", quiet = FALSE,cstrKeepCols = "ID"),
    "not found in FFEM dataset"
  )

  # Test dichotomous categorical covariate WITHOUT recoding 
  # This triggers the 'else' block for dichotomous covariates
  # We MUST use a native 0/1 covariate here to pass strict validation
  df_native <- data.table::fread(strFFEMData, data.table = FALSE)
  df_native$SEX_01 <- ifelse(df_native$SEX == 1, 0, 1)
  
  dichot_no_recode <- createFREMData(
    strFFEMData = df_native,
    cstrCatCovs = "SEX_01",
    bRecodeDichotomous = FALSE,
    cstrKeepCols = "ID"
  )
  expect_snapshot_value(stabilize(as.data.frame(head(dichot_no_recode, 15))), style = "serialize")
  
  # Test providing only categorical covariates (no continuous) 
  cat_only <- createFREMData(
    strFFEMData = strFFEMData,
    cstrCatCovs = "SEX",
    cstrContCovs = NULL,
    bRecodeDichotomous = TRUE, # <-- Added because SEX is 1/2 in the raw file
    cstrKeepCols = "ID"
  )
  expect_snapshot_value(stabilize(as.data.frame(head(cat_only, 15))), style = "serialize")


  # Test scenario where covariates exist but have only missing (-99) values (L184 & L231)
  df_orig <- data.table::fread(strFFEMData, data.table = FALSE)
  df_mod <- df_orig
  # Make all WT and SEX values missing
  df_mod$WT <- -99
  df_mod$SEX <- -99
  tmp_csv_missing <- tempfile(fileext = ".csv")
  write.csv(df_mod, tmp_csv_missing, row.names = FALSE)

  # Test ONLY the missing continuous covariate case
  expect_output(
    createFREMData(
      strFFEMData = tmp_csv_missing,
      cstrContCovs = "WT",
      quiet = FALSE,
      cstrKeepCols = "ID"
    ),
    "No non-missing covariate values for WT. Skipping this covariate."
  )

  # Test ONLY the missing categorical covariate case
  expect_output(
    createFREMData(
      strFFEMData = tmp_csv_missing,
      cstrCatCovs = "SEX",
      quiet = FALSE,
      cstrKeepCols = "ID"
    ),
    "No non-missing covariate values for SEX. Skipping this covariate."
  )

  unlink(tmp_csv_missing) # Clean up

})

################################################################################
# NEW TESTS TO COVER FINAL RED LINES from package_coverage()
################################################################################

test_that("createFREMData covers all edge cases from coverage report", {

  # 1. Create a custom, temporary dataset to control all conditions
  test_data <- data.frame(
    ID = c(1, 1, 2, 2, 3, 3),
    TIME = c(0, 1, 0, 1, 0, 1),
    DV = c(10, 11, 12, 13, 14, 15),
    DV_ALL_MISSING = -99,
    WT_ALL_MISSING = -99,
    RACE = c(1, 1, 2, 2, 3, 3), # Polycotomous covariate with 3 levels
    RACE_ALL_MISSING = -99
  )

  tmp_test_csv <- tempfile(fileext = ".csv")
  write.csv(test_data, tmp_test_csv, row.names = FALSE)


  # 2. Test for a DV column with only missing values
  expect_warning(
    createFREMData(
      strFFEMData = tmp_test_csv,
      cstrDV = "DV_ALL_MISSING",
      cstrKeepCols = "ID" # The temp data only has ID and the DV
    ),
    "Note that it might be inconsistencies in DV fremtypes"
  )

  # 3. Test recoding of a polycothomous covariate
  result_poly <- createFREMData(strFFEMData = tmp_test_csv, cstrCatCovs = "RACE",cstrKeepCols = "ID")

  # ROBUST TEST: Check that the DV column contains the recoded 0/1 values
  # The function should create one row per ID for each new dummy variable
  frem_100 <- result_poly[result_poly$FREMTYPE == 100, ]
  frem_200 <- result_poly[result_poly$FREMTYPE == 200, ]

  # The expected values are known based on the 3 unique IDs
  expect_equal(frem_100$DV, c(0, 1, 0))
  expect_equal(frem_200$DV, c(0, 0, 1))


  # 4. Test covariates with only missing values
  expect_output(
    createFREMData(strFFEMData = tmp_test_csv, cstrContCovs = "WT_ALL_MISSING", quiet = FALSE,cstrKeepCols = "ID"),
    "No non-missing covariate values for WT_ALL_MISSING"
  )
  expect_output(
    createFREMData(strFFEMData = tmp_test_csv, cstrCatCovs = "RACE_ALL_MISSING", quiet = FALSE,cstrKeepCols = "ID"),
    "No non-missing covariate values for RACE_ALL_MISSING"
  )

  # # 5. Test the 'missing columns' logic
  # result_missing_col <- createFREMData(
  #   strFFEMData = tmp_test_csv,
  #   cstrKeepCols = c("ID", "TIME", "DV", "FREMTYPE", "IMAGINARY_COLUMN")
  # )
  # expect_true("IMAGINARY_COLUMN" %in% names(result_missing_col))
  # expect_true(all(is.na(result_missing_col$IMAGINARY_COLUMN)))


  unlink(tmp_test_csv) # Clean up the temporary file
})

test_that("createFREMData warns and soft-drops missing keep_cols (allows Phase 2 generation)", {
  ffem_data_path <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem") 
  
  invalid_keep_cols <- c("ID", "TIME", "SEX_2") # SEX_2 doesn't exist natively
  
  # Action & Assertion: Expect a warning, not an error
  expect_warning(
    result <- createFREMData(
      strFFEMData = ffem_data_path,
      cstrKeepCols = invalid_keep_cols,
      quiet = TRUE
    ),
    regexp = "missing from the input data and will be ignored during this phase"
  )
  
  # Assert that the function successfully continued and dropped the missing column
  expect_true("ID" %in% names(result))
  expect_true("TIME" %in% names(result))
  expect_false("SEX_2" %in% names(result)) # Dropped locally
})

test_that("createFREMData retains original row order for tied times (Fix C)", {
  td <- withr::local_tempdir()
  tmp_csv <- file.path(td, "tied_time_data.csv")
  
  # Create a dataset where a Dose (EVID=1) and Obs (EVID=0) share the exact same TIME
  test_df <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 0),
    EVID = c(1, 0), # Dose is original row 1, Obs is original row 2
    AMT = c(100, 0),
    DV = c(0, 10.5), 
    WT = c(70, 70)
  )
  write.csv(test_df, tmp_csv, row.names = FALSE)
  
  result <- createFREMData(
    strFFEMData = tmp_csv,
    cstrContCovs = "WT",
    cstrKeepCols = c("ID", "TIME", "EVID", "AMT"),
    quiet = TRUE
  )
  
  # Because we sort by ORIG_ROW_IDX (ascending) and then FREMTYPE (ascending):
  # Original Row 1 (Dose) should generate FREMTYPE 0, then FREMTYPE 100.
  # Original Row 2 (Obs) should generate FREMTYPE 0, then FREMTYPE 100.
  
  # Assert the strict order of EVIDs (both dose rows MUST precede both obs rows)
  expect_equal(result$EVID, c(1, 0, 0))
  
  # Assert the strict order of FREMTYPEs within those blocks
  expect_equal(result$FREMTYPE, c(0, 100, 0))
  
  # Clean up
  unlink(tmp_csv)
})

test_that("createFREMData fails fast on 1/2 covariates when bRecodeDichotomous is FALSE", {
  test_data <- data.frame(
    ID = c(1, 2), TIME = c(0, 1), EVID = c(0, 0), AMT = c(0, 0), RATE = c(0, 0), DV = c(10, 12),
    SEX = c(1, 2)
  )
  
  expect_error(
    createFREMData(
      strFFEMData = test_data,
      cstrCatCovs = "SEX",
      bRecodeDichotomous = FALSE,
      cstrKeepCols = "ID"
    ),
    regexp = "Strict Validation Error: Dichotomous covariate 'SEX' is coded as 1/2"
  )
})

test_that("allowNon01 = TRUE permits 1/2 covariates to pass through untouched", {
  # Setup: SEX is coded as 1/2
  sample_data <- data.frame(
    ID = 1:4, TIME = 0, DV = 10, EVID = 0, AMT = 0, RATE = 0,
    SEX = c(1, 2, 1, 2)
  )
  
  tmp_csv <- tempfile(fileext = ".csv")
  write.csv(sample_data, tmp_csv, row.names = FALSE)
  
  # Action: Override the kill-switch without triggering recoding
  result <- createFREMData(
    strFFEMData = tmp_csv,
    cstrCatCovs = "SEX", 
    cstrKeepCols = "ID",
    bRecodeDichotomous = FALSE,
    allowNon01 = TRUE,
    quiet = TRUE
  )
  
  # In Phase 1's long FREM output, the first covariate is appended as FREMTYPE 100
  frem_sex <- result[result$FREMTYPE == 100, ]
  
  # Assertions: The DV column for those rows must retain the raw 1/2 scale
  expect_equal(frem_sex$DV, c(1, 2, 1, 2))
  
  unlink(tmp_csv)
})

test_that("createFREMData retains dosing records (EVID != 0) when DV is '.' or NA", {
  
  # 1. Setup Data
  # We construct a mixed dataset to test all edge cases of the DV filter logic:
  # Row 1: Dose with DV as literal string "."
  # Row 2: Dose with DV as NA (standard read.csv behavior)
  # Row 3: Valid Observation
  # Row 4: Invalid Observation (DV = -99)
  # Row 5: Invalid Observation (DV = NA)
  dfFFEM <- data.frame(
    ID = c(1, 1, 1, 1, 1),
    TIME = c(0, 0, 12, 24, 36),
    AMT = c(100, 50, 0, 0, 0),
    EVID = c(1, 1, 0, 0, 0),
    DV = c(".", NA, "10.5", "-99", NA)
  )
  
  # 2. Action
  result_df <- createFREMData(
    strFFEMData = dfFFEM,
    cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "DV"),
    cstrSetToZero = "AMT",
    cstrDV = "DV",
    quiet = TRUE
  )
  
  # 3. Assertions
  base_data <- result_df[result_df$FREMTYPE == 0, ]
  
  # We expect exacty rows 1, 2, and 3 to survive. 
  # Rows 4 and 5 must be safely dropped.
  expect_equal(nrow(base_data), 3)
  expect_equal(base_data$TIME, c(0, 0, 12))
  expect_equal(base_data$EVID, c(1, 1, 0))
  expect_equal(base_data$AMT, c(100, 50, 0))
  
  # Ensure the DVs were preserved exactly as they entered
  expect_equal(base_data$DV[1], ".")
  expect_true(is.na(base_data$DV[2]))
  expect_equal(base_data$DV[3], "10.5")
})

test_that("createFREMData correctly handles keepDoseOnlySubjects toggle", {
  
  # 1. Setup Mock Data
  # ID 1: Standard subject (has a dose and an EVID=0 observation)
  # ID 2: Dose-only subject (has a dose, but NO EVID=0 observation)
  dfFFEM <- data.frame(
    ID = c(1, 1, 2),
    TIME = c(0, 12, 0),
    AMT = c(100, 0, 100),
    EVID = c(1, 0, 1),
    DV = c(".", "10.5", ".")
  )
  
  # --- Action 1: Default Legacy Behavior (FALSE) ---
  res_false <- createFREMData(
    strFFEMData = dfFFEM,
    cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "DV"),
    cstrSetToZero = "AMT",
    cstrDV = "DV", 
    keepDoseOnlySubjects = FALSE,
    quiet = TRUE
  )
  
  # Assertions for FALSE (Option 1: PsN legacy mode)
  # ID 2 should be completely eradicated from the dataset
  expect_false(2 %in% res_false$ID) 
  expect_true(1 %in% res_false$ID)
  
  # Only ID 1's 2 rows should exist
  expect_equal(nrow(res_false), 2)
  
  
  # --- Action 2: New Retention Behavior (TRUE) ---
  res_true <- createFREMData(
    strFFEMData = dfFFEM,
    cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "DV"),
    cstrSetToZero = "AMT",
    cstrDV = "DV",
    keepDoseOnlySubjects = TRUE,
    quiet = TRUE
  )
  
  # Assertions for TRUE (Option 2: Statistically complete mode)
  # ID 2 must be preserved
  expect_true(2 %in% res_true$ID) 
  expect_true(1 %in% res_true$ID)
  
  # Base records should include ID 1 (2 rows) + ID 2 (1 dose row)
  expect_equal(nrow(res_true), 3) 
})