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
  tmp1 <- createFREMData(strFFEMData = strFFEMData, quiet=TRUE)
  expect_snapshot_value(stabilize(as.data.frame(head(tmp1))), style = "serialize")

  ## Check that it works with continuous covariates

  # One continuous covariate
  tmp2_a <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrContCovs = "WT")
  expect_snapshot_value(stabilize(as.data.frame(head(tmp2_a))), style = "serialize")

  # One continuous covariate with other arguments
  tmp2_b <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE, # Changed to TRUE
    cstrKeepCols = c("ID","TIME","AMT","EVID","RATE","FREMTYPE"),
    cSortCols=c("ID","TIME","FREMTYPE"),
    cstrContCovs = "WT")
  expect_snapshot_value(stabilize(as.data.frame(head(tmp2_b))), style = "serialize")

  # Two continuous covariates
  tmp3 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrContCovs = c("WT","BMI"))
  expect_snapshot_value(stabilize(as.data.frame(head(tmp3))), style = "serialize")

  # When one continuous covariate does not exist
  tmp4 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrContCovs = c("WT","BMI","TEST"))
  expect_snapshot_value(stabilize(as.data.frame(head(tmp4))), style = "serialize")

  ## Check that it works with categorical covariates

  # One categorical covariate
  tmp5 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = "SEX")
  expect_snapshot_value(stabilize(as.data.frame(head(tmp5))), style = "serialize")

  # Two categorical covariates
  tmp6 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = c("SEX","SMOK"))
  expect_snapshot_value(stabilize(as.data.frame(head(tmp6))), style = "serialize")

  # When one categorical covariate does not exist
  tmp7 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = c("SEX","SMOK","TEST"))
  expect_snapshot_value(stabilize(as.data.frame(head(tmp7))), style = "serialize")

  # Polycothomous categorical covariate
  tmp8 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = c("NCIL"))
  expect_snapshot_value(stabilize(as.data.frame(head(tmp8))), style = "serialize")

  ## Check a combination of covariate types
  tmp10 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE,
    cstrCatCovs = c("SEX","SMOK","NCIL"),
    cstrContCovs = c("WT","BMI","AGE"))
  expect_snapshot_value(stabilize(as.data.frame(head(tmp10))), style = "serialize")

  ## Check the multiple DV feature
  tmp9 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE, # Changed to TRUE
    cstrDV =c("DV","LNDV"))
  expect_snapshot_value(stabilize(as.data.frame(head(tmp9))), style = "serialize")

  ## Check the multiple DV feature with many covariates
  tmp11 <- createFREMData(
    strFFEMData = strFFEMData,
    quiet=TRUE, # Changed to TRUE
    cstrDV =c("DV","LNDV"),
    cstrCatCovs = c("SEX","SMOK","NCIL"),
    cstrContCovs = c("WT","BMI","AGE"))
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
    createFREMData(strFFEMData = strFFEMData, cstrContCovs = "WT", cFremtypes = c(0)), # Asks for 1 DV and 1 Cov, but provides only 1 fremtype
    "The number of fremtypes are not the same as the number of frem variables"
  )

  # Test file writing functionality (L252)
  tmp_file <- tempfile(fileext = ".csv")
  createFREMData(strFFEMData = strFFEMData, strFREMDataFileName = tmp_file)
  expect_true(file.exists(tmp_file))
  unlink(tmp_file) # Clean up

  # Test verbose output for missing covariates (L171 & L197)
  expect_output(
    createFREMData(strFFEMData = strFFEMData, cstrContCovs = "FAKE_CONT", quiet = FALSE),
    "not found in FFEM dataset"
  )
  expect_output(
    createFREMData(strFFEMData = strFFEMData, cstrCatCovs = "FAKE_CAT", quiet = FALSE),
    "not found in FFEM dataset"
  )

  # Test dichotomous categorical covariate WITHOUT recoding (L222-L228)
  # This triggers the 'else' block for dichotomous covariates
  dichot_no_recode <- createFREMData(
    strFFEMData = strFFEMData,
    cstrCatCovs = "SEX",
    bRecodeDichotomous = FALSE
  )
  expect_snapshot_value(stabilize(as.data.frame(head(dichot_no_recode, 15))), style = "serialize")

  # Test providing only categorical covariates (no continuous) (covers L126)
  cat_only <- createFREMData(
    strFFEMData = strFFEMData,
    cstrCatCovs = "SEX",
    cstrContCovs = NULL
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
      quiet = FALSE
    ),
    "No non-missing covariate values for WT. Skipping this covariate."
  )

  # Test ONLY the missing categorical covariate case
  expect_output(
    createFREMData(
      strFFEMData = tmp_csv_missing,
      cstrCatCovs = "SEX",
      quiet = FALSE
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
    createFREMData(strFFEMData = tmp_test_csv, cstrDV = "DV_ALL_MISSING"),
    "Note that it might be inconsistencies in DV fremtypes"
  )

  # 3. Test recoding of a polycothomous covariate
  result_poly <- createFREMData(strFFEMData = tmp_test_csv, cstrCatCovs = "RACE")

  # ROBUST TEST: Check that the DV column contains the recoded 0/1 values
  # The function should create one row per ID for each new dummy variable
  frem_100 <- result_poly[result_poly$FREMTYPE == 100, ]
  frem_200 <- result_poly[result_poly$FREMTYPE == 200, ]

  # The expected values are known based on the 3 unique IDs
  expect_equal(frem_100$DV, c(0, 1, 0))
  expect_equal(frem_200$DV, c(0, 0, 1))


  # 4. Test covariates with only missing values
  expect_output(
    createFREMData(strFFEMData = tmp_test_csv, cstrContCovs = "WT_ALL_MISSING", quiet = FALSE),
    "No non-missing covariate values for WT_ALL_MISSING"
  )
  expect_output(
    createFREMData(strFFEMData = tmp_test_csv, cstrCatCovs = "RACE_ALL_MISSING", quiet = FALSE),
    "No non-missing covariate values for RACE_ALL_MISSING"
  )

  # 5. Test the 'missing columns' logic
  result_missing_col <- createFREMData(
    strFFEMData = tmp_test_csv,
    cstrKeepCols = c("ID", "TIME", "DV", "FREMTYPE", "IMAGINARY_COLUMN")
  )
  expect_true("IMAGINARY_COLUMN" %in% names(result_missing_col))
  expect_true(all(is.na(result_missing_col$IMAGINARY_COLUMN)))


  unlink(tmp_test_csv) # Clean up the temporary file
})
