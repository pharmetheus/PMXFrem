### FILE: tests/testthat/test-prepareNewCovariates.R

test_that("prepareNewCovariates handles continuous and polychotomous categorical covariates", {
  
  # 1. Setup
  sample_ffem <- data.frame(
    ID = 1:6,
    AGE = c(25, 30, 35, 40, 45, 50),
    SITE = c(101, 102, 103, 101, 102, 103) # 3-level categorical
  )
  
  # 2. Action
  result <- prepareNewCovariates(
    dfFFEM = sample_ffem,
    cstrCatCovsToAdd = "SITE",
    cstrContCovsToAdd = "AGE",
    cstrCovsToAddOrder = NULL,
    existingCovNames = list(covNames = c(), orgCovNames = c()), # No existing covs
    lastFremType = 0, # Starting from scratch
    iFremTypeIncrease = 100,
    strID = "ID",
    overrideExistingCheck = FALSE,
    quiet = TRUE,
    bRecodeDichotomous = FALSE # Explicitly test with the new safe default
  )
  
  # 3. Assertions
  # Check the list of added covariate names (alphabetical order)
  expect_equal(result$addedList, c("AGE", "SITE_102", "SITE_103"))
  
  # Check that the main data frame was modified correctly
  expect_true(all(c("SITE_102", "SITE_103") %in% names(result$dfFFEM)))
  expect_equal(result$dfFFEM$SITE_102, c(0, 1, 0, 0, 1, 0))
  
  # Check the covList structure for one of the new covariates
  expect_equal(names(result$covList$AGE), c("Name", "Mean", "Var", "Fremtype", "Fix","Data"))
  expect_equal(result$covList$AGE$Mean, 37.5)
  expect_equal(result$covList$AGE$Fremtype, 100)
  
  # Check that the last FREMTYPE was updated
  expect_equal(result$lastFremType, 300)
  
  # Use a snapshot for a comprehensive check of the returned list
  expect_snapshot_value(result, style = "serialize")
})

test_that("prepareNewCovariates fails fast on 1/2 covariates when bRecodeDichotomous is FALSE", {
  # Setup: SEX is coded as 1/2, violating strict 0/1 baseline assumptions
  sample_ffem <- data.frame(ID = 1:4, SEX = c(1, 2, 1, 2))
  
  # Default is now FALSE, so this must throw our strict validation error
  expect_error(
    prepareNewCovariates(
      dfFFEM = sample_ffem,
      cstrCatCovsToAdd = "SEX", 
      cstrContCovsToAdd = NULL,
      cstrCovsToAddOrder = NULL,
      existingCovNames = list(covNames = c(), orgCovNames = c()),
      lastFremType = 0,
      iFremTypeIncrease = 100,
      strID = "ID",
      overrideExistingCheck = FALSE,
      quiet = TRUE,
      bRecodeDichotomous = FALSE
    ),
    regexp = "Strict Validation Error: Dichotomous covariate 'SEX' is coded as 1/2"
  )
})

test_that("prepareNewCovariates recodes 1/2 to 0/1 when bRecodeDichotomous is TRUE", {
  # Setup: SEX is coded as 1/2
  sample_ffem <- data.frame(ID = 1:4, SEX = c(1, 2, 1, 2))
  
  # Action: Override safety check by forcing TRUE
  result <- prepareNewCovariates(
    dfFFEM = sample_ffem,
    cstrCatCovsToAdd = "SEX", 
    cstrContCovsToAdd = NULL,
    cstrCovsToAddOrder = NULL,
    existingCovNames = list(covNames = c(), orgCovNames = c()),
    lastFremType = 0,
    iFremTypeIncrease = 100,
    strID = "ID",
    overrideExistingCheck = FALSE,
    quiet = TRUE,
    bRecodeDichotomous = TRUE
  )
  
  # The covariate name should have been appended with the variant level
  expect_equal(result$addedList, "SEX_2")
  
  # The data should now be strictly 0/1
  expect_equal(result$dfFFEM$SEX_2, c(0, 1, 0, 1))
  
  # The mean should be calculated on the 0/1 scale (mean of c(0,1,0,1) is 0.5)
  expect_equal(result$covList$SEX_2$Mean, 0.5)
})

test_that("prepareNewCovariates accepts native 0/1 covariates without recoding when bRecodeDichotomous is FALSE", {
  # Setup: Valid 0/1 coding
  sample_ffem <- data.frame(ID = 1:4, FLAG = c(0, 1, 0, 1))
  
  result <- prepareNewCovariates(
    dfFFEM = sample_ffem,
    cstrCatCovsToAdd = "FLAG", 
    cstrContCovsToAdd = NULL,
    cstrCovsToAddOrder = NULL,
    existingCovNames = list(covNames = c(), orgCovNames = c()),
    lastFremType = 0,
    iFremTypeIncrease = 100,
    strID = "ID",
    overrideExistingCheck = FALSE,
    quiet = TRUE,
    bRecodeDichotomous = FALSE
  )
  
  # It should retain the raw name and scale
  expect_equal(result$addedList, "FLAG")
  expect_equal(result$covList$FLAG$Mean, 0.5)
  expect_equal(result$dfFFEM$FLAG, c(0, 1, 0, 1))
})

test_that("prepareNewCovariates outputs correct console message for dichotomous covariates", {
  # Setup: Valid 0/1 coding
  sample_ffem <- data.frame(ID = 1:4, FLAG = c(0, 1, 0, 1))
  
  # Action & Assertion: Capture the output and match the exact string
  expect_output(
    prepareNewCovariates(
      dfFFEM = sample_ffem,
      cstrCatCovsToAdd = "FLAG", 
      cstrContCovsToAdd = NULL,
      cstrCovsToAddOrder = NULL,
      existingCovNames = list(covNames = c(), orgCovNames = c()),
      lastFremType = 0,
      iFremTypeIncrease = 100,
      strID = "ID",
      overrideExistingCheck = FALSE,
      quiet = FALSE, # Must be FALSE to capture the printq output
      bRecodeDichotomous = FALSE,
      allowNon01 = FALSE
    ),
    regexp = "Categorical covariate FLAG is dichotomous, treating as continuous without recoding"
  )
})

test_that("allowNon01 = TRUE permits 1/2 covariates to pass through untouched in Phase 2", {
  # Setup: SEX is coded as 1/2
  sample_ffem <- data.frame(ID = 1:4, SEX = c(1, 2, 1, 2))
  
  # Action: Override the kill-switch without triggering recoding
  result <- prepareNewCovariates(
    dfFFEM = sample_ffem,
    cstrCatCovsToAdd = "SEX", 
    cstrContCovsToAdd = NULL,
    cstrCovsToAddOrder = NULL,
    existingCovNames = list(covNames = c(), orgCovNames = c()),
    lastFremType = 0,
    iFremTypeIncrease = 100,
    strID = "ID",
    overrideExistingCheck = FALSE,
    quiet = TRUE,
    bRecodeDichotomous = FALSE,
    allowNon01 = TRUE
  )
  
  # Assertions:
  # 1. The covariate name should remain untouched (not "SEX_2")
  expect_equal(result$addedList, "SEX")
  
  # 2. The wide dataset should retain the raw 1/2 scale
  expect_equal(result$dfFFEM$SEX, c(1, 2, 1, 2))
  
  # 3. The calculated mathematical prior (Mean) should be based on the 1/2 scale (mean is 1.5)
  expect_equal(result$covList$SEX$Mean, 1.5)
})