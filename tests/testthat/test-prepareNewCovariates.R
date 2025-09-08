test_that("prepareNewCovariates handles continuous and categorical covariates", {
  
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
    quiet = TRUE
  )
  
  # 3. Assertions
  # Check the list of added covariate names (alphabetical order)
  expect_equal(result$addedList, c("AGE", "SITE_102", "SITE_103"))
  
  # Check that the main data frame was modified correctly
  expect_true(all(c("SITE_102", "SITE_103") %in% names(result$dfFFEM)))
  expect_equal(result$dfFFEM$SITE_102, c(0, 1, 0, 0, 1, 0))
  
  # Check the covList structure for one of the new covariates
  expect_equal(names(result$covList$AGE), c("Name", "Mean", "Var", "Fremtype", "Data"))
  expect_equal(result$covList$AGE$Mean, 37.5)
  expect_equal(result$covList$AGE$Fremtype, 100)
  
  # Check that the last FREMTYPE was updated
  expect_equal(result$lastFremType, 300)
  
  # Use a snapshot for a comprehensive check of the returned list
  expect_snapshot_value(result, style = "serialize")
})

test_that("prepareNewCovariates re-classifies binary categoricals", {
  
  sample_ffem <- data.frame(ID = 1:4, SEX = c(0, 1, 0, 1))
  
  result <- prepareNewCovariates(
    dfFFEM = sample_ffem,
    cstrCatCovsToAdd = "SEX", # Initially passed as categorical
    cstrContCovsToAdd = NULL,
    cstrCovsToAddOrder = NULL,
    existingCovNames = list(covNames = c(), orgCovNames = c()),
    lastFremType = 0,
    iFremTypeIncrease = 100,
    strID = "ID",
    overrideExistingCheck = FALSE,
    quiet = TRUE
  )
  
  # It should be treated as a single continuous (0/1) variable, not binarized
  expect_equal(result$addedList, "SEX")
  expect_length(result$covList, 1)
  expect_equal(result$lastFremType, 100)
})