### FILE: tests/testthat/test-calcFremShrinkage.R ###

library(testthat)
library(PMXFrem)

modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
modName   <- "run31max1-2"

test_that("calcFremShrinkage computes valid shrinkages and returns a strictly formatted data.frame", {
  
  # Run the native shrinkage function
  shrinkages <- calcFremShrinkage(
    modName = modName, 
    modDevDir = modDevDir, 
    quiet = TRUE
  )
  
  # Check object type
  expect_s3_class(shrinkages, "data.frame")
  expect_true(nrow(shrinkages) > 0)
  
  # Check expected columns
  expected_cols <- c("Parameter", "ETA_Var", "ETA_SD", "EBV_Var", "EBV_SD")
  expect_true(all(expected_cols %in% names(shrinkages)))
  
  # Check column data types
  expect_type(shrinkages$Parameter, "character")
  expect_type(shrinkages$ETA_Var, "double")
  expect_type(shrinkages$ETA_SD, "double")
  expect_type(shrinkages$EBV_Var, "double")
  expect_type(shrinkages$EBV_SD, "double")
  
  # Verify specific structural ETAs were parsed
  expect_true("ETA1" %in% shrinkages$Parameter)
})

test_that("calcFremShrinkage safely aborts if required files are missing", {
  
  # Test with a non-existent model name to trigger missing .ext file error
  expect_error(
    calcFremShrinkage(
      modName = "missing_phantom_model", 
      modDevDir = modDevDir, 
      quiet = TRUE
    ),
    "Cannot find .ext file at"
  )
})