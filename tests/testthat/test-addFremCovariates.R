test_that("the correct columns are added", {

  data <- readr::read_csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv",package="PMXFrem"),
                          show_col_types = FALSE) %>%
    dplyr::filter(BLQ!=1)

  expect_error(addFREMcovariates())
  expect_error(addFREMcovariates("test"))
  expect_error(addFREMcovariates(data))


  ## Test the case when modFile is not NULL
  newData <-addFREMcovariates(data,modFile = system.file("extdata/SimNeb/run31.mod",package="PMXFrem"))

  ## Check object type
  expect_equal("data.frame" %in% class(newData),TRUE)

  ## Check the existence of columns
  expect_equal("RACEL_3" %in% names(newData),TRUE)
  expect_equal("RACEL_2" %in% names(newData),TRUE)
  expect_equal("NCIL_2"   %in% names(newData),TRUE)
  expect_equal("NCIL_1"   %in% names(newData),TRUE)

  ## Check the non-existence of columns
  expect_equal("GENO2_1"  %in% names(newData),FALSE)
  expect_equal("SEX_1"   %in% names(newData),FALSE)

  ## Check content
  expect_equal(length(unique(newData$RACEL_3)), 2) # 0, 1, and -99
  expect_equal(max(newData$RACEL_3), 1)
  expect_equal(min(newData$RACEL_3), 0)
  
  ## Check the case when covariates is not NULL

  # Test case 1: This call produces a warning and a data frame.
  warnings1 <- testthat::capture_warnings({
    result1 <- addFREMcovariates(data %>% dplyr::filter(NCIL != 2), covariates = c("RACEL", "NCIL"))
  })
  expect_match(warnings1[[1]], "NCIL has only two non-missing levels", fixed = TRUE)
  expect_snapshot_value(stabilize(as.data.frame(result1)), style = "serialize")

  # Test case 2: This call also produces a warning and a data frame.
  warnings2 <- testthat::capture_warnings({
    result2 <- addFREMcovariates(data %>% dplyr::filter(NCIL != 2), covariates = c("RACEL", "NCIL", "RACE"))
  })
  expect_match(warnings2[[1]], "NCIL has only two non-missing levels", fixed = TRUE)
  expect_snapshot_value(stabilize(as.data.frame(result2)), style = "serialize")


  # Test cases that throw both a warning and then an error
  expect_warning(
    expect_error(
      addFREMcovariates(data, covariates = "test"),
      regexp = "No binarised covariates to add"
    ),
    regexp = "test does not exist in the data set"
  )

  expect_warning(
    expect_error(
      addFREMcovariates(data, covariates = "SEX"),
      regexp = "No binarised covariates to add"
    ),
    regexp = "SEX has only two non-missing levels"
  )

  # This case throws MULTIPLE warnings, then errors.
  warnings3 <- testthat::capture_warnings(
    expect_error(
      addFREMcovariates(data, covariates = c("ETHNIC", "SEX")),
      regexp = "No binarised covariates to add"
    )
  )
  expect_snapshot_value(warnings3, style = "serialize")


  # Only warnings
  expect_warning(addFREMcovariates(data,covariates=c("RACE","SEX")))

  newData <- addFREMcovariates(data,covariates=c("RACEL"))
  expect_equal("RACEL_3" %in% names(newData),TRUE)
  expect_equal("RACEL_2" %in% names(newData),TRUE)
  expect_equal(nrow(data),nrow(newData))
  
  ## ------------------------------------------------------------------------
  ## Check imputeMissing toggle for FREM vs FFEM data
  ## ------------------------------------------------------------------------
  
  # Inject missing values into the first two rows of RACEL
  data_missing <- data
  data_missing$RACEL[1:2] <- -99
  
  # Test imputeMissing = FALSE (FREM behavior: preserves -99)
  newData_frem <- addFREMcovariates(data_missing, covariates = c("RACEL"), 
                                    iMiss = -99, imputeMissing = FALSE)
  
  expect_equal(newData_frem$RACEL_3[1:2], c(-99, -99))
  expect_equal(newData_frem$RACEL_2[1:2], c(-99, -99))
  
  # Test imputeMissing = TRUE (FFEM behavior: imputes to 0, which is the default)
  newData_ffem <- addFREMcovariates(data_missing, covariates = c("RACEL"), 
                                    iMiss = -99, imputeMissing = TRUE)
  
  expect_equal(newData_ffem$RACEL_3[1:2], c(0, 0))
  expect_equal(newData_ffem$RACEL_2[1:2], c(0, 0))
})

