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
  expect_equal(length(unique(newData$RACEL_3)),2)
  expect_equal(max(newData$RACEL_3),1)
  expect_equal(min(newData$RACEL_3),0)


  ## Check the case when covariates is not NULL

  # Test case 1: This call produces a warning and a data frame.
  warnings1 <- testthat::capture_warnings({
    result1 <- addFREMcovariates(data %>% dplyr::filter(NCIL != 2), covariates = c("RACEL", "NCIL"))
  })
  expect_match(warnings1[[1]], "NCIL has only two non-missing levels", fixed = TRUE)
  expect_snapshot(stabilize(as.data.frame(result1)))

  # Test case 2: This call also produces a warning and a data frame.
  warnings2 <- testthat::capture_warnings({
    result2 <- addFREMcovariates(data %>% dplyr::filter(NCIL != 2), covariates = c("RACEL", "NCIL", "RACE"))
  })
  expect_match(warnings2[[1]], "NCIL has only two non-missing levels", fixed = TRUE)
  expect_snapshot(stabilize(as.data.frame(result2)))


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
  expect_snapshot(warnings3)


  # Only warnings
  expect_warning(addFREMcovariates(data,covariates=c("RACE","SEX")))

  newData <- addFREMcovariates(data,covariates=c("RACEL"))
  expect_equal("RACEL_3" %in% names(newData),TRUE)
  expect_equal("RACEL_2" %in% names(newData),TRUE)
  expect_equal(nrow(data),nrow(newData))
})
