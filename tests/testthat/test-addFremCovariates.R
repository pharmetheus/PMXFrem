
test_that("the correct columns are added", {

  data <- read_csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv",package="PMXFrem"),
                   show_col_types = FALSE) %>%
    filter(BLQ!=1)

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

  # Check that the column naming is OK
  expect_snapshot(addFREMcovariates(data %>% filter(NCIL!=2),covariates=c("RACEL","NCIL")))
  expect_snapshot(addFREMcovariates(data %>% filter(NCIL!=2),covariates=c("RACEL","NCIL","RACE")))

  # Both warning and error
  expect_snapshot(error=TRUE,addFREMcovariates(data,covariates="test"))
  expect_snapshot(error=TRUE,addFREMcovariates(data,covariates="SEX"))
  expect_snapshot(error=TRUE,addFREMcovariates(data,covariates=c("ETHNIC","SEX")))

  # Only warnings
  expect_warning(addFREMcovariates(data,covariates=c("RACE","SEX")))

  newData <- addFREMcovariates(data,covariates=c("RACEL"))
  expect_equal("RACEL_3" %in% names(newData),TRUE)
  expect_equal("RACEL_2" %in% names(newData),TRUE)
  expect_equal(nrow(data),nrow(newData))
})
