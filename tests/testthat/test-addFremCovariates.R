test_that("the correct columns are added", {
  data <- read_csv(system.file("extdata/SimVal/DAT-1-MI-PMX-2.csv",package="PMXFrem"),show_col_types = FALSE) %>% 
    filter(BLQ!=1) %>% 
    filter(TYPE!=1) %>% 
    filter(TSLD<100)
  
  newData <-addFremCovariates(data,modFile = system.file("extdata/SimVal/run9.mod",package="PMXFrem"))
  
  ## Check object type
  expect_equal("data.frame" %in% class(newData),TRUE)
  
  ## Check the existence of columns
  expect_equal("RACEL_3" %in% names(newData),TRUE)
  expect_equal("RACEL_2" %in% names(newData),TRUE)
  expect_equal("NCI_2"   %in% names(newData),TRUE)
  expect_equal("NCI_1"   %in% names(newData),TRUE)
  expect_equal("GENO_4"  %in% names(newData),TRUE)
  expect_equal("GENO_3"  %in% names(newData),TRUE)
  expect_equal("GENO_2"  %in% names(newData),TRUE)
  
  ## Check the non-existence of columns
  expect_equal("GENO_1"  %in% names(newData),FALSE)
  expect_equal("SEX_1"   %in% names(newData),FALSE)
  
  #Cehck content
  expect_equal(length(unique(newData$RACEL_3)),2)
  expect_equal(max(newData$RACEL_3),1)
  expect_equal(min(newData$RACEL_3),0)
  
})
