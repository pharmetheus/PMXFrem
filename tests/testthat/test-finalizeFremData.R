test_that("finalizeFremData subsets and writes data while preserving original ID appearance order (happy path)", {
  
  # 1. Setup
  td <- withr::local_tempdir()
  output_path <- file.path(td, "final.csv")
  
  # Unsorted, extra columns, but includes FREMTYPE
  sample_df <- data.frame(
    ID = c(2, 1),
    TIME = c(0, 0),
    DV = c(20, 10),
    FREMTYPE = c(0, 0),
    EXTRA_COL = c("b", "a")
  )
  
  # 2. Action
  # No warning should be generated here
  result_df <- finalizeFremData(
    dfFREM = sample_df,
    strID = "ID", # Updated from sortFREMDataset
    cstrKeepCols = c("ID", "DV", "FREMTYPE"), # Keep FREMTYPE
    bWriteData = TRUE,
    strNewFREMData = output_path
  )
  
  # 3. Assertions
  # Check the returned data frame
  expect_equal(names(result_df), c("ID", "DV", "FREMTYPE"))
  
  # The new stable sort preserves the exact original appearance order of IDs
  expect_equal(result_df$ID, c(2, 1)) 
  
  # Check the side effect (file writing)
  expect_true(file.exists(output_path))
  written_df <- read.csv(output_path)
  expect_equal(names(written_df), c("ID", "DV", "FREMTYPE"))
})

test_that("finalizeFremData warns if FREMTYPE is not kept", {
  
  # 1. Setup
  td <- withr::local_tempdir()
  output_path <- file.path(td, "final_warning.csv")
  
  # This sample data does not need FREMTYPE, as it will be removed by cstrKeepCols
  sample_df <- data.frame(ID = 1, DV = 10, FREMTYPE = 0, EXTRA_COL = "a")
  
  # 2. Action & Assertion
  # We expect the function to issue the specific warning
  expect_warning(
    finalizeFremData(
      dfFREM = sample_df,
      strID = "ID", # Updated from sortFREMDataset
      cstrKeepCols = c("ID", "DV"), # Explicitly exclude FREMTYPE
      bWriteData = TRUE,
      strNewFREMData = output_path
    ),
    regexp = "No FREMTYPE available in dataset"
  )
})

test_that("finalizeFremData preserves intra-subject sequence and places covariates at row 2", {
  
  # 1. Setup
  sample_df <- data.frame(
    ID = c(1, 1, 1, 1, 1, 1),
    TIME = c(0, 12, 0, 0, 12, 24),
    EVID = c(1, 0, 0, 4, 0, 0),
    FREMTYPE = c(0, 0, 100, 0, 0, 0) # 100 is the pseudo-observation
  )
  
  # 2. Action
  result_df <- finalizeFremData(
    dfFREM = sample_df,
    strID = "ID",
    cstrKeepCols = names(sample_df),
    bWriteData = FALSE,
    strNewFREMData = NULL
  )
  
  # 3. Assertions
  # Base records should retain their exact original relative order, 
  # but the pseudo-record (FREMTYPE=100) must immediately follow the first record.
  expect_equal(result_df$FREMTYPE, c(0, 100, 0, 0, 0, 0))
  
  # The EVID sequence should be untouched (ignoring the pseudo insertion)
  # Corrected to 6 elements!
  expect_equal(result_df$EVID, c(1, 0, 0, 4, 0, 0))
  
  # The TIME sequence proves it ignored the EVID=4 reset
  # Corrected to 6 elements!
  expect_equal(result_df$TIME, c(0, 0, 12, 0, 12, 24))
})

test_that("finalizeFremData correctly groups multiple subjects and their pseudo-observations", {
  
  # 1. Setup
  # Mixed subjects and pseudo-observations
  sample_df <- data.frame(
    ID = c(2, 1, 2, 1),
    TIME = c(0, 0, 0, 0),
    FREMTYPE = c(0, 0, 100, 100),
    VAL = c("Base2", "Base1", "Pseudo2", "Pseudo1")
  )
  
  # 2. Action
  result_df <- finalizeFremData(
    dfFREM = sample_df,
    strID = "ID",
    cstrKeepCols = names(sample_df),
    bWriteData = FALSE,
    strNewFREMData = NULL
  )
  
  # 3. Assertions
  # Subject 2 appeared first, so it should be processed first.
  # Order should be: Base2, Pseudo2, Base1, Pseudo1
  expect_equal(result_df$ID, c(2, 2, 1, 1))
  expect_equal(result_df$FREMTYPE, c(0, 100, 0, 100))
  expect_equal(result_df$VAL, c("Base2", "Pseudo2", "Base1", "Pseudo1"))
})