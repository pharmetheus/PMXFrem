test_that("finalizeFremData sorts, subsets, and writes data correctly (happy path)", {
  
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
    sortFREMDataset = c("ID"),
    cstrKeepCols = c("ID", "DV", "FREMTYPE"), # Keep FREMTYPE
    bWriteData = TRUE,
    strNewFREMData = output_path
  )
  
  # 3. Assertions
  # Check the returned data frame
  expect_equal(names(result_df), c("ID", "DV", "FREMTYPE"))
  expect_equal(result_df$ID, c(1, 2)) # Should be sorted
  
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
  sample_df <- data.frame(ID = 1, DV = 10, EXTRA_COL = "a")
  
  # 2. Action & Assertion
  # We expect the function to issue the specific warning
  expect_warning(
    finalizeFremData(
      dfFREM = sample_df,
      sortFREMDataset = c("ID"),
      cstrKeepCols = c("ID", "DV"), # Explicitly exclude FREMTYPE
      bWriteData = TRUE,
      strNewFREMData = output_path
    ),
    regexp = "No FREMTYPE available in dataset"
  )
})