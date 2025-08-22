test_that("getExt works with real data file", {
  # Keep the original test to ensure no regressions
  extData <- getExt(system.file("extdata","SimNeb/run30.ext",package="PMXFrem"))
  expect_s3_class(extData, "data.frame")
})

test_that("getExt handles all logic branches", {
  # Setup: Create temporary .ext files to test all logic branches
  temp_dir <- file.path(tempdir(), "test-getExt-comprehensive")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

  create_file <- function(path, content) {
    writeLines(text = content, con = path)
  }

  # --- Test Fixtures ---
  content1 <- c("TABLE NO. 1", "ITERATION TVAL", "-1000000000 1.1")
  file_1_table <- file.path(temp_dir, "one_table.ext")
  create_file(file_1_table, content1)

  content2 <- c("TABLE NO. 1", "ITERATION TVAL", "-1000000000 1.1",
                "TABLE NO. 2", "ITERATION TVAL", "-1000000000 2.2")
  file_2_tables <- file.path(temp_dir, "two_tables.ext")
  create_file(file_2_tables, content2)

  content3 <- c("TABLE NO. 1", "ITERATION TVAL", "-1000000000 1.1",
                "TABLE NO. 2", "ITERATION TVAL", "-1000000000 2.2",
                "TABLE NO. 3", "ITERATION TVAL", "-1000000000 3.3")
  file_3_tables <- file.path(temp_dir, "three_tables.ext")
  create_file(file_3_tables, content3)

  content4 <- c("TABLE NO. 1", "A B", "1 1", "TABLE NO. 2", "C D", "2 2",
                "TABLE NO. 3", "E F", "3 3", "TABLE NO. 4", "ITERATION TVAL", "-1000000000 4.4")
  file_4_tables <- file.path(temp_dir, "four_tables.ext")
  create_file(file_4_tables, content4)

  content_no_table <- c("HEADER LINE", "ITERATION TVAL", "-1000000000 1.1")
  file_no_table <- file.path(temp_dir, "no_table.ext")
  create_file(file_no_table, content_no_table)

  # --- Test Cases ---
  expect_snapshot(getExt(file_1_table))
  expect_snapshot(getExt(file_2_tables, set = 1))
  expect_snapshot(getExt(file_2_tables, set = 2))
  expect_snapshot(getExt(file_3_tables, set = 2))
  expect_snapshot(getExt(file_3_tables, set = 3))
  expect_snapshot(getExt(file_4_tables, set = 4))
  expect_snapshot(getExt(file_4_tables)) # Default to last

  # Test error: invalid set number
  expect_error(getExt(file_1_table, set = 2))

  # Test error: file contains no "TABLE" headers
  expect_error(getExt(file_no_table), regexp = "object 'myext' not found")
})
