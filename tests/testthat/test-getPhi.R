test_that("getPhi works with real data files", {
  # Keep original tests
  phiData <- getPhi(system.file("extdata","SimNeb/run30.phi",package="PMXFrem"), warn = FALSE)
  expect_s3_class(phiData, "data.frame")

  expect_warning(getPhi(system.file("extdata","SimNeb/CopyOfrun30.phi",package="PMXFrem")))
})


test_that("getPhi handles all logic branches", {
  # Use a temporary directory that is automatically cleaned up
  temp_dir <- withr::local_tempdir()

  create_file <- function(path, content) {
    full_path <- file.path(temp_dir, path)
    writeLines(text = content, con = full_path)
    return(full_path)
  }

  # --- Test Fixtures ---
  file_1_table <- create_file("one_table.phi", c("TABLE NO. 1", "ID ETA1", "1 1.1"))
  file_2_tables <- create_file("two_tables.phi", c("TABLE NO. 1", "ID ETA1", "1 1.1", "TABLE NO. 2", "ID ETA1", "1 2.2"))
  file_3_tables <- create_file("three_tables.phi", c("TABLE NO. 1", "ID ETA1", "1 1.1", "TABLE NO. 2", "ID ETA1", "1 2.2", "TABLE NO. 3", "ID ETA1", "1 3.3"))
  file_4_tables <- create_file("four_tables.phi", c("TABLE NO. 1", "A B", "1 1", "TABLE NO. 2", "C D", "2 2", "TABLE NO. 3", "E F", "3 3", "TABLE NO. 4", "ID ETA1", "1 4.4"))
  file_phi_names <- create_file("phi_names.phi", c("TABLE NO. 1", "ID PHI(1) ETA(2)", "1 0.1 0.2"))
  file_no_table <- create_file("no_table.phi", c("HEADER LINE", "ID ETA1", "1 1.1"))

  # --- Test Cases ---
  expect_s3_class(getPhi(file_1_table), "data.frame")
  expect_s3_class(getPhi(file_2_tables, set = 2), "data.frame")

  # Tests for red lines
  expect_equal(getPhi(file_2_tables, set = 1)$ETA1, 1.1)
  expect_equal(getPhi(file_3_tables, set = 2)$ETA1, 2.2)
  expect_equal(getPhi(file_3_tables, set = 3)$ETA1, 3.3)
  expect_equal(getPhi(file_4_tables, set = 4)$ETA1, 4.4)

  # Test warning logic
  expect_warning(getPhi(file_phi_names), regexp = "generated without PHITYPE=1")
  expect_silent(getPhi(file_phi_names, warn = FALSE))

  # Test error: invalid set number
  expect_error(getPhi(file_1_table, set = 2))

  # Test error: file contains no "TABLE" headers
  expect_error(getPhi(file_no_table))
})
