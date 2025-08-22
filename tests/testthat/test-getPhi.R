test_that("getPhi works with real data files", {
  # Keep original tests
  phiData <- getPhi(system.file("extdata","SimNeb/run30.phi",package="PMXFrem"), warn = FALSE)
  expect_s3_class(phiData, "data.frame")

  expect_warning(getPhi(system.file("extdata","SimNeb/CopyOfrun30.phi",package="PMXFrem")))
})


test_that("getPhi handles all logic branches", {
  # Setup: Create temporary .phi files to test all logic branches
  temp_dir <- file.path(tempdir(), "test-getPhi-comprehensive")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

  create_file <- function(path, content) {
    writeLines(text = content, con = path)
  }

  # --- Test Fixtures ---
  content1 <- c("TABLE NO. 1", "ID ETA1", "1 1.1")
  file_1_table <- file.path(temp_dir, "one_table.phi")
  create_file(file_1_table, content1)

  content2 <- c("TABLE NO. 1", "ID ETA1", "1 1.1", "TABLE NO. 2", "ID ETA1", "1 2.2")
  file_2_tables <- file.path(temp_dir, "two_tables.phi")
  create_file(file_2_tables, content2)

  content3 <- c("TABLE NO. 1", "ID ETA1", "1 1.1", "TABLE NO. 2", "ID ETA1", "1 2.2", "TABLE NO. 3", "ID ETA1", "1 3.3")
  file_3_tables <- file.path(temp_dir, "three_tables.phi")
  create_file(file_3_tables, content3)

  content4 <- c("TABLE NO. 1", "A B", "1 1", "TABLE NO. 2", "C D", "2 2",
                "TABLE NO. 3", "E F", "3 3", "TABLE NO. 4", "ID ETA1", "1 4.4")
  file_4_tables <- file.path(temp_dir, "four_tables.phi")
  create_file(file_4_tables, content4)

  content_phi_names <- c("TABLE NO. 1", "ID PHI(1) ETA(2)", "1 0.1 0.2")
  file_phi_names <- file.path(temp_dir, "phi_names.phi")
  create_file(file_phi_names, content_phi_names)

  content_no_table <- c("HEADER LINE", "ID ETA1", "1 1.1")
  file_no_table <- file.path(temp_dir, "no_table.phi")
  create_file(file_no_table, content_no_table)

  # --- Test Cases ---
  expect_snapshot(getPhi(file_1_table))
  expect_snapshot(getPhi(file_2_tables, set = 1))
  expect_snapshot(getPhi(file_3_tables, set = 2))
  expect_snapshot(getPhi(file_4_tables, set = 4))
  expect_snapshot(getPhi(file_4_tables)) # Default to last

  # Test warning logic
  expect_warning(getPhi(file_phi_names), regexp = "generated without PHITYPE=1")
  expect_silent(getPhi(file_phi_names, warn = FALSE))

  # Test error: invalid set number
  expect_error(getPhi(file_1_table, set = 2))

  # Test error: file contains no "TABLE" headers
  expect_error(getPhi(file_no_table), regexp = "object 'myphi' not found")
})
