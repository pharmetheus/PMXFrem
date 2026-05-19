# tests/testthat/test-createMockExt.R

test_that("createMockExt expands correctly for a single covariate", {
  td <- withr::local_tempdir()
  base_ext_path <- file.path(td, "base.ext")
  mock_ext_path <- file.path(td, "mock.ext")
  
  # 1. SETUP: Create a dummy NONMEM .ext file
  # 2 THETAs, 1 SIGMA, 1 OMEGA
  ext_lines <- c(
    "TABLE NO.     1",
    " ITERATION  THETA1  THETA2  SIGMA(1,1)  OMEGA(1,1)  OBJ",
    " -1000000000  1.00000E+01  2.00000E+01  1.00000E-01  2.00000E-01  1.00000E+02",
    " -1000000006  0.00000E+00  1.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00"
  )
  writeLines(ext_lines, base_ext_path)
  
  initialCovariateInfo <- list(
    list(name = "WT", mean = 75, variance = 10, shouldFixTheta = TRUE, fremType = 100)
  )
  
  # 2. ACTION
  result_path <- createMockExt(
    baseExtFile = base_ext_path,
    mockExtFile = mock_ext_path,
    initialCovariateInfo = initialCovariateInfo,
    dummySigma = 1e-7
  )
  
  # 3. ASSERTIONS
  expect_true(file.exists(result_path))
  
  # Read the generated mock file directly
  mock_lines <- readLines(result_path)
  headers <- strsplit(trimws(mock_lines[2]), "\\s+")[[1]]
  estimates <- strsplit(trimws(mock_lines[3]), "\\s+")[[1]]
  fix_flags <- strsplit(trimws(mock_lines[4]), "\\s+")[[1]]
  
  # Check Headers: Should add THETA3, SIGMA(2,1), SIGMA(2,2), OMEGA(2,1), OMEGA(2,2)
  expect_true("THETA3" %in% headers)
  expect_true("SIGMA(2,2)" %in% headers)
  expect_true("OMEGA(2,2)" %in% headers)
  
  # Find column indices to verify values
  theta3_idx <- match("THETA3", headers)
  sigma22_idx <- match("SIGMA(2,2)", headers)
  omega22_idx <- match("OMEGA(2,2)", headers)
  
  # Verify Estimates (Note: they are formatted in scientific notation)
  expect_equal(as.numeric(estimates[theta3_idx]), 75)
  expect_equal(as.numeric(estimates[sigma22_idx]), 1e-7)
  expect_equal(as.numeric(estimates[omega22_idx]), 10)
  
  # Verify Fix Flags (shouldFixTheta = TRUE -> 1)
  expect_equal(as.numeric(fix_flags[theta3_idx]), 1)
  expect_equal(as.numeric(fix_flags[sigma22_idx]), 1) # Dummy sigmas are always fixed
  expect_equal(as.numeric(fix_flags[omega22_idx]), 0) # Omegas are always estimated
})


test_that("createMockExt scales dynamically for multiple covariates (Y-1 Categorical Expansion)", {
  td <- withr::local_tempdir()
  base_ext_path <- file.path(td, "base.ext")
  mock_ext_path <- file.path(td, "mock.ext")
  
  ext_lines <- c(
    "TABLE NO.     1",
    " ITERATION  THETA1  SIGMA(1,1)  OMEGA(1,1)  OBJ",
    " -1000000000  5.00000E+00  1.00000E-01  2.00000E-01  1.00000E+02",
    " -1000000006  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00"
  )
  writeLines(ext_lines, base_ext_path)
  
  # Two covariates representing RACEL_2 and RACEL_3
  initialCovariateInfo <- list(
    list(name = "RACEL_2", mean = 0.4, variance = 0.24, shouldFixTheta = FALSE, fremType = 100),
    list(name = "RACEL_3", mean = 0.1, variance = 0.09, shouldFixTheta = FALSE, fremType = 200)
  )
  
  createMockExt(base_ext_path, mock_ext_path, initialCovariateInfo)
  
  mock_lines <- readLines(mock_ext_path)
  headers <- strsplit(trimws(mock_lines[2]), "\\s+")[[1]]
  estimates <- strsplit(trimws(mock_lines[3]), "\\s+")[[1]]
  fix_flags <- strsplit(trimws(mock_lines[4]), "\\s+")[[1]]
  
  # Should add THETA2 and THETA3
  expect_true(all(c("THETA2", "THETA3") %in% headers))
  
  # Matrix expansion check: 
  # Base had OMEGA(1,1). Cov 1 adds OMEGA(2,1), OMEGA(2,2). Cov 2 adds OMEGA(3,1), OMEGA(3,2), OMEGA(3,3).
  expect_true("OMEGA(3,3)" %in% headers)
  
  theta3_idx <- match("THETA3", headers)
  omega33_idx <- match("OMEGA(3,3)", headers)
  
  # Values for RACEL_3
  expect_equal(as.numeric(estimates[theta3_idx]), 0.1)
  expect_equal(as.numeric(estimates[omega33_idx]), 0.09)
  expect_equal(as.numeric(fix_flags[theta3_idx]), 0) # shouldFixTheta was FALSE
})


test_that("createMockExt handles base models missing the fixed parameters row (-1000000006)", {
  td <- withr::local_tempdir()
  base_ext_path <- file.path(td, "base_no_fix.ext")
  mock_ext_path <- file.path(td, "mock_no_fix.ext")
  
  # Omit the -1000000006 row
  ext_lines <- c(
    "TABLE NO.     1",
    " ITERATION  THETA1  SIGMA(1,1)  OMEGA(1,1)  OBJ",
    " -1000000000  5.00000E+00  1.00000E-01  2.00000E-01  1.00000E+02"
  )
  writeLines(ext_lines, base_ext_path)
  
  initialCovariateInfo <- list(
    list(name = "AGE", mean = 40, variance = 20, shouldFixTheta = TRUE, fremType = 100)
  )
  
  expect_no_error({
    createMockExt(base_ext_path, mock_ext_path, initialCovariateInfo)
  })
  
  mock_lines <- readLines(mock_ext_path)
  
  # Verify the fallback successfully generated the -1000000006 row in the output
  expect_true(any(grepl("-1000000006", mock_lines)))
  
  # The base THETA1 fix flag should default to 0 (estimated) in the fallback
  headers <- strsplit(trimws(mock_lines[2]), "\\s+")[[1]]
  fix_flags <- strsplit(trimws(mock_lines[4]), "\\s+")[[1]]
  expect_equal(as.numeric(fix_flags[match("THETA1", headers)]), 0)
})