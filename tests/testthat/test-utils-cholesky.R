test_that("generate_cholesky_lines produces correct 2x2 base math", {
  # 2x2 matrix, no offset
  lines <- generate_cholesky_lines(n = 2, eta_offset = 0)
  
  # Verify L factor generation
  expect_true(any(grepl("L11 = SQRT\\(MAX\\(0\\.000001, V11\\)\\)", lines)))
  expect_true(any(grepl("L21 = V21 / L11", lines)))
  expect_true(any(grepl("L22 = SQRT\\(MAX\\(0\\.000001, V22 - L21\\*\\*2\\)\\)", lines)))
  
  # Verify MYETA correlation linkage
  expect_true(any(grepl("MYETA1 = L11 \\* ETA\\(1\\)", lines)))
  expect_true(any(grepl("MYETA2 = L21 \\* ETA\\(1\\) \\+ L22 \\* ETA\\(2\\)", lines)))
  
  # Verify no unintended offsets are injected
  expect_false(any(grepl("Offset applied", lines)))
})

test_that("generate_cholesky_lines respects eta_offset", {
  # 3x3 matrix, skipping 2 ETAs (e.g., IIV on CL, V)
  lines <- generate_cholesky_lines(n = 3, eta_offset = 2)
  
  # Check header injection for auditability 
  expect_true(any(grepl("Offset applied: starting at index 3", lines)))
  
  # Verify indexing starts at 3
  expect_true(any(grepl("L33 = SQRT\\(MAX\\(0\\.000001, V33\\)\\)", lines)))
  expect_true(any(grepl("L43 = V43 / L33", lines)))
  
  # Verify higher-order covariance subtraction logic with offsets
  expect_true(any(grepl("L44 = SQRT\\(MAX\\(0\\.000001, V44 - L43\\*\\*2\\)\\)", lines)))
  expect_true(any(grepl("L54 = \\(V54 - L53 \\* L43\\) / L44", lines)))
  
  # Verify MYETA generation with offset indices
  expect_true(any(grepl("MYETA3 = L33 \\* ETA\\(3\\)", lines)))
  expect_true(any(grepl("MYETA5 = L53 \\* ETA\\(3\\) \\+ L54 \\* ETA\\(4\\) \\+ L55 \\* ETA\\(5\\)", lines)))
})

test_that("generate_cholesky_lines handles 1x1 edge case gracefully", {
  # 1x1 matrix, no offset
  lines <- generate_cholesky_lines(n = 1, eta_offset = 0)
  
  expect_true(any(grepl("L11 = SQRT\\(MAX\\(0\\.000001, V11\\)\\)", lines)))
  expect_true(any(grepl("MYETA1 = L11 \\* ETA\\(1\\)", lines)))
  
  # Ensure no cross-term math is accidentally generated
  expect_false(any(grepl("L21", lines)))
  expect_false(any(grepl("\\+", lines[grep("MYETA", lines)]))) # No addition in the MYETA line
})