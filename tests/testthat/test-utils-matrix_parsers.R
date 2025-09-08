test_that("parseMatrixBlockToMatrix handles simple diagonal matrices", {
  # Input with multiple simple diagonal records, comments, and FIX
  block <- c(
    "$OMEGA 0.1 ; IIV CL",
    "$OMEGA 0.2 FIX ; IIV V"
  )
  
  mat <- parseMatrixBlockToMatrix(block)
  
  expected_mat <- diag(c(0.1, 0.2))
  
  expect_true(is.matrix(mat))
  expect_equal(dim(mat), c(2, 2))
  expect_equal(mat, expected_mat)
})

test_that("parseMatrixBlockToMatrix handles single-line BLOCK matrices", {
  # Input for a 2x2 block matrix on a single line
  block <- c("$OMEGA BLOCK(2) 0.1 0.01 0.2")
  
  mat <- parseMatrixBlockToMatrix(block)
  
  expected_mat <- matrix(c(0.1, 0.01, 0.01, 0.2), nrow = 2, byrow = TRUE)
  
  expect_true(is.matrix(mat))
  expect_equal(dim(mat), c(2, 2))
  expect_equal(mat, expected_mat)
  expect_true(isSymmetric(mat))
})

test_that("parseMatrixBlockToMatrix handles multi-line BLOCK matrices", {
  # Input from run30.mod's OMEGA block
  block <- c(
    "$OMEGA BLOCK(3)",
    " 0.488192  ; 3. IIV on CL",
    " 0.270746 0.655494  ; 4. IIV on V",
    " -0.025678 0.0206116 0.157579  ; 5. IIV on MAT"
  )
  
  mat <- parseMatrixBlockToMatrix(block)
  
  # Manually construct the expected symmetric matrix
  expected_mat <- matrix(0, 3, 3)
  expected_mat[lower.tri(expected_mat, diag = TRUE)] <- c(0.488192, 0.270746, -0.025678, 0.655494, 0.0206116, 0.157579)
  expected_mat[upper.tri(expected_mat)] <- t(expected_mat)[upper.tri(expected_mat)]
  
  expect_true(is.matrix(mat))
  expect_equal(dim(mat), c(3, 3))
  expect_equal(mat, expected_mat)
  expect_true(isSymmetric(mat))
})

test_that("parseMatrixBlockToMatrix errors on incorrect number of BLOCK values", {
  # A BLOCK(2) needs 3 values, but only 2 are provided.
  block_too_few <- c("$OMEGA BLOCK(2) 0.1 0.2")
  expect_error(
    parseMatrixBlockToMatrix(block_too_few),
    regexp = "Number of values in OMEGA BLOCK does not match"
  )
  
  # A BLOCK(2) needs 3 values, but 4 are provided.
  block_too_many <- c("$OMEGA BLOCK(2) 0.1 0.01 0.2 0.3")
  expect_error(
    parseMatrixBlockToMatrix(block_too_many),
    regexp = "Number of values in OMEGA BLOCK does not match"
  )
})