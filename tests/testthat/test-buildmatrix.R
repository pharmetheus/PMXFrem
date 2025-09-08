test_that("buildmatrix: forceSingleBlock argument works correctly", {
  
  # Create a block-diagonal matrix, similar to what createMinimalFremModel produces
  block_diag_matrix <- matrix(
    c(0.2, 0.01, 0.00,
      0.01, 0.3, 0.00,
      0.00, 0.00, 10.0),
    nrow = 3, 
    byrow = TRUE
  )
  
  # 1. Test the default behavior (forceSingleBlock = FALSE)
  # It should correctly identify two separate blocks.
  result_default <- buildmatrix(block_diag_matrix, strName = "$OMEGA")
  
  expected_default <- c(
    "$OMEGA BLOCK(2) ",
    "0.2 ",
    "0.01 0.3 ",
    "$OMEGA BLOCK(1) ",
    "10"
  )
  
  expect_equal(result_default, expected_default)
  
  # 2. Test the new behavior (forceSingleBlock = TRUE)
  # It should ignore the block structure and treat it as one dense matrix.
  result_forced <- buildmatrix(block_diag_matrix, strName = "$OMEGA", forceSingleBlock = TRUE)
  
  expected_forced <- c(
    "$OMEGA BLOCK(3) ",
    "0.2 ",
    "0.01 0.3 ",
    "0 0 10 "
  )
  
  expect_equal(result_forced, expected_forced)
})