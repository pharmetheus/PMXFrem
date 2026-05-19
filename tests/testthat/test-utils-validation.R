test_that("validateFremData catches missing rows, corrupted data, and scrambled IDs", {
  
  orig_data <- data.frame(
    ID = c(1, 1, 2),
    TIME = c(0, 12, 0),
    DV = c(10, 5, 20)
  )
  
  # 1. Happy Path
  good_frem <- data.frame(
    ID = c(1, 1, 2, 1, 2),
    TIME = c(0, 12, 0, 0, 0),
    DV = c(10, 5, 20, 1, 1),
    FREMTYPE = c(0, 0, 0, 100, 100)
  )
  expect_true(validateFremData(orig_data, good_frem, quiet = TRUE))
  
  # 2. Dropped Row (e.g., if a dose was filtered out)
  dropped_row_frem <- good_frem[-2, ] # Remove row 2
  expect_error(
    validateFremData(orig_data, dropped_row_frem, quiet = TRUE),
    "CRITICAL DATA ERROR: Row count mismatch"
  )
  
  # 3. Data Mutation (e.g., if TIME or DV got changed)
  mutated_frem <- good_frem
  mutated_frem$DV[1] <- 999 
  expect_error(
    validateFremData(orig_data, mutated_frem, quiet = TRUE),
    "CRITICAL DATA ERROR: Data mutation detected in column 'DV'"
  )
  
  # 4. Scrambled IDs (e.g., if merge() scrambled the cohort order)
  scrambled_frem <- good_frem
  scrambled_frem$ID[1:3] <- c(2, 1, 1) # Flip the cohort order in the base data
  expect_error(
    validateFremData(orig_data, scrambled_frem, quiet = TRUE),
    "CRITICAL DATA ERROR: Subject cohort mismatch"
  )
})