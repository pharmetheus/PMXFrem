test_that("augmentFremData correctly adds new individuals, DVs, and covariates", {
  
  # 1. Setup
  # Initial FREM data: 1 subject, 1 DV, 1 existing covariate (WT)
  initial_dfFREM <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 0),
    DV = c(10, 70),
    FREMTYPE = c(0, 100), # DV=0, WT=100
    EVID = c(0, 0)
  )
  
  # Full dataset: 2 subjects, 2 DVs, 1 old cov (WT), 1 new cov (AGE)
  full_dfFFEM <- data.frame(
    ID = c(1, 1, 2, 2),
    TIME = c(0, 1, 0, 1),
    DV1 = c(10, 8, 20, 15), # Corresponds to FREMTYPE 0
    DV2 = c(1, 0.8, 2, 1.5),# Corresponds to new FREMTYPE 1
    WT = c(70, 70, 80, 80),
    AGE = c(30, 30, 40, 40),
    EVID = c(0, 0, 0, 0)
  )
  
  # Info for existing covariates
  existing_covnames <- list(covNames = "WT")
  
  # Prepared info for new covariate to add (AGE)
  prepared_covList <- list(
    AGE = list(
      Name = "AGE",
      Fremtype = 200,
      Data = data.frame(ID = c(1, 2), AGE = c(30, 40))
    )
  )
  prepared_addedList <- "AGE"
  
  # 2. Action
  result_df <- augmentFremData(
    dfFREM = initial_dfFREM,
    dfFFEM = full_dfFFEM,
    covList = prepared_covList,
    addedList = prepared_addedList,
    covnames = existing_covnames,
    cstrDV = c("DV1", "DV2"),
    strID = "ID",
    iFremTypeIncrease = 100,
    cstrSetToZero = "EVID",
    quiet = TRUE
  )
  
  # 3. Assertions
  # New individual (ID=2) should be present
  expect_true(2 %in% result_df$ID)
  
  # New DV (FREMTYPE=1) should be present for both subjects
  expect_equal(sum(result_df$FREMTYPE == 1), 4) # 2 obs for each of 2 subjects
  
  # Old covariate (WT, FREMTYPE=100) should be present for new subject (ID=2)
  expect_true(any(result_df$ID == 2 & result_df$FREMTYPE == 100))
  
  # New covariate (AGE, FREMTYPE=200) should be present for both subjects
  expect_equal(sum(result_df$FREMTYPE == 200), 2) # 1 obs for each of 2 subjects
  
  # Check that EVID was set to 0 for the new AGE records
  expect_true(all(result_df$EVID[result_df$FREMTYPE == 200] == 0))
  
  # Snapshot the resulting data frame for a comprehensive check
  expect_snapshot_value(result_df, style = "serialize")
})

test_that("augmentFremData preserves row order and prevents Cartesian explosion using match()", {
  
  # 1. Setup Data
  dfFFEM <- data.frame(
    ID = c(2, 2, 1, 1),
    TIME = c(0, 12, 0, 24),
    DV = c(5, 6, 10, 12),
    PHASE1_COL = c(10, 10, 20, 20)
  )
  
  # dfFREM: Simulating Phase 1 output.
  # Intentionally out-of-order (ID 2 before 1) to ensure the function doesn't silently sort.
  dfFREM <- data.frame(
    ID = c(2, 2, 1, 1, 2, 1),
    TIME = c(0, 12, 0, 24, 0, 0),
    FREMTYPE = c(0, 0, 0, 0, 100, 100),
    DV = c(5, 6, 10, 12, 5, 10),
    PHASE1_COL = c(10, 10, 20, 20, 10, 20)
  )
  
  # covList & addedList: Simulating Phase 2 adding a new covariate
  addedList <- c("NEW_COV")
  covList <- list(
    NEW_COV = list(
      Name = "NEW_COV",
      Fremtype = 200,
      Data = data.frame(
        ID = c(2, 1),
        TIME = c(0, 0),
        NEW_COV = c(1, 0)
      )
    )
  )
  
  covnames <- list(covNames = "PHASE1_COV")
  
  # 2. Action
  result_df <- augmentFremData(
    dfFREM = dfFREM, 
    dfFFEM = dfFFEM, 
    covList = covList, 
    addedList = addedList, 
    covnames = covnames, 
    cstrDV = "DV", 
    strID = "ID", 
    iFremTypeIncrease = 100, 
    cstrSetToZero = "TIME", 
    quiet = TRUE
  )
  
  # 3. Assertions
  # A. Cartesian Explosion Check (6 base rows + 2 new covariate rows = 8)
  expect_equal(nrow(result_df), 8)
  
  # B. Order Preservation Check
  # The first 6 rows MUST perfectly match the input dfFREM's scrambled order.
  expect_equal(result_df$ID[1:6], c(2, 2, 1, 1, 2, 1))
  expect_equal(result_df$TIME[1:6], c(0, 12, 0, 24, 0, 0))
  
  # C. Cloning check: The new covariate rows (7 and 8) must clone the base cohort order.
  # Since ID 2 came first in dfFREM, row 7 MUST belong to ID 2.
  expect_equal(result_df$ID[7:8], c(2, 1))
  expect_equal(result_df$PHASE1_COL[7:8], c(10, 20))
  
  # D. The new covariate was properly appended
  expect_equal(result_df$FREMTYPE[7:8], c(200, 200))
})