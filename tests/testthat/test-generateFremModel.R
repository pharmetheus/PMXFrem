library(testthat)

test_that("generateFremModel generates correct model text and OMEGA structure", {
  
  # 1. Setup
  td <- withr::local_tempdir()
  template_model_path <- file.path(td, "template.mod")
  writeLines(c(
    "$PROBLEM Test", 
    "$INPUT ID DV TIME", 
    "$DATA data.csv", 
    "$THETA 1", 
    "$OMEGA BLOCK(1) 0.1",
    "$OMEGA BLOCK(1) 0.2"
  ), template_model_path)
  
  final_df <- data.frame(ID = 1, DV = 1, TIME = 0, FREMTYPE = 0)
  
  # modelState with structural zeros (independent blocks)
  modelState <- list(
    theta = 10, 
    omegaMatrix = diag(c(0.1, 0.2)), 
    thetaFix = 0, 
    numTheta = 1, 
    numOmega = 2
  )
  
  # 2. Action
  model_lines <- generateFremModel(
    final_df         = final_df,
    modelState       = modelState,
    covList          = list(), 
    addedList        = NULL, 
    covnames         = list(covNames = c()),
    strFREMModel     = template_model_path, 
    strNewFREMData   = "final.csv",
    bWriteMod        = FALSE, 
    bWriteFIX        = TRUE, 
    noBaseThetas     = 1, 
    numSkipOm        = 0,
    numParCov        = 2, 
    covEpsNum        = 1, 
    basenames_th     = "BASE", 
    basenames_om     = "BASE",
    dDefaultCovValue = 1e-05, 
    strUpdateType    = "DataAndModel"
  )
  
  # 3. Assertions
  omega_block_declarations <- grep("\\$OMEGA BLOCK", model_lines)
  expect_equal(length(omega_block_declarations), 1)
  expect_match(model_lines[omega_block_declarations], "BLOCK\\(2\\)")
  
  val_lines <- model_lines[(omega_block_declarations + 1):(omega_block_declarations + 2)]
  
  has_lone_zero <- any(grepl("(?<![0-9.])0(?![0-9.])", val_lines, perl = TRUE))
  expect_false(has_lone_zero)
  expect_true(any(grepl("1e-05", val_lines)))
  
  expect_true(any(grepl("\\$INPUT ID DV TIME FREMTYPE", model_lines)))
  expect_true(any(grepl("\\$THETA 10 ; 1 TV_BASE", model_lines)))
  
  # 4. Snapshot
  expect_snapshot_value(stabilize(model_lines), style = "serialize")
})

test_that("generateFremModel handles NoData update type correctly", {
  td <- withr::local_tempdir()
  template_model_path <- file.path(td, "template_nodata.mod")
  writeLines(c("$PROBLEM Test", "$THETA 1", "$OMEGA 0.1"), template_model_path)
  
  modelState <- list(theta = 5, omegaMatrix = matrix(0.05), thetaFix = 1, numTheta = 1, numOmega = 1)
  
  result <- generateFremModel(
    final_df         = NULL,
    modelState       = modelState,
    covList          = list(), 
    addedList        = NULL, 
    covnames         = list(covNames = c()),
    strFREMModel     = template_model_path, 
    strNewFREMData   = "wont_be_written.csv",
    bWriteMod        = FALSE, 
    bWriteFIX        = TRUE, 
    noBaseThetas     = 1, 
    numSkipOm        = 0,
    numParCov        = 1, 
    covEpsNum        = 1, 
    basenames_th     = "BASE", 
    basenames_om     = "BASE",
    dDefaultCovValue = 1e-05, 
    strUpdateType    = "NoData"
  )
  
  expect_false(any(grepl("\\$DATA", result)))
  expect_false(any(grepl("\\$INPUT", result)))
  expect_true(any(grepl("\\$THETA 5 FIX", result)))
})

test_that("generateFremModel exactly preserves base model comments", {
  td <- withr::local_tempdir()
  template_model_path <- file.path(td, "template_comments.mod")
  
  # Mock model with messy, real-world comment formats that we want to keep exactly
  writeLines(c(
    "$PROBLEM Test Comments",
    "$THETA 10 ; 1. TVCL",           # PsN style with period
    "$THETA 20 ; 2 TVV",             # Missing period
    "$THETA 30 ; 3 TV_TVMAT",        # Redundant TV_ prefix
    "$OMEGA BLOCK(3)",
    "0.1 ; 1 BSV_CL",                # Standard BSV
    "0.01 0.2 ; 2. BSV_V",           # PsN style with period
    "0.01 0.01 0.3 ; 3 MAT"          # Missing BSV_ prefix
  ), template_model_path)
  
  modelState <- list(
    theta = c(10, 20, 30), 
    omegaMatrix = diag(c(0.1, 0.2, 0.3)), 
    thetaFix = c(0, 0, 0), 
    numTheta = 3, 
    numOmega = 3
  )
  
  # Action
  result_lines <- generateFremModel(
    final_df         = NULL,
    modelState       = modelState,
    covList          = list(), 
    addedList        = NULL, 
    covnames         = list(covNames = c()),
    strFREMModel     = template_model_path, 
    strNewFREMData   = "dummy.csv",
    bWriteMod        = FALSE, 
    bWriteFIX        = TRUE, 
    noBaseThetas     = 3, 
    numSkipOm        = 0,
    numParCov        = 3, 
    covEpsNum        = 1, 
    basenames_th     = NULL, 
    basenames_om     = NULL,
    dDefaultCovValue = 1e-05, 
    strUpdateType    = "NoData"
  )
  
  # Assertions: Verify it kept EXACTLY what was after the semicolon
  expect_true(any(grepl("\\$THETA 10 ; 1\\. TVCL", result_lines)))
  expect_true(any(grepl("\\$THETA 20 ; 2 TVV", result_lines)))
  expect_true(any(grepl("\\$THETA 30 ; 3 TV_TVMAT", result_lines)))
  
  omega_block_idx <- grep("\\$OMEGA BLOCK", result_lines)
  val_lines <- result_lines[(omega_block_idx + 1):(omega_block_idx + 3)]
  
  expect_true(any(grepl("; 1 BSV_CL", val_lines)))
  expect_true(any(grepl("; 2\\. BSV_V", val_lines)))
  expect_true(any(grepl("; 3 MAT", val_lines)))
})

test_that("generateFremModel falls back to BASE placeholders if comments are missing", {
  td <- withr::local_tempdir()
  template_model_path <- file.path(td, "template_no_comments.mod")
  
  # Mock model with NO comments
  writeLines(c(
    "$PROBLEM Test",
    "$THETA 10", "$THETA 20",
    "$OMEGA BLOCK(2) 0.1 0.01 0.2"
  ), template_model_path)
  
  modelState <- list(
    theta = c(10, 20), omegaMatrix = diag(c(0.1, 0.2)), 
    thetaFix = c(0, 0), numTheta = 2, numOmega = 2
  )
  
  result_lines <- generateFremModel(
    final_df = NULL, modelState = modelState, covList = list(), addedList = NULL, 
    covnames = list(covNames = c()), strFREMModel = template_model_path, strNewFREMData = "dummy.csv",
    bWriteMod = FALSE, bWriteFIX = TRUE, noBaseThetas = 2, numSkipOm = 0, numParCov = 2, 
    covEpsNum = 1, basenames_th = NULL, basenames_om = NULL, dDefaultCovValue = 1e-05, strUpdateType = "NoData"
  )
  
  # Assertions: Verify the fallback mechanism engaged
  expect_true(any(grepl("\\$THETA 10 ; 1 TV_BASE1", result_lines)))
  expect_true(any(grepl("; 2 BSV_BASE2", result_lines)))
})

test_that("generateFremModel reconstructs the full FREMTYPE evaluation block", {
  td <- withr::local_tempdir()
  template_model_path <- file.path(td, "template_fremtype.mod")
  
  # Create a template simulating an incoming Phase 1 minimal model
  writeLines(c(
    "$PROBLEM Test",
    ";;;FREM CODE BEGIN COMPACT",
    ";;;DO NOT MODIFY",
    "      IF(FREMTYPE.EQ.100) THEN",
    ";        WT 1",
    "         Y = COV3 + EPS(1)",
    "         IPRED = COV3",
    "      END IF",
    ";;;FREM CODE END COMPACT"
  ), template_model_path)
  
  # Simulate Phase 2 adding 2 new covariates to the existing WT
  covnames <- list(covNames = "WT")
  addedList <- c("AGE", "SEX_2")
  
  modelState <- list(
    theta = c(1, 2, 3), omegaMatrix = diag(3), 
    thetaFix = c(0, 0, 0), numTheta = 3, numOmega = 3
  )
  
  model_lines <- generateFremModel(
    final_df         = NULL,
    modelState       = modelState,
    covList          = list(
      AGE = list(Name = "AGE", Mean = 40, Var = 15),
      SEX_2 = list(Name = "SEX_2", Mean = 0.5, Var = 0.25)
    ), 
    addedList        = addedList, 
    covnames         = covnames,
    strFREMModel     = template_model_path, 
    strNewFREMData   = "dummy.csv",
    bWriteMod        = FALSE, bWriteFIX = TRUE, 
    noBaseThetas     = 0, numSkipOm = 0, numParCov = 0, 
    covEpsNum        = 2, basenames_th = NULL, basenames_om = NULL,
    dDefaultCovValue = 1e-05, strUpdateType = "NoData"
  )
  
  # ASSERTIONS
  expect_true(any(grepl("FREMTYPE\\.EQ\\.100", model_lines)), info = "Failed to evaluate original covariate (WT).")
  expect_true(any(grepl("FREMTYPE\\.EQ\\.200", model_lines)), info = "Failed to construct FREMTYPE block for first added covariate (AGE).")
  expect_true(any(grepl("FREMTYPE\\.EQ\\.300", model_lines)), info = "Failed to construct FREMTYPE block for second added covariate (SEX_2).")
  expect_true(any(grepl(";\\s+SEX_2 1", model_lines)), info = "Failed to label the final covariate in the evaluation block.")
})