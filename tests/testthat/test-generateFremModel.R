test_that("generateFremModel generates correct model text", {
  
  # 1. Setup
  td <- withr::local_tempdir()
  template_model_path <- file.path(td, "template.mod")
  writeLines(c("$PROBLEM Test", "$INPUT ID DV", "$DATA data.csv", "$THETA 1", "$OMEGA 1"), template_model_path)
  
  final_df <- data.frame(ID = 1, DV = 1, TIME = 0, FREMTYPE = 0)
  modelState <- list(theta = 10, omegaMatrix = matrix(1), thetaFix = 0, numTheta = 1, numOmega = 1)
  
  # 2. Action
  model_lines <- generateFremModel(
    final_df = final_df,
    modelState = modelState,
    covList = list(), addedList = NULL, covnames = list(covNames = c()),
    strFREMModel = template_model_path, strNewFREMData = "final.csv",
    bWriteMod = FALSE, bWriteFIX = TRUE, noBaseThetas = 1, numSkipOm = 0,
    numParCov = 1, covEpsNum = 1, basenames_th = "BASE", basenames_om = "BASE",
    dDefaultCovValue = 0.01, strUpdateType = "DataAndModel"
  )
  
  # 3. Assertions
  expect_true(any(grepl("\\$INPUT ID DV TIME FREMTYPE", model_lines)))
  expect_true(any(grepl("\\$THETA 10  ; 1 TV_BASE", model_lines)))
  
  # Snapshot for comprehensive check
  expect_snapshot_value(model_lines, style = "serialize")
})