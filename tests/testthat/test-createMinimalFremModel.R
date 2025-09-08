library(testthat)

test_that("createMinimalFremModel correctly injects code with MU-modeling", {
  td <- withr::local_tempdir()
  base_model_path <- file.path(td, "base.mod")
  frem_data_path <- file.path(td, "frem_data.csv")
  
  writeLines(c(
    "$PROBLEM Base Model", "$INPUT ID TIME DV AMT", "$DATA data.csv IGNORE=@",
    "$PK",
    "  CL = THETA(1) * EXP(ETA(1))",
    "  V  = THETA(2) * EXP(ETA(2))",
    "$ERROR",
    "  Y = F + EPS(1)",
    "$THETA 10",
    "$THETA 80",
    "$OMEGA BLOCK(2) 0.2 0.01 0.3",
    "$SIGMA 1 FIX"
  ), base_model_path)
  
  baseModelInfo <- parseBaseModel(base_model_path, numSkipOm = 0)
  
  initialCovariateInfo <- list(list(
    name = "WT", mean = 75, variance = 10, fremType = 100, shouldFixTheta = TRUE
  ))
  
  minimalModelLines <- createMinimalFremModel(
    baseModelInfo = baseModelInfo,
    initialCovariateInfo = initialCovariateInfo,
    fremDataPath = frem_data_path,
    fremDataHeaders = c("ID", "TIME", "AMT", "DV", "FREMTYPE"),
    covEpsNum = 2,
    useMuModeling = TRUE
  )
  
  expect_true(any(grepl("MU_3 = THETA\\(3\\)", minimalModelLines)))
  # Updated regex to look for the dynamic index (e.g., ; 3 TV_WT)
  expect_true(any(grepl("\\$THETA\\s+75\\s+FIX\\s+;\\s+\\d+\\s+TV_WT", minimalModelLines)))
  expect_true(any(grepl("OMEGA BLOCK\\(3\\)", minimalModelLines, ignore.case = TRUE)))
  expect_true(any(grepl("1e-05\\s+1e-05\\s+10", minimalModelLines)))
  expect_true(any(grepl("\\$SIGMA\\s+1\\s+FIX", minimalModelLines)))
  
  stabilized_lines <- gsub(
    pattern = "(^\\$DATA\\s+)(.*?)(\\s+IGNORE=@)", 
    replacement = "\\1[placeholder_path]\\3", 
    x = minimalModelLines
  )
  
  expect_snapshot_value(stabilize(stabilized_lines), style = "serialize")
})

test_that("createMinimalFremModel works without MU-modeling", {
  td <- withr::local_tempdir()
  base_model_path <- file.path(td, "base.mod")
  frem_data_path <- file.path(td, "frem_data.csv")
  
  writeLines(c(
    "$PROBLEM Base Model", "$INPUT ID TIME DV AMT", "$DATA data.csv IGNORE=@",
    "$PK", "  CL = THETA(1) * EXP(ETA(1))", "  V  = THETA(2) * EXP(ETA(2))",
    "$ERROR", "  Y = F + EPS(1)", "$THETA 10", "$THETA 80",
    "$OMEGA BLOCK(2) 0.2 0.01 0.3",
    "$SIGMA 1 FIX"
  ), base_model_path)
  
  baseModelInfo <- parseBaseModel(base_model_path, numSkipOm = 0)
  initialCovariateInfo <- list(list(
    name = "WT", mean = 75, variance = 10, fremType = 100, shouldFixTheta = FALSE
  ))
  
  minimalModelLines <- createMinimalFremModel(
    baseModelInfo = baseModelInfo,
    initialCovariateInfo = initialCovariateInfo,
    fremDataPath = frem_data_path,
    fremDataHeaders = c("ID", "TIME", "AMT", "DV", "FREMTYPE"),
    covEpsNum = 2,
    useMuModeling = FALSE
  )
  
  expect_false(any(grepl("MU_3", minimalModelLines)))
  expect_true(any(grepl("Y = THETA\\(3\\) \\+ ETA\\(3\\) \\+ EPS\\(2\\)", minimalModelLines)))
  # Updated regex to look for the dynamic index
  expect_true(any(grepl("\\$THETA\\s+75\\s+;\\s+\\d+\\s+TV_WT", minimalModelLines)))
})

test_that("getCovNames can parse the output of createMinimalFremModel", {
  td <- withr::local_tempdir()
  base_model_path <- file.path(td, "base.mod")
  frem_data_path <- file.path(td, "frem_data.csv")
  minimal_model_output_path <- file.path(td, "minimal.mod")
  
  writeLines(c(
    "$PROBLEM Base Model", "$INPUT ID TIME DV AMT", "$DATA data.csv IGNORE=@",
    "$PK",
    "  CL = THETA(1) * EXP(ETA(1))",
    "  V  = THETA(2) * EXP(ETA(2))",
    "$ERROR",
    "  Y = F + EPS(1)",
    "$THETA 10",
    "$THETA 80",
    "$OMEGA BLOCK(2) 0.2 0.01 0.3",
    "$SIGMA 1 FIX"
  ), base_model_path)
  
  baseModelInfo <- parseBaseModel(base_model_path, numSkipOm = 0)
  
  initialCovariateInfo <- list(list(
    name = "WT", mean = 75, variance = 10, fremType = 100, shouldFixTheta = TRUE
  ))
  
  minimalModelLines <- createMinimalFremModel(
    baseModelInfo = baseModelInfo,
    initialCovariateInfo = initialCovariateInfo,
    fremDataPath = frem_data_path,
    fremDataHeaders = c("ID", "TIME", "AMT", "DV", "FREMTYPE"),
    covEpsNum = 2,
    useMuModeling = TRUE
  )
  
  writeLines(minimalModelLines, minimal_model_output_path)
  
  covs_from_output <- NULL
  expect_no_error({
    covs_from_output <- getCovNames(modFile = minimal_model_output_path)
  })
  
  expect_type(covs_from_output, "list")
  expect_named(covs_from_output, c("covNames", "polyCatCovs", "orgCovNames"))
  expect_equal(covs_from_output$covNames, "WT")
  expect_equal(covs_from_output$orgCovNames, "WT")
  expect_length(covs_from_output$polyCatCovs, 0)
})

test_that("createMinimalFremModel handles a real-world user model (run30.mod)", {
  td <- withr::local_tempdir()
  frem_data_path <- file.path(td, "frem_run30_data.csv")
  
  run30_mod_path <- system.file("extdata", "SimNeb", "run30.mod", package = "PMXFrem")
  
  if (!file.exists(run30_mod_path)) {
    skip("Test file 'run30.mod' not found in package 'extdata'.")
  }
  
  baseModelInfo <- parseBaseModel(run30_mod_path, numSkipOm = 2)
  
  initialCovariateInfo <- list(
    list(
      name = "AGE",
      mean = 60,
      variance = 15,
      fremType = 101,
      shouldFixTheta = FALSE 
    )
  )
  
  sigma_values <- parseMatrixBlockToMatrix(baseModelInfo$sigmaBlock)
  num_sigmas <- nrow(sigma_values)
  
  minimalModelLines <- createMinimalFremModel(
    baseModelInfo = baseModelInfo,
    initialCovariateInfo = initialCovariateInfo,
    fremDataPath = frem_data_path,
    fremDataHeaders = c("ID", "TIME", "AMT", "DV", "FREMTYPE"),
    covEpsNum = num_sigmas + 1,
    useMuModeling = TRUE
  )
  
  expected_new_theta <- baseModelInfo$numThetas + 1 
  expected_new_eta <- baseModelInfo$numOmegas + 1   
  
  expect_true(any(grepl(paste0("MU_", expected_new_eta, " = THETA\\(", expected_new_theta, "\\)"), minimalModelLines)))
  expect_true(any(grepl(paste0("COV", expected_new_eta, " = MU_", expected_new_eta, " \\+ ETA\\(", expected_new_eta, "\\)"), minimalModelLines)))
  
  # Updated regex to look for the dynamic index here as well
  expect_true(any(grepl(paste0("\\$THETA\\s+", initialCovariateInfo[[1]]$mean, "\\s+;\\s+\\d+\\s+TV_", initialCovariateInfo[[1]]$name), minimalModelLines)))
  expect_false(any(grepl(paste0("\\$THETA\\s+", initialCovariateInfo[[1]]$mean, "\\s+FIX"), minimalModelLines)))
  
  expected_new_omega_block_size <- baseModelInfo$numParCov + 1
  expect_true(any(grepl(paste0("OMEGA BLOCK\\(", expected_new_omega_block_size, "\\)"), minimalModelLines, ignore.case = TRUE)))
  expect_true(any(grepl(paste0("\\$SIGMA.*", "0.001 FIX"), minimalModelLines)))
  
  stabilized_lines <- gsub(
    pattern = "(^\\$DATA\\s+)(.*?)(\\s+IGNORE=@)", 
    replacement = "\\1[placeholder_path]\\3", 
    x = minimalModelLines
  )
  
  expect_snapshot_value(stabilize(stabilized_lines), style = "serialize")
})

test_that("createMinimalFremModel perfectly preserves base OMEGA comments", {
  td <- withr::local_tempdir()
  base_model_path <- file.path(td, "base_with_comments.mod")
  frem_data_path <- file.path(td, "frem_data.csv")
  
  writeLines(c(
    "$PROBLEM Base Model", "$INPUT ID TIME DV AMT", "$DATA data.csv IGNORE=@",
    "$PK", "  CL = THETA(1) * EXP(ETA(1))",
    "$ERROR", "  Y = F + EPS(1)",
    "$THETA 10",
    "$OMEGA BLOCK(1) 0.1 ; 1. BSV_CL",
    "$SIGMA 1 FIX"
  ), base_model_path)
  
  baseModelInfo <- parseBaseModel(base_model_path, numSkipOm = 0)
  
  initialCovariateInfo <- list(list(
    name = "WT", mean = 75, variance = 10, fremType = 100, shouldFixTheta = TRUE
  ))
  
  minimalModelLines <- createMinimalFremModel(
    baseModelInfo = baseModelInfo,
    initialCovariateInfo = initialCovariateInfo,
    fremDataPath = frem_data_path,
    fremDataHeaders = c("ID", "TIME", "AMT", "DV", "FREMTYPE"),
    covEpsNum = 2,
    useMuModeling = TRUE
  )
  
  omega_block_idx <- grep("OMEGA BLOCK\\(2\\)", minimalModelLines)
  expect_true(any(grepl("; 1\\. BSV_CL", minimalModelLines[omega_block_idx + 1])))
  expect_true(any(grepl("; 2 BSV_WT", minimalModelLines[omega_block_idx + 2])))
})

test_that("createMinimalFremModel correctly assigns global indices to skipped and main omegas", {
  td <- withr::local_tempdir()
  base_model_path <- file.path(td, "base.mod")
  frem_data_path <- file.path(td, "frem_data.csv")
  
  writeLines(c(
    "$PROBLEM Base Model", "$INPUT ID TIME DV AMT", "$DATA data.csv IGNORE=@",
    "$PK", "  CL = THETA(1) * EXP(ETA(1))",
    "$ERROR", "  Y = F + EPS(1)",
    "$THETA 10",
    "$OMEGA 0.1 ; 1. IIV on RUV",
    "$OMEGA 0.2 FIX ; 2. IIV on D1",
    "$OMEGA BLOCK(1) 0.3 ; 3. IIV on CL",
    "$SIGMA 1 FIX"
  ), base_model_path)
  
  baseModelInfo <- parseBaseModel(base_model_path, numSkipOm = 2)
  
  initialCovariateInfo <- list(list(
    name = "WT", mean = 75, variance = 10, fremType = 100, shouldFixTheta = TRUE
  ))
  
  minimalModelLines <- createMinimalFremModel(
    baseModelInfo = baseModelInfo,
    initialCovariateInfo = initialCovariateInfo,
    fremDataPath = frem_data_path,
    fremDataHeaders = c("ID", "TIME", "AMT", "DV", "FREMTYPE"),
    covEpsNum = 2,
    useMuModeling = TRUE
  )
  
  expect_true(any(grepl("\\$OMEGA BLOCK\\(1\\) 0\\.1\\s+; 1\\. IIV on RUV", minimalModelLines)))
  expect_true(any(grepl("\\$OMEGA BLOCK\\(1\\) 0\\.2 FIX ; 2\\. IIV on D1", minimalModelLines)))
  expect_true(any(grepl("OMEGA BLOCK\\(2\\)", minimalModelLines)))
  
  omega_block_idx <- grep("OMEGA BLOCK\\(2\\)", minimalModelLines)
  expect_true(any(grepl("; 3\\. IIV on CL", minimalModelLines[omega_block_idx + 1])))
  expect_true(any(grepl("; 4 BSV_WT", minimalModelLines[omega_block_idx + 2])))
})