# --- Helper function to build dummy files for testing ---
setup_dummy_frem_files <- function(td) {
  base_mod_path <- file.path(td, "base.mod")
  base_ext_path <- file.path(td, "base.ext")
  data_path     <- file.path(td, "data.csv")
  
  # 1. Base Model
  writeLines(c(
    "$PROBLEM Base Model",
    "$INPUT ID TIME AMT DV WT RACEL",
    "$DATA data.csv IGNORE=@",
    "$PK", " CL = THETA(1) * EXP(ETA(1))",
    "$ERROR", " Y = F + EPS(1)",
    "$THETA 10",
    "$OMEGA BLOCK(1) 0.1",
    "$SIGMA 1 FIX"
  ), base_mod_path)
  
  # 2. Base Ext
  writeLines(c(
    "TABLE NO. 1",
    " ITERATION  THETA1  SIGMA(1,1)  OMEGA(1,1)  OBJ",
    " -1000000000  1.00000E+01  1.00000E+00  1.00000E-01  1.00000E+02",
    " -1000000006  0.00000E+00  1.00000E+00  0.00000E+00  0.00000E+00"
  ), base_ext_path)
  
  # 3. Base Data
  dummy_data <- data.frame(
    ID    = c(1, 1, 2, 2, 3, 3),
    TIME  = c(0, 1, 0, 1, 0, 1),
    AMT   = c(100, 0, 100, 0, 100, 0),
    DV    = c(0, 5, 0, 6, 0, 7),
    WT    = c(70, 70, 80, 80, 90, 90),
    RACEL = c(1, 1, 2, 2, 3, 3)
  )
  write.csv(dummy_data, data_path, row.names = FALSE, quote = FALSE)
  
  # Return the PMX arguments instead of direct paths!
  return(list(modName = "base", modDevDir = td, csv = data_path))
}

test_that("createFREMmodel_phase1 processes a continuous covariate", {
  missVal <- -99
  td <- withr::local_tempdir()
  out_dir <- file.path(td, "out_cont")
  files <- setup_dummy_frem_files(td)
  
  result <- createFREMmodel_phase1(
    modName       = files$modName,
    modDevDir     = files$modDevDir,
    ffemDataFile  = files$csv,
    covariates    = c("WT"),
    outputDir     = out_dir,
    cstrKeepCols  = c("ID", "TIME", "AMT", "DV"), 
    missVal       = missVal,
    fixTheta      = TRUE,
    roundMeanTo   = 1,
    catCovs       = NULL,
    useMuModeling = FALSE,
    quiet         = TRUE
  )
  
  expect_true(file.exists(result$minimalModelFile))
  expect_true(file.exists(result$minimalDataFile))
  expect_true("validatedData" %in% names(result))
  expect_s3_class(result$validatedData, "data.frame")
  
  # Ensure the data passed back matches the expected input variables
  expect_true(all(c("ID", "TIME", "AMT", "DV", "WT") %in% names(result$validatedData)))
  
  mod_lines <- readLines(result$minimalModelFile)
  # Updated regex to match the new dynamic index (e.g., "; 2 TV_WT")
  expect_true(any(grepl("\\$THETA\\s+80\\s+FIX\\s+;\\s+\\d+\\s+TV_WT", mod_lines)))
  expect_true(any(grepl("FREMTYPE\\.EQ\\.100", mod_lines)))
})

test_that("createFREMmodel_phase1 handles polychotomous Y-1 expansion", {
  missVal <- -99
  td <- withr::local_tempdir()
  out_dir <- file.path(td, "out_cat")
  files <- setup_dummy_frem_files(td)
  
  result <- createFREMmodel_phase1(
    modName       = files$modName,
    modDevDir     = files$modDevDir,
    ffemDataFile  = files$csv,
    covariates    = c("RACEL"), 
    outputDir     = out_dir,
    cstrKeepCols  = c("ID", "TIME", "AMT", "DV"), 
    catCovs       = c("RACEL"), 
    missVal       = missVal,
    roundMeanTo   = 3,
    quiet         = TRUE
  )
  
  mod_lines <- readLines(result$minimalModelFile)
  
  expect_true(any(grepl("TV_RACEL_2", mod_lines)))
  expect_true(any(grepl("TV_RACEL_3", mod_lines)))
  expect_true(any(grepl("FREMTYPE\\.EQ\\.100", mod_lines)))
  expect_true(any(grepl("FREMTYPE\\.EQ\\.200", mod_lines)))
  expect_true(any(grepl("OMEGA BLOCK\\(3\\)", mod_lines, ignore.case = TRUE)))
})

test_that("createFREMmodel_phase1 handles a binary (0/1) categorical covariate", {
  td <- withr::local_tempdir()
  files <- setup_dummy_frem_files(td)

  # rewrite the model + data with a strict 0/1 binary covariate SEX
  writeLines(c(
    "$PROBLEM Base Model",
    "$INPUT ID TIME AMT DV WT SEX",
    "$DATA data.csv IGNORE=@",
    "$PK", " CL = THETA(1) * EXP(ETA(1))",
    "$ERROR", " Y = F + EPS(1)",
    "$THETA 10", "$OMEGA BLOCK(1) 0.1", "$SIGMA 1 FIX"
  ), file.path(td, "base.mod"))
  write.csv(data.frame(
    ID   = c(1, 1, 2, 2, 3, 3, 4, 4),
    TIME = c(0, 1, 0, 1, 0, 1, 0, 1),
    AMT  = c(100, 0, 100, 0, 100, 0, 100, 0),
    DV   = c(0, 5, 0, 6, 0, 7, 0, 8),
    WT   = c(70, 70, 80, 80, 90, 90, 100, 100),
    SEX  = c(0, 0, 1, 1, 0, 0, 1, 1)
  ), files$csv, row.names = FALSE, quote = FALSE)

  result <- createFREMmodel_phase1(
    modName      = files$modName,
    modDevDir    = files$modDevDir,
    ffemDataFile = files$csv,
    covariates   = c("SEX"),
    outputDir    = file.path(td, "out_bin"),
    cstrKeepCols = c("ID", "TIME", "AMT", "DV"),
    catCovs      = c("SEX"),
    roundMeanTo  = 3,
    quiet        = TRUE
  )

  mod_lines <- readLines(result$minimalModelFile)
  # a single dummy for the higher level, and only one FREMTYPE for it
  expect_true(any(grepl("TV_SEX_1", mod_lines)))
  expect_true(any(grepl("FREMTYPE\\.EQ\\.100", mod_lines)))
  expect_false(any(grepl("FREMTYPE\\.EQ\\.200", mod_lines)))
})

test_that("createFREMmodel_phase1 talks through its steps when quiet = FALSE", {
  td <- withr::local_tempdir()
  files <- setup_dummy_frem_files(td)

  # no outputDir -> defaults to modDevDir; quiet = FALSE -> step messages.
  # The IGNORE=@ single-char-IGNORE warning from filterDataFromModel is
  # unrelated noise here.
  expect_message(
    suppressWarnings(createFREMmodel_phase1(
      modName      = files$modName,
      modDevDir    = files$modDevDir,
      ffemDataFile = files$csv,
      covariates   = c("WT"),
      cstrKeepCols = c("ID", "TIME", "AMT", "DV"),
      roundMeanTo  = 1,
      quiet        = FALSE
    )),
    "Phase 1 complete"
  )
})

test_that("createFREMmodel_phase1 safely applies log transformations", {
  td <- withr::local_tempdir()
  out_dir <- file.path(td, "out_log")
  files <- setup_dummy_frem_files(td)
  
  result <- createFREMmodel_phase1(
    modName       = files$modName,
    modDevDir     = files$modDevDir,
    ffemDataFile  = files$csv,
    covariates    = c("WT"),
    outputDir     = out_dir,
    cstrKeepCols  = c("ID", "TIME", "AMT", "DV"), 
    logtCovs      = c("WT"), 
    roundMeanTo   = 3,
    quiet         = TRUE
  )
  
  mod_lines <- readLines(result$minimalModelFile)
  
  # Math check: mean of log(c(70, 80, 90)) is ~4.377
  # Updated regex to match the new dynamic index (e.g., "; 2 TV_WT")
  expect_true(any(grepl("\\$THETA\\s+4.377\\s+FIX\\s+;\\s+\\d+\\s+TV_WT", mod_lines)))
})
