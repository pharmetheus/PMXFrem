test_that("parseBaseModel extracts correct structural information from dummy model", {
  td <- withr::local_tempdir()
  model_path <- file.path(td, "base.mod")
  
  # Create a base model file that meets the new structural requirements
  writeLines(c(
    "$PROBLEM Base Model",            # Line 1
    "$INPUT ID TIME DV AMT",          # Line 2
    "$PK",                            # Line 3
    "  CL = THETA(1) * EXP(ETA(1))",  # Line 4
    "  V  = THETA(2) * EXP(ETA(2))",  # Line 5 <- End of $PK block
    "$ERROR",                         # Line 6
    "  Y = F + EPS(1)",               # Line 7 <- End of $ERROR block
    "$THETA (10 FIX) ; CL",           # Line 8
    "$THETA 80 ; V",                  # Line 9
    "$OMEGA 0.1 ; IIV RES (skipped)",
    "$OMEGA BLOCK(2) 0.2 0.01 0.3 ; IIV CL, IIV V"
  ), model_path)
  
  # Call the parser, skipping the first OMEGA
  parsedInfo <- parseBaseModel(baseModelFile = model_path, numSkipOm = 1)
  
  # Assertions
  expect_equal(parsedInfo$numThetas, 2)
  expect_equal(parsedInfo$numOmegas, 3)
  expect_equal(parsedInfo$numParCov, 2)
  expect_equal(parsedInfo$lastThetaLine, 9)
  expect_equal(parsedInfo$muInsertLine, 5)
  expect_equal(parsedInfo$fremBlockInsertLine, 7)
  expect_length(parsedInfo$modelLines, 11)
})

test_that("parseBaseModel handles real-world model files correctly", {
  
  # Test with run30.mod, which should PASS validation with numSkipOm = 2
  run30_path <- system.file("extdata/SimNeb/run30.mod", package = "PMXFrem")
  
  parsedInfo_run30 <- NULL
  expect_no_error({
    parsedInfo_run30 <- parseBaseModel(baseModelFile = run30_path, numSkipOm = 2)
  })
  expect_equal(parsedInfo_run30$numOmegas, 5)
  expect_equal(parsedInfo_run30$numParCov, 3)
  
  # Test with run5.mod, which should FAIL validation
  run5_path <- system.file("extdata/SimVal/run5.mod", package = "PMXFrem")
  
  expect_error(
    parseBaseModel(baseModelFile = run5_path, numSkipOm = 2),
    regexp = "The base model's \\$OMEGA structure is not suitable for FREM"
  )
})

test_that("parseBaseModel handles edge cases and invalid structures", {
  td <- withr::local_tempdir()
  model_path <- file.path(td, "edge_case.mod")
  
  # Test 1: User-specified insertion points are respected
  writeLines(c("$PK", "$ERROR", "$THETA 1", "$OMEGA BLOCK(1) 1"), model_path)
  parsedInfo <- parseBaseModel(
    baseModelFile = model_path, 
    numSkipOm = 0,
    muInsertLineUser = 123,
    fremBlockInsertLineUser = 456
  )
  expect_equal(parsedInfo$muInsertLine, 123)
  expect_equal(parsedInfo$fremBlockInsertLine, 456)
  
  # Test 2: Model is missing a required $PK block
  writeLines(c("$ERROR", "$THETA 1", "$OMEGA BLOCK(1) 1"), model_path)
  expect_error(
    parseBaseModel(model_path, 0),
    regexp = "Could not find a \\$PK block"
  )
  
  # Test 3: Model is missing a required $ERROR block
  writeLines(c("$PK", "$THETA 1", "$OMEGA BLOCK(1) 1"), model_path)
  expect_error(
    parseBaseModel(model_path, 0),
    # FIX: Corrected "an" to "a"
    regexp = "Could not find a \\$ERROR block"
  )
  
  # Test 4: $PK is the last block in the file
  writeLines(c("$THETA 1", "$OMEGA BLOCK(1) 1", "$ERROR", "$PK"), model_path)
  parsedInfo <- parseBaseModel(model_path, 0)
  expect_equal(parsedInfo$muInsertLine, 4)
  
  # Test 5: $OMEGA block only contains SAME records
  writeLines(c("$PK", "$ERROR", "$THETA 1", "$OMEGA SAME"), model_path)
  expect_error(
    parseBaseModel(model_path, 0),
    regexp = "Could not find any diagonal OMEGA records"
  )
  
  # Test 6: Mismatched numSkipOm
  run30_path <- system.file("extdata/SimNeb/run30.mod", package = "PMXFrem")
  expect_error(
    parseBaseModel(baseModelFile = run30_path, numSkipOm = 1),
    regexp = "does not match the specified numSkipOm"
  )
  
  # Test 7: Model is missing a required $OMEGA block
  writeLines(c("$PK", "$ERROR", "$THETA 1"), model_path)
  expect_error(
    parseBaseModel(model_path, 0),
    regexp = "Could not find an \\$OMEGA block"
  )
})

test_that("parseBaseModel throws correct errors for unsuitable structures", {
  td <- withr::local_tempdir()
  model_path <- file.path(td, "bad_structure.mod")
  
  # Test for Line 54: Model is missing a required $THETA block
  writeLines(c("$PK", "$ERROR", "$OMEGA BLOCK(1) 1"), model_path)
  expect_error(
    parseBaseModel(model_path, 0),
    regexp = "Could not find any initial estimates in the \\$THETA block"
  )
  
  # Test for Lines 78-82: Model has diagonal OMEGAs but no BLOCK record
  # We can use run5.mod for this, as it has this exact structure.
  run5_path <- system.file("extdata/SimVal/run5.mod", package = "PMXFrem")
  expect_error(
    parseBaseModel(baseModelFile = run5_path, numSkipOm = 2),
    regexp = "The OMEGAs to be used in FREM must be defined in a final '\\$OMEGA BLOCK\\(N\\)' record"
  )
  
  # Test for Lines 87-92: A simple $OMEGA record appears after the final BLOCK
  writeLines(c(
    "$PK", "$ERROR", "$THETA 1", 
    "$OMEGA BLOCK(1) 0.1",
    "$OMEGA 0.2" # This line is illegal according to our rules
  ), model_path)
  expect_error(
    parseBaseModel(model_path, 0),
    regexp = "Simple \\$OMEGA records were found after the final BLOCK"
  )
  
  # Test for Lines 115-119: Mismatched numSkipOm
  # run30.mod has 2 simple OMEGAs before its BLOCK. Calling with numSkipOm = 1 should fail.
  run30_path <- system.file("extdata/SimNeb/run30.mod", package = "PMXFrem")
  expect_error(
    parseBaseModel(baseModelFile = run30_path, numSkipOm = 1),
    regexp = "does not match the specified numSkipOm"
  )
})