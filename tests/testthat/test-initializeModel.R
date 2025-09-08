test_that("initializeModelParameters works when .ext file exists", {
  # Setup a self-contained temporary environment
  td <- withr::local_tempdir()
  
  # Copy necessary input files into the temporary directory
  model_path <- file.path(td, "run31.mod")
  ext_path <- file.path(td, "run31.ext") # Ensure .ext is present
  file.copy(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), model_path)
  file.copy(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"), ext_path)
  
  # Call the function under test
  modelState <- initializeModelParameters(
    strFREMModel = model_path,
    numNonFREMThetas = 7,
    numSkipOm = 2,
    numParCov = NULL # Let the function calculate it
  )
  
  # The output is a structured list, which is stable for snapshotting
  expect_snapshot_value(modelState, style = "serialize")
})

test_that("initializeModelParameters works by parsing .mod when .ext is missing", {
  # Setup a self-contained temporary environment
  td <- withr::local_tempdir()
  
  # Copy only the model file, no .ext file
  model_path <- file.path(td, "run31.mod")
  file.copy(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), model_path)
  
  # Call the function under test. numParCov is required here.
  modelState <- initializeModelParameters(
    strFREMModel = model_path,
    numNonFREMThetas = 7,
    numSkipOm = 2,
    numParCov = 3 # This value must be provided when no .ext file exists
  )
  
  # The snapshot should reflect that theta/omega values are NULL,
  # but the parameter counts are correctly parsed.
  expect_snapshot_value(modelState, style = "serialize")
})

test_that("initializeModelParameters stops correctly when .ext is missing and numParCov is NULL", {
  # Setup a self-contained temporary environment
  td <- withr::local_tempdir()
  
  # Copy only the model file
  model_path <- file.path(td, "run31.mod")
  file.copy(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), model_path)
  
  # Expect the function to throw a specific error
  expect_error(
    initializeModelParameters(
      strFREMModel = model_path,
      numNonFREMThetas = 7,
      numSkipOm = 2,
      numParCov = NULL # The condition that should trigger the error
    ),
    regexp = "If no \\*.ext file exist, the number of parameters \\(numParCov\\) needs to be specified!"
  )
})

test_that("initializeModelParameters stops correctly when model file is missing", {
  # No files needed, just a path to a non-existent file
  non_existent_path <- "non_existent_model.mod"
  
  expect_error(
    initializeModelParameters(
      strFREMModel = non_existent_path,
      numNonFREMThetas = 7,
      numSkipOm = 2,
      numParCov = 3
    ),
    regexp = "Cannot find the FREM model"
  )
})