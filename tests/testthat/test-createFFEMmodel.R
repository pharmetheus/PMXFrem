test_that("createFFEMmodel works", {

  # Setup a self-contained temporary environment for all file operations
  td <- withr::local_tempdir()

  modDevDir <- system.file("extdata/SimNeb",package="PMXFrem")
  dataFile  <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv",package="PMXFrem")
  fremRun   <- 31
  baseRun1  <- 30
  baseRun2  <- "30a"

  # Define the output path for the data file inside the temporary directory
  new_data_path <- file.path(td, "testDataFile.csv")

  expect_error(createFFEMmodel())
  expect_error(createFFEMmodel(runno=6))
  expect_error(createFFEMmodel(modName="run6"))
  expect_error(createFFEMmodel(baserunno=6))
  expect_error(createFFEMmodel(baseModdName="run6"))


  ffemMod1 <- createFFEMmodel(runno              = fremRun,
                              modDevDir          = modDevDir,
                              numNonFREMThetas   = 7,
                              numSkipOm          = 2,
                              parNames           = c("CL","V","MAT"),
                              dataFile           = dataFile,
                              newDataFile        = new_data_path, # Use the safe, temporary path
                              quiet              = TRUE,
                              baserunno          = baseRun1)

  # Verify that the file was actually written as a side effect
  expect_true(file.exists(new_data_path))
  # Use the generic stabilize() for a character vector, but replace the temp path first
  ffemMod1_stabilized <- gsub(td, "[placeholder_path]", ffemMod1, fixed = TRUE)
  expect_snapshot_value(stabilize(ffemMod1_stabilized), style = "serialize")

  ffemMod2 <- createFFEMmodel(runno              = fremRun,
                              modDevDir          = modDevDir,
                              numNonFREMThetas   = 7,
                              numSkipOm          = 2,
                              parNames           = c("CL","V","MAT"),
                              dataFile           = dataFile,
                              newDataFile        = new_data_path, # Use the safe, temporary path
                              quiet              = TRUE,
                              baserunno          = baseRun2)
  # Use the generic stabilize() for a character vector, but replace the temp path first
  ffemMod2_stabilized <- gsub(td, "[placeholder_path]", ffemMod2, fixed = TRUE)
  expect_snapshot_value(stabilize(ffemMod2_stabilized), style = "serialize")

})

test_that("createFFEMmodel generates Cholesky code when omegaToData = TRUE", {
  
  # Setup a self-contained temporary environment
  td <- withr::local_tempdir()
  modDevDir <- system.file("extdata/SimNeb", package="PMXFrem")
  dataFile  <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package="PMXFrem")
  fremRun   <- 31
  baseRun   <- 30
  new_data_path <- file.path(td, "testDataFile_cholesky.csv")
  
  # Generate the model in memory (quiet = TRUE, no file write needed to check the returned character vector)
  cholesky_model_lines <- createFFEMmodel(
    runno            = fremRun,
    modDevDir        = modDevDir,
    numNonFREMThetas = 7,
    numSkipOm        = 2, # Tests index offsetting natively
    parNames         = c("CL", "V", "MAT"), # 3 structural parameters
    dataFile         = dataFile,
    newDataFile      = new_data_path, 
    quiet            = TRUE,
    baserunno        = baseRun,
    omegaToData      = TRUE
  )
  
  # 1. Verify the Identity $OMEGA Block Injection
  # Since numSkipOm = 2 and there are 3 parNames, we expect the Cholesky block to be BLOCK(3) FIX
  expect_true(any(grepl("\\$OMEGA BLOCK\\(3\\) FIX", cholesky_model_lines)))
  
  # Check for the diagonal 1.0 injections (Identity matrix signature)
  # The third line of a 3x3 identity block should be "0.0 0.0 1.0"
  expect_true(any(grepl("0\\.0\\s+0\\.0\\s+1\\.0", cholesky_model_lines)))
  
  # 2. Verify ETA -> MYETA Translation
  # Because numSkipOm = 2, the first structural ETA is ETA(3). It should now be MYETA3.
  expect_true(any(grepl("MYETA3 \\+ CLFREMCOV", cholesky_model_lines)))
  expect_true(any(grepl("MYETA4 \\+ VFREMCOV", cholesky_model_lines)))
  expect_false(any(grepl("ETA\\(3\\)\\+CLFREMCOV", cholesky_model_lines))) # Ensure the old syntax is gone
  
  # 3. Verify Cholesky Math Injection
  # We should find the base L33 generation line
  expect_true(any(grepl("L33 = SQRT\\(MAX\\(0\\.000001, V33\\)\\)", cholesky_model_lines)))
  # We should find the MYETA correlation linkage
  expect_true(any(grepl("MYETA4 = L43 \\* ETA\\(3\\) \\+ L44 \\* ETA\\(4\\)", cholesky_model_lines)))
  
  # 4. Snapshot the final output to lock in the structural state
  cholesky_stabilized <- gsub(td, "[placeholder_path]", cholesky_model_lines, fixed = TRUE)
  expect_snapshot_value(stabilize(cholesky_stabilized), style = "serialize")
})
