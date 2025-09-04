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
