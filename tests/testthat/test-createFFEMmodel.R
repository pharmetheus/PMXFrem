test_that("createFFEMmodel works", {

  modDevDir <- system.file("extdata/SimNeb",package="PMXFrem")
  dataFile  <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv",package="PMXFrem")
  fremRun   <- 31
  baseRun1  <- 30
  baseRun2  <- "30a"


  expect_error(createFFEMmodel())
  expect_error(createFFEMmodel(runno=6))
  expect_error(createFFEMmodel(modName="run6"))
  expect_error(createFFEMmodel(baserunno=6))
  expect_error(createFFEMmodel(baseModdName="run6"))


  ffemMod1 <- createFFEMmodel(runno            = fremRun,
                              modDevDir        = modDevDir,
                              numNonFREMThetas = 7,
                              numSkipOm        = 2,
                              parNames         = c("CL","V","MAT"),
                              dataFile         = dataFile,
                              newDataFile      = "testDataFile.csv",
                              quiet            = TRUE,
                              baserunno        = baseRun1)
  expect_snapshot(stabilize(ffemMod1))

  ffemMod2 <- createFFEMmodel(runno            = fremRun,
                              modDevDir        = modDevDir,
                              numNonFREMThetas = 7,
                              numSkipOm        = 2,
                              parNames         = c("CL","V","MAT"),
                              dataFile         = dataFile,
                              newDataFile      = "testDataFile.csv",
                              quiet            = TRUE,
                              baserunno        = baseRun2)
  expect_snapshot(stabilize(ffemMod2))

})
