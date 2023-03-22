PMXRenv::activate.unqualified.packages()
test_that("createFFEMmodel works", {

  modDevDir <- system.file("extdata/SimNeb",package="PMXFrem")
  dataFile  <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv",package="PMXFrem")
  fremRun   <- 31
  baseRun   <- 30

  ffemMod <- createFFEMmodel(runno            = fremRun,
                             modDevDir        = modDevDir,
                             numNonFREMThetas = 7,
                             numSkipOm        = 2,
                             parNames         = c("CL","V","MAT"),
                             dataFile         = dataFile,
                             newDataFile      = "testDataFile.csv",
                             quiet            = TRUE,
                             baserunno        = baseRun)

  expect_snapshot(ffemMod)

})
