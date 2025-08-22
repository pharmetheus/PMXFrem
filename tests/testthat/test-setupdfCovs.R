test_that("setupdfCovs works", {

  modDevDir <- system.file("extdata/SimNeb",package="PMXFrem")
  fremRunno <- 31
  modFile   <- file.path(modDevDir,paste0("run",fremRunno,".mod"))

  dfData <- read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"))

  expect_snapshot(stabilize(setupdfCovs(modFile)))
  expect_error(setupdfCovs(modFile,fremCovs="tmp"))

  expect_snapshot(stabilize(setupdfCovs(modFile,fremCovs = c("AGE","SEX"))))
  expect_snapshot(stabilize(setupdfCovs(modFile,additionalCovs = "FORM")))
  expect_snapshot(stabilize(setupdfCovs(modFile,fremCovs = c("AGE","SEX"),additionalCovs = "FORM")))
})
