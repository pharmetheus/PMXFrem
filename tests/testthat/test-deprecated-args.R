# The old names of the data set arguments still work, with a warning.

base_data <- function() {
  system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")
}

test_that("createFREMmodel(ffemDataFile =) warns and gives the same result as dataFile =", {
  modDevDir <- system.file("extdata", "SimNeb", package = "PMXFrem")
  dataFile <- file.path(modDevDir, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")
  run <- function(...) {
    outDir <- withr::local_tempdir(.local_envir = parent.frame())
    createFREMmodel(
      modName = "run30", modDevDir = modDevDir, ..., covariates = "WT",
      numNonFREMThetas = 7, numSkipOm = 2, outputDir = outDir,
      cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "RATE", "FOOD", "DAY", "BLQ"),
      fremModName = "frem_dep", quiet = TRUE
    )
  }
  resNew <- suppressWarnings(run(dataFile = dataFile))

  warnings <- character()
  resOld <- withCallingHandlers(
    run(ffemDataFile = dataFile),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(
    "`createFREMmodel(ffemDataFile =)` is deprecated; use `dataFile =` instead." %in% warnings
  )
  expect_identical(readLines(resOld$model), readLines(resNew$model))
  expect_identical(readLines(resOld$data), readLines(resNew$data))
})

test_that("createFREMmodel still errors when no data set is given", {
  expect_error(
    suppressWarnings(createFREMmodel(
      modName = "run30", modDevDir = system.file("extdata", "SimNeb", package = "PMXFrem"),
      covariates = "WT", numNonFREMThetas = 7, numSkipOm = 2, outputDir = withr::local_tempdir(),
      quiet = TRUE
    )),
    'argument "dataFile" is missing'
  )
})

test_that("createFREMData(strFFEMData =, strFREMDataFileName =) warns and matches the new names", {
  out <- withr::local_tempfile(fileext = ".csv")
  outOld <- withr::local_tempfile(fileext = ".csv")
  new <- createFREMData(dataFile = base_data(), fremDataFile = out, cstrKeepCols = "ID")

  expect_warning(
    old1 <- createFREMData(strFFEMData = base_data(), cstrKeepCols = "ID"),
    "`createFREMData(strFFEMData =)` is deprecated; use `dataFile =` instead.",
    fixed = TRUE
  )
  expect_equal(old1, new)

  expect_warning(
    old2 <- createFREMData(base_data(), strFREMDataFileName = outOld, cstrKeepCols = "ID"),
    "`createFREMData(strFREMDataFileName =)` is deprecated; use `fremDataFile =` instead.",
    fixed = TRUE
  )
  expect_equal(old2, new)
  expect_identical(readLines(outOld), readLines(out))
})

test_that("updateFREMmodel(strFREMData =, strFFEMData =, strNewFREMData =) warns and matches the new names", {
  td <- withr::local_tempdir()
  for (f in c("run31.mod", "run31.ext", "frem_dataset.dta", "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")) {
    file.copy(system.file("extdata/SimNeb", f, package = "PMXFrem"), td)
  }
  common <- list(
    strFREMModel = file.path(td, "run31.mod"), cstrRemoveCov = "SEX",
    numNonFREMThetas = 7, numSkipOm = 2, bWriteData = FALSE, bWriteMod = FALSE,
    quiet = TRUE, bWriteFIX = FALSE,
    cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "RATE", "DV", "FOOD", "FREMTYPE")
  )
  fremIn <- file.path(td, "frem_dataset.dta")
  dataIn <- file.path(td, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")
  newOut <- file.path(td, "new.csv")

  new <- do.call(updateFREMmodel, c(common, list(
    fremDataFile = fremIn, dataFile = dataIn, newFremDataFile = newOut
  )))

  expect_warning(
    old <- do.call(updateFREMmodel, c(common, list(
      strFREMData = fremIn, dataFile = dataIn, newFremDataFile = newOut
    ))),
    "`updateFREMmodel(strFREMData =)` is deprecated; use `fremDataFile =` instead.",
    fixed = TRUE
  )
  expect_equal(old, new)

  expect_warning(
    old <- do.call(updateFREMmodel, c(common, list(
      fremDataFile = fremIn, strFFEMData = dataIn, newFremDataFile = newOut
    ))),
    "`updateFREMmodel(strFFEMData =)` is deprecated; use `dataFile =` instead.",
    fixed = TRUE
  )
  expect_equal(old, new)

  expect_warning(
    old <- do.call(updateFREMmodel, c(common, list(
      fremDataFile = fremIn, dataFile = dataIn, strNewFREMData = newOut
    ))),
    "`updateFREMmodel(strNewFREMData =)` is deprecated; use `newFremDataFile =` instead.",
    fixed = TRUE
  )
  expect_equal(old, new)
})

test_that("addFREMcovariates(dfFFEM =) warns and matches data =", {
  df <- data.frame(ID = 1:6, RACEL = c(1, 2, 3, 1, 2, 3))
  new <- addFREMcovariates(data = df, covariates = "RACEL")
  expect_warning(
    old <- addFREMcovariates(dfFFEM = df, covariates = "RACEL"),
    "`addFREMcovariates(dfFFEM =)` is deprecated; use `data =` instead.",
    fixed = TRUE
  )
  expect_equal(old, new)
})
