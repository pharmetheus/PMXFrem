# How createFREMmodel() reads the base model's $INPUT (T34). $INPUT names every
# column of the data file, in order; =DROP or =SKIP only marks a column the
# model does not use, so the data file still has it.

## Write a data file and a model with the given $INPUT and $DATA lines.
writeInputCase <- function(dir, data, input, dataRecord = "$DATA data.csv") {
  dataPath <- file.path(dir, "data.csv")
  modelPath <- file.path(dir, "model.mod")
  write.csv(data, dataPath, row.names = FALSE, quote = FALSE)
  writeLines(c("$PROBLEM test", paste("$INPUT", input), dataRecord), modelPath)
  list(data = dataPath, model = modelPath)
}

test_that("=SKIP and a bare DROP drop their column too", {
  td <- withr::local_tempdir()
  f <- writeInputCase(td, data.frame(A = 1, B = 2, C = 3, D = 4), "ID DV=SKIP DROP WT")
  res <- prepareAndValidateData(f$data, f$model, covariates = "WT", quiet = TRUE)
  expect_equal(names(res$validatedData), c("ID", "WT"))
  expect_equal(res$validatedData$WT, 4)
})

test_that("a column whose name merely contains DROP is kept", {
  td <- withr::local_tempdir()
  f <- writeInputCase(td, data.frame(A = 1, B = 0, C = 70), "ID DROPOUT WT")
  res <- prepareAndValidateData(f$data, f$model, covariates = "WT", quiet = TRUE)
  expect_equal(names(res$validatedData), c("ID", "DROPOUT", "WT"))
})

test_that("IGNORE can filter on a dropped column, which is then removed", {
  td <- withr::local_tempdir()
  f <- writeInputCase(td,
    data.frame(A = 1:3, B = c(1, 2, 1), C = c(70, 80, 90)),
    "ID STUDY=DROP WT",
    dataRecord = "$DATA data.csv IGNORE=(STUDY.EQ.2)"
  )
  res <- prepareAndValidateData(f$data, f$model, covariates = "WT", quiet = TRUE)
  expect_equal(res$validatedData$ID, c(1, 3))
  expect_false("STUDY" %in% names(res$validatedData))
})

test_that("columns beyond the $INPUT items are ignored, and named in a message", {
  td <- withr::local_tempdir()
  f <- writeInputCase(td, data.frame(A = 1, B = 70, EXTRA1 = 5, EXTRA2 = 6), "ID WT")
  expect_message(
    res <- prepareAndValidateData(f$data, f$model, covariates = "WT"),
    "EXTRA1, EXTRA2"
  )
  expect_equal(names(res$validatedData), c("ID", "WT"))
  expect_silent(prepareAndValidateData(f$data, f$model, covariates = "WT", quiet = TRUE))
})

test_that("a dropped column cannot be a covariate or a kept column", {
  td <- withr::local_tempdir()
  f <- writeInputCase(td, data.frame(A = 1, B = 2, C = 70), "ID AGE=DROP WT")
  expect_error(
    prepareAndValidateData(f$data, f$model, covariates = c("WT", "AGE"), quiet = TRUE),
    "AGE.*=DROP"
  )
  expect_error(
    prepareAndValidateData(f$data, f$model, covariates = "WT", cstrKeepCols = c("ID", "AGE"), quiet = TRUE),
    "AGE.*=DROP"
  )
})

test_that("createFREMmodel() accepts a base model with =DROP columns", {
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  td <- withr::local_tempdir()
  file.copy(file.path(src, c("run30.mod", "run30.ext")), td)

  ## A trailing visit-date column that only the data file has, plus two
  ## existing columns marked =DROP - the shape of most real base models.
  dat <- read.csv(file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"))
  dat$VDATE <- "2026-01-01"
  dataFile <- file.path(td, "data.csv")
  write.csv(dat, dataFile, row.names = FALSE, quote = FALSE)

  mod <- readLines(file.path(td, "run30.mod"))
  inputStart <- grep("^\\$INPUT", mod)
  inputEnd <- grep("^\\$DATA", mod) - 1
  mod[inputStart:inputEnd] <- sub(" STUDYID ", " STUDYID=DROP ", mod[inputStart:inputEnd])
  mod[inputStart:inputEnd] <- sub(" DAY ", " DAY=DROP ", mod[inputStart:inputEnd])
  mod[inputEnd] <- paste(mod[inputEnd], "VDATE=DROP")
  input <- paste(mod[inputStart:inputEnd], collapse = " ")
  expect_match(input, "STUDYID=DROP")
  expect_match(input, "DAY=DROP")
  expect_match(input, "VDATE=DROP")
  writeLines(mod, file.path(td, "run30.mod"))

  res <- suppressWarnings(createFREMmodel(
    modName = "run30", modDevDir = td, ffemDataFile = dataFile,
    covariates = c("AGE", "WT"), numSkipOm = 2,
    cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "RATE", "FOOD", "DV"),
    outputDir = td, fremModName = "frem", quiet = TRUE
  ))

  expect_true(file.exists(res$model))
  fremInput <- grep("^\\$INPUT", readLines(res$model), value = TRUE)
  expect_false(grepl("VDATE|STUDYID|DAY", fremInput))
  fremData <- read.csv(res$data)
  expect_setequal(unique(fremData$FREMTYPE), c(0, 100, 200))
})

test_that("a dropped column among the default keep columns is not an error", {
  ## Only a cstrKeepCols the user wrote is checked; the defaults include
  ## columns (II, SS, RATE) many base models drop or lack.
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  td <- withr::local_tempdir()
  file.copy(file.path(src, c("run30.mod", "run30.ext")), td)
  mod <- readLines(file.path(td, "run30.mod"))
  inputStart <- grep("^\\$INPUT", mod)
  mod[inputStart] <- sub(" RATE ", " RATE=DROP ", mod[inputStart])
  expect_match(mod[inputStart], "RATE=DROP")
  writeLines(mod, file.path(td, "run30.mod"))

  expect_no_error(suppressWarnings(createFREMmodel(
    modName = "run30", modDevDir = td,
    ffemDataFile = file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"),
    covariates = "AGE", numSkipOm = 2, outputDir = td, fremModName = "frem", quiet = TRUE
  )))
})
