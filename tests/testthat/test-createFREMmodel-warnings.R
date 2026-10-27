# The single-character IGNORE warning (T37). PMXFrem does not apply IGNORE=c
# itself, so it warns when that filter could remove data records - and only
# then. Per the NONMEM 7.5 help ($DATA), IGNORE=c skips records whose first
# character is c, and IGNORE=@ those whose first non-blank character is a
# letter or @. A numeric first column holds neither, so only the header goes.

singleCharWarnings <- function(expr) {
  n <- 0
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("single-character IGNORE", conditionMessage(w))) n <<- n + 1
    invokeRestart("muffleWarning")
  })
  n
}

writeModel <- function(dir, dataRecord) {
  path <- file.path(dir, "model.mod")
  writeLines(c("$PROBLEM test", "$INPUT ID WT", dataRecord), path)
  path
}

test_that("IGNORE=@ on a numeric first column is not warned about", {
  mod <- writeModel(withr::local_tempdir(), "$DATA data.csv IGNORE=@")
  expect_no_warning(filterDataFromModel(mod, data.frame(ID = 1:2, WT = c(70, 80))))
})

test_that("a filter that could remove data records still warns, with a class", {
  td <- withr::local_tempdir()
  ## A character first column: IGNORE=C would drop the rows that start with C.
  expect_warning(
    filterDataFromModel(writeModel(td, "$DATA data.csv IGNORE=C"), data.frame(ID = c("A1", "C2"), WT = 1:2)),
    class = "pmxfrem_single_char_ignore"
  )
  ## A numeric first column with NA (written as letters), or a digit as the
  ## character, can also hit data records.
  expect_warning(
    filterDataFromModel(writeModel(td, "$DATA data.csv IGNORE=@"), data.frame(ID = c(1, NA), WT = 1:2)),
    class = "pmxfrem_single_char_ignore"
  )
  expect_warning(
    filterDataFromModel(writeModel(td, "$DATA data.csv IGNORE=1"), data.frame(ID = c(1, 2), WT = 1:2)),
    class = "pmxfrem_single_char_ignore"
  )
})

fremWarningCount <- function(modDir) {
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  td <- withr::local_tempdir()
  singleCharWarnings(suppressMessages(capture.output(createFREMmodel(
    modName = "run30", modDevDir = modDir,
    ffemDataFile = file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"),
    covariates = c("AGE", "WT"), numSkipOm = 2,
    cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "RATE", "FOOD", "DV"),
    outputDir = td, fremModName = "frem", quiet = FALSE
  ))))
}

test_that("createFREMmodel() on run30 (IGNORE=@, numeric first column) does not warn", {
  expect_equal(fremWarningCount(system.file("extdata", "SimNeb", package = "PMXFrem")), 0)
})

test_that("an IGNORE PMXFrem wrote itself is never warned about", {
  ## A base model with only a list IGNORE: the one IGNORE=@ in sight is the
  ## minimal model's, which PMXFrem writes for its own data file.
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  td <- withr::local_tempdir()
  file.copy(file.path(src, c("run30.mod", "run30.ext")), td)
  mod <- readLines(file.path(td, "run30.mod"))
  i <- grep("^\\$DATA", mod)
  mod[i] <- sub("IGNORE=@", "", mod[i])
  expect_false(grepl("IGNORE=@", mod[i]))
  writeLines(mod, file.path(td, "run30.mod"))
  expect_equal(fremWarningCount(td), 0)
})
