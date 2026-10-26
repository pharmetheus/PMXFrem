# IGNORE / ACCEPT lists compare text where NONMEM does (T41). NONMEM 7.5 help,
# $DATA, IGNORE=(list): "With "=", "==", "/=', ".EQ." and ".NE.", the value in
# the data record and the value in the list are compared as character strings.
# Otherwise, they are converted to numeric and compared numerically. (This is
# the case with .NEN. and .EQN.)"

# A model with one $DATA record, and a data file written as given (text kept).
fixtureModel <- function(dir, dataRecord) {
  path <- file.path(dir, "model.mod")
  writeLines(c("$PROBLEM test", "$INPUT ID BLQ WT", dataRecord), path)
  path
}

fixtureData <- function(dir, blq) {
  path <- file.path(dir, "data.csv")
  writeLines(c("ID,BLQ,WT", paste(seq_along(blq), blq, 70 + seq_along(blq), sep = ",")), path)
  path
}

keptIDs <- function(dataRecord, blq) {
  td <- withr::local_tempdir(.local_envir = parent.frame())
  res <- prepareAndValidateData(
    ffemDataFile = fixtureData(td, blq),
    baseModelFile = fixtureModel(td, dataRecord),
    covariates = "WT", quiet = TRUE, keepDoseOnlySubjects = TRUE
  )
  res$validatedData$ID
}

test_that("a file value written 1.0 is kept by IGNORE=(BLQ.EQ.1) and dropped by .EQN.", {
  blq <- c("1.0", "1", "0")
  # Text: "1.0" != "1", so only the row written 1 is dropped.
  expect_equal(keptIDs("$DATA data.csv IGNORE=(BLQ.EQ.1)", blq), c(1, 3))
  expect_equal(keptIDs("$DATA data.csv IGNORE=(BLQ=1)", blq), c(1, 3))
  expect_equal(keptIDs("$DATA data.csv IGNORE=(BLQ==1)", blq), c(1, 3))
  # Numeric: both 1.0 and 1 equal 1.
  expect_equal(keptIDs("$DATA data.csv IGNORE=(BLQ.EQN.1)", blq), 3)
})

test_that("the list value is text too: BLQ.EQ.1.0 matches only 1.0", {
  expect_equal(keptIDs("$DATA data.csv IGNORE=(BLQ.EQ.1.0)", c("1.0", "1", "0")), c(2, 3))
})

test_that("/= and .NE. are text, .NEN. is numeric, under IGNORE", {
  blq <- c("1.0", "1", "0")
  # Text: "1.0" differs from "1", so rows 1 and 3 are dropped.
  expect_equal(keptIDs("$DATA data.csv IGNORE=(BLQ/=1)", blq), 2)
  expect_equal(keptIDs("$DATA data.csv IGNORE=(BLQ.NE.1)", blq), 2)
  # Numeric: only the 0 differs from 1.
  expect_equal(keptIDs("$DATA data.csv IGNORE=(BLQ.NEN.1)", blq), c(1, 2))
})

test_that("ACCEPT compares the same way", {
  blq <- c("1.0", "1", "0")
  expect_equal(keptIDs("$DATA data.csv ACCEPT=(BLQ.EQ.1)", blq), 2)
  expect_equal(keptIDs("$DATA data.csv ACCEPT=(BLQ.EQN.1)", blq), c(1, 2))
  expect_equal(keptIDs("$DATA data.csv ACCEPT=(BLQ.NE.1)", blq), c(1, 3))
})

test_that("ordering operators are numeric", {
  blq <- c("1.0", "1", "0", "2E+00")
  expect_equal(keptIDs("$DATA data.csv IGNORE=(BLQ.GT.1)", blq), c(1, 2, 3))
  expect_equal(keptIDs("$DATA data.csv IGNORE=(BLQ>=1)", blq), 3)
  expect_equal(keptIDs("$DATA data.csv IGNORE=(BLQ.LT.1)", blq), c(1, 2, 4))
})

test_that("table-file style values compare as written (help: IGNORE=(OCC==1.0000E+00))", {
  blq <- c("1.0000E+00", "0.0000E+00", "1")
  expect_equal(keptIDs("$DATA data.csv IGNORE=(BLQ==1.0000E+00)", blq), c(2, 3))
  expect_equal(keptIDs("$DATA data.csv IGNORE=(BLQ.EQN.1)", blq), 2)
})

test_that("quoted values and quoted commas are taken literally", {
  td <- withr::local_tempdir()
  writeLines(c("ID,BLQ,WT", "1,M,70", "2,F,80", "3,\"a,b\",90"), file.path(td, "data.csv"))
  run <- function(rec) {
    prepareAndValidateData(file.path(td, "data.csv"), fixtureModel(td, rec),
      covariates = "WT", quiet = TRUE, keepDoseOnlySubjects = TRUE
    )$validatedData$ID
  }
  expect_equal(run("$DATA data.csv IGNORE=(BLQ.EQ.'M')"), c(2, 3))
  expect_equal(run("$DATA data.csv IGNORE=(BLQ=\"F\", BLQ='M')"), 3)
  expect_equal(run("$DATA data.csv IGNORE=(BLQ.EQ.'a,b')"), c(1, 2))
})

test_that("several conditions in one list: a record satisfying at least one is dropped", {
  # Help: IGNORE=(GEN.EQ.1,AGE.GT.60) drops GEN equal to 1 or AGE greater than 60.
  blq <- c("1.0", "1", "0", "0")
  expect_equal(keptIDs("$DATA data.csv IGNORE=(BLQ.EQ.1,WT.GT.73)", blq), c(1, 3))
  # ACCEPT=(GEN.NE.1,AGE.LE.60) is "drop where GEN=1 AND AGE>60".
  expect_equal(keptIDs("$DATA data.csv ACCEPT=(BLQ.NE.1,WT.LE.71)", blq), c(1, 3, 4))
})

test_that("data.frame input is compared as as.character() of its values", {
  td <- withr::local_tempdir()
  mod <- fixtureModel(td, "$DATA data.csv IGNORE=(BLQ.EQ.1)")
  df <- data.frame(ID = 1:3, BLQ = c(1, 1, 0), WT = c(70, 80, 90))
  expect_equal(filterDataFromModel(mod, df)$ID, 3)
  # A character column keeps its text.
  df$BLQ <- c("1.0", "1", "0")
  expect_equal(filterDataFromModel(mod, df)$ID, c(1, 3))
})

test_that("rawData supplies the text as written, and wins over wideData", {
  td <- withr::local_tempdir()
  mod <- fixtureModel(td, "$DATA data.csv IGNORE=(BLQ.EQ.1)")
  df <- data.frame(ID = 1:3, BLQ = c(1, 1, 0), WT = c(70, 80, 90))
  raw <- data.frame(ID = c("1", "2", "3"), BLQ = c("1.0", "1", "0"), WT = c("70", "80", "90"))
  expect_equal(filterDataFromModel(mod, df, rawData = raw)$ID, c(1, 3))
  # The returned data keep wideData's types.
  expect_true(is.numeric(filterDataFromModel(mod, df, rawData = raw)$BLQ))
  expect_error(
    filterDataFromModel(mod, df, rawData = raw[1:2, ]),
    "same number of rows"
  )
})

test_that("a label that is not a data column is reported", {
  td <- withr::local_tempdir()
  mod <- fixtureModel(td, "$DATA data.csv IGNORE=(NOPE.EQ.1)")
  expect_error(
    filterDataFromModel(mod, data.frame(ID = 1, BLQ = 0, WT = 70)),
    "NOPE"
  )
})

test_that("updateFREMmodel's file path reads the text too", {
  # filterDataFromModel is handed the raw text of the FFEM file; here through
  # the helper updateFREMmodel uses.
  td <- withr::local_tempdir()
  path <- fixtureData(td, c("1.0", "1", "0"))
  raw <- .readDataAsText(path)
  expect_true(all(vapply(raw, is.character, logical(1))))
  expect_equal(raw$BLQ, c("1.0", "1", "0"))
})
