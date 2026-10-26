# The $DATA record of a generated FFEM model: the new data file's path as
# given - a Windows path keeps its backslashes - and the base model's
# options kept.

ffemDataRecord <- function(dataLines, newDataFile) {
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  td <- withr::local_tempdir()
  file.copy(file.path(src, c("run30.mod", "run30.ext", "run30.lst", "run30.phi")), td)
  mod <- readLines(file.path(td, "run30.mod"))
  start <- grep("^\\$DATA", mod)
  nxt <- grep("^\\$", mod)
  end <- nxt[nxt > start][1] - 1
  mod <- c(mod[seq_len(start - 1)], dataLines, mod[(end + 1):length(mod)])
  writeLines(mod, file.path(td, "run30.mod"))

  withr::local_dir(td)
  lines <- suppressWarnings(suppressMessages(createFFEMmodel(
    runno = 31, modDevDir = src, numNonFREMThetas = 7, numSkipOm = 2,
    parNames = c("CL", "V", "MAT"),
    dataFile = file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"),
    newDataFile = newDataFile, quiet = TRUE,
    baserunno = 30, baseModDevDir = td
  )))
  s <- grep("^\\$DATA", lines)
  nx <- grep("^\\$", lines)
  lines[s:(nx[nx > s][1] - 1)]
}

test_that("a Windows path in newDataFile keeps its backslashes", {
  ## A real folder, written with backslashes as Windows users type paths.
  newData <- paste0(normalizePath(withr::local_tempdir(), winslash = "\\"), "\\ffem.csv")
  skip_if_not(grepl("\\", newData, fixed = TRUE), "no backslashes in a path on this OS")
  rec <- ffemDataRecord(c("$DATA ../data.csv IGNORE=@", "      IGNORE(BLQ.EQN.1)"), newData)
  expect_equal(rec[1], paste("$DATA", newData, "IGNORE=@"))
  expect_equal(trimws(rec[2]), "IGNORE(BLQ.EQN.1)")
})

test_that("the base model's $DATA options are kept, and the old file name is not", {
  rec <- ffemDataRecord("$DATA ../data.csv NOREWIND", "ffem.csv")
  expect_equal(rec[1], "$DATA ffem.csv NOREWIND")

  rec <- ffemDataRecord("$DATA ../data.csv", "ffem.csv")
  expect_equal(rec[1], "$DATA ffem.csv")
})
