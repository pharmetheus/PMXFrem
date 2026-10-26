# The base model's $TABLE records are carried into the FREM model (T39).
# run30's table lists data items the FREM data set does not carry (NO,
# STUDYID, TAD, ODV, the covariates), model items (CPRED, CWRES,
# ETAS(1:LAST)) and options (NOPRINT ONEHEADER FILE=xptab30).

fremModelLines <- function(covariates, fremModName = "frem", modDir = NULL, quiet = TRUE) {
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  if (is.null(modDir)) modDir <- src
  td <- withr::local_tempdir()
  res <- suppressWarnings(createFREMmodel(
    modName = "run30", modDevDir = modDir,
    ffemDataFile = file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"),
    covariates = covariates, numSkipOm = 2,
    cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "RATE", "FOOD", "DV"),
    outputDir = td, fremModName = fremModName, quiet = quiet
  ))
  readLines(res$model)
}

## The words of the $TABLE records, one character vector per record.
tableWords <- function(lines) {
  starts <- grep("^\\s*\\$TAB", lines)
  lapply(starts, function(s) {
    nxt <- grep("^\\s*\\$", lines)
    end <- c(nxt[nxt > s], length(lines) + 1)[1] - 1
    w <- strsplit(trimws(gsub(";.*", "", paste(lines[s:end], collapse = " "))), "\\s+")[[1]]
    w[-1]
  })
}

test_that("the base model's $TABLE is kept, pruned to what the FREM data carries", {
  for (covs in list("AGE", c("AGE", "WT"))) {
    tabs <- tableWords(fremModelLines(covs))
    expect_length(tabs, 1)
    w <- tabs[[1]]
    expect_false(any(c("NO", "STUDYID", "TAD", "ODV", "WT", "AGE") %in% w))
    expect_true(all(c("ID", "TIME", "DV", "EVID", "CPRED", "CWRES", "ETAS(1:LAST)", "FREMTYPE", "NOPRINT", "ONEHEADER") %in% w))
    expect_false("FILE=xptab30" %in% w)
  }
})

test_that("the table file is renamed so the base run's tables survive", {
  expect_true("FILE=xptab30_frem" %in% tableWords(fremModelLines("AGE"))[[1]])
  expect_true("FILE=xptab31" %in% tableWords(fremModelLines("AGE", fremModName = "run31"))[[1]])
})

test_that("the items left out of the table are named", {
  expect_message(fremModelLines("AGE", quiet = FALSE), "STUDYID")
})

test_that("a base model without $TABLE gives a FREM model without one", {
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  td <- withr::local_tempdir()
  file.copy(file.path(src, c("run30.mod", "run30.ext")), td)
  mod <- readLines(file.path(td, "run30.mod"))
  start <- grep("^\\$TAB", mod)
  nxt <- grep("^\\$", mod)
  end <- c(nxt[nxt > start], length(mod) + 1)[1] - 1
  writeLines(mod[-(start:end)], file.path(td, "run30.mod"))
  expect_length(tableWords(fremModelLines("AGE", modDir = td)), 0)
})

test_that("updateFREMmodel() prunes the table when it rewrites $INPUT", {
  ## run31's table lists STUDYID; with the default keep columns the new FREM
  ## data set does not carry it, and NM-TRAN would reject the table.
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  td <- withr::local_tempdir()
  file.copy(file.path(src, c("run31.mod", "run31.ext")), td)
  out <- suppressWarnings(updateFREMmodel(
    strFREMModel = file.path(td, "run31.mod"),
    strFREMData = file.path(src, "frem_dataset.dta"),
    strFFEMData = file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"),
    cstrContCovsToAdd = "HT",
    numNonFREMThetas = 7, numSkipOm = 2,
    bWriteData = FALSE, bWriteMod = FALSE, quiet = TRUE
  ))
  newInput <- strsplit(trimws(sub("^\\$INPUT", "", grep("^\\$INPUT", out$model, value = TRUE))), "\\s+")[[1]]
  w <- tableWords(out$model)[[1]]
  expect_false("STUDYID" %in% newInput)
  expect_false("STUDYID" %in% w)
  expect_true("FILE=xptab31" %in% w)
})

test_that(".fremCarryTables() drops BY and EXCLUDE_BY when their items go", {
  lines <- c(
    "$TABLE ID TIME STUDY DV BY STUDY NOPRINT FILE=t1",
    "$TABLE ID DV EXCLUDE_BY BLQ ONEHEADER FILE=t2"
  )
  out <- .fremCarryTables(lines, oldInput = c("ID", "TIME", "STUDY", "DV", "BLQ"), newInput = c("ID", "TIME", "DV"), quiet = TRUE)
  w <- tableWords(out)
  expect_equal(w[[1]], c("ID", "TIME", "DV", "FREMTYPE", "NOPRINT", "FILE=t1"))
  expect_equal(w[[2]], c("ID", "DV", "FREMTYPE", "ONEHEADER", "FILE=t2"))
})
