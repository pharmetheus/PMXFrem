# $INPUT label=synonym (T44). NONMEM lets either name be used and one of the
# two must be a reserved label (NONMEM help, $INPUT). The data column is named
# by the reserved one; the other stays available as `synonym`.

synTableWords <- function(lines) {
  starts <- grep("^\\s*\\$TAB", lines)
  lapply(starts, function(s) {
    nxt <- grep("^\\s*\\$", lines)
    end <- c(nxt[nxt > s], length(lines) + 1)[1] - 1
    w <- strsplit(trimws(gsub(";.*", "", paste(lines[s:end], collapse = " "))), "\\s+")[[1]]
    w[-1]
  })
}

test_that(".fremInputItems() names a column by the reserved one of label and synonym", {
  items <- .fremInputItems("$INPUT ID TIME CP=DV DV2=EVID X=Y DROPPED=DROP")
  expect_equal(items$name, c("ID", "TIME", "DV", "EVID", "X", "DROPPED"))
  expect_equal(items$synonym, c("", "", "CP", "DV2", "Y", ""))
  expect_equal(items$dropped, c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
})

test_that(".fremInputItems() keeps a reserved label and exposes the synonym", {
  items <- .fremInputItems("$INPUT ID DV=CP")
  expect_equal(items$name, c("ID", "DV"))
  expect_equal(items$synonym, c("", "CP"))
})

test_that(".fremInputItems() keeps the label when neither name is reserved", {
  items <- .fremInputItems("$INPUT ID X=Y")
  expect_equal(items$name, c("ID", "X"))
  expect_equal(items$synonym, c("", "Y"))
})

test_that(".fremInputItems() reads reserved labels case-insensitively", {
  expect_equal(.fremInputItems("$INPUT id cp=dv")$name, c("id", "dv"))
})

test_that("DROP and SKIP are still dropped, and are not synonyms", {
  items <- .fremInputItems("$INPUT ID A=DROP B=SKIP DROP SKIP=C")
  expect_equal(items$dropped, c(FALSE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(items$synonym[items$dropped], rep("", 4))
})

test_that(".fremCarryTables() reads a table token for either name as that item", {
  lines <- c("$TABLE ID CP WT NOPRINT FILE=t1")
  syn <- c(CP = "DV")
  ## The item is carried: CP survives, as the name the new $INPUT has.
  out <- .fremCarryTables(lines,
    oldInput = c("ID", "DV", "WT"), newInput = c("ID", "DV", "WT"),
    oldSynonyms = syn, quiet = TRUE
  )
  expect_equal(synTableWords(out)[[1]], c("ID", "DV", "WT", "FREMTYPE", "NOPRINT", "FILE=t1"))
  ## The item is not carried: CP goes with it.
  out <- .fremCarryTables(lines,
    oldInput = c("ID", "DV", "WT"), newInput = c("ID", "WT"),
    oldSynonyms = syn, quiet = TRUE
  )
  expect_equal(synTableWords(out)[[1]], c("ID", "WT", "FREMTYPE", "NOPRINT", "FILE=t1"))
})

test_that(".fremCarryTables() does not list an item twice when both its names are in the table", {
  out <- .fremCarryTables("$TABLE ID CP DV NOPRINT",
    oldInput = c("ID", "DV"), newInput = c("ID", "DV"),
    oldSynonyms = c(CP = "DV"), quiet = TRUE
  )
  expect_equal(synTableWords(out)[[1]], c("ID", "DV", "FREMTYPE", "NOPRINT"))
})

## A copy of run30 whose $INPUT reads `DV` as `CP=DV`, and whose $TABLE lists CP.
synonymCase <- function(td, item = "DV", syn = "CP") {
  input <- paste0(syn, "=", item)
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  file.copy(file.path(src, c("run30.mod", "run30.ext")), td)
  mod <- readLines(file.path(td, "run30.mod"))
  s <- grep("^\\$INPUT", mod)
  mod[s] <- sub(paste0(" ", item, " "), paste0(" ", input, " "), mod[s])
  expect_match(mod[s], input, fixed = TRUE)
  t <- grep("^\\$TABLE", mod)
  mod[t] <- sub(paste0(" ", item, " "), paste0(" ", syn, " "), mod[t])
  expect_match(mod[t], paste0(" ", syn, " "), fixed = TRUE)
  writeLines(mod, file.path(td, "run30.mod"))
  file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")
}

test_that("prepareAndValidateData() names the column DV for CP=DV, and takes either name", {
  td <- withr::local_tempdir()
  model <- file.path(td, "model.mod")
  writeLines(c("$PROBLEM test", "$INPUT ID CP=DV WT", "$DATA data.csv"), model)
  dat <- data.frame(A = 1:2, B = c(5, 6), C = c(70, 80))
  res <- prepareAndValidateData(dat, model, covariates = "WT", quiet = TRUE)
  expect_equal(names(res$validatedData), c("ID", "DV", "WT"))
  expect_equal(res$inputNames, c("ID", "DV", "WT"))
  ## The synonym is accepted wherever the column name is.
  expect_no_error(prepareAndValidateData(dat, model,
    covariates = c("WT", "CP"), cstrKeepCols = c("ID", "CP"), quiet = TRUE
  ))
})

test_that("createFREMmodel() finds DV when $INPUT says CP=DV", {
  td <- withr::local_tempdir()
  dataFile <- synonymCase(td)
  res <- suppressWarnings(createFREMmodel(
    modName = "run30", modDevDir = td, ffemDataFile = dataFile,
    covariates = c("AGE", "WT"), numSkipOm = 2,
    cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "RATE", "FOOD", "CP"),
    outputDir = td, fremModName = "frem", quiet = TRUE
  ))
  fremData <- read.csv(res$data)
  expect_true("DV" %in% names(fremData))
  expect_false("CP" %in% names(fremData))
  model <- readLines(res$model)
  fremInput <- strsplit(trimws(sub("^\\$INPUT", "", grep("^\\$INPUT", model, value = TRUE))), "\\s+")[[1]]
  expect_true("DV" %in% fremInput)
  ## CP in the base $TABLE was the DV item, which the FREM data carries.
  w <- synTableWords(model)[[1]]
  expect_true("DV" %in% w)
  expect_false("CP" %in% w)
})

test_that("createFREMmodel() prunes a table synonym whose item the FREM data lacks", {
  td <- withr::local_tempdir()
  dataFile <- synonymCase(td, item = "RATE", syn = "RT")
  res <- suppressWarnings(createFREMmodel(
    modName = "run30", modDevDir = td, ffemDataFile = dataFile,
    covariates = c("AGE", "WT"), numSkipOm = 2,
    cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "FOOD", "DV"),
    outputDir = td, fremModName = "frem", quiet = TRUE
  ))
  expect_false("RATE" %in% names(read.csv(res$data)))
  expect_false("RT" %in% synTableWords(readLines(res$model))[[1]])
})

test_that("an IGNORE condition may use either name of a label=synonym item", {
  ## NONMEM accepts either name; the column is named DV here, but the base
  ## model's IGNORE refers to CP.
  td <- withr::local_tempdir()
  write.csv(data.frame(A = c(1, 1, 2), B = c(0, 1, 1), C = c(0, 5, 6)),
    file.path(td, "data.csv"),
    row.names = FALSE, quote = FALSE
  )
  writeLines(
    c("$PROBLEM test", "$INPUT ID TIME CP=DV", "$DATA data.csv IGNORE=(CP.EQN.0)"),
    file.path(td, "model.mod")
  )
  res <- prepareAndValidateData(file.path(td, "data.csv"), file.path(td, "model.mod"),
    covariates = NULL, quiet = TRUE, keepDoseOnlySubjects = TRUE
  )
  expect_equal(names(res$validatedData), c("ID", "TIME", "DV"))
  expect_equal(res$validatedData$DV, c(5, 6))
})
