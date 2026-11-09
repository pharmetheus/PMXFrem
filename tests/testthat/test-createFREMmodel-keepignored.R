# createFREMmodel(keepIgnoredRecords = TRUE) (PRRS-322): the FREM data set
# keeps every record of the original data set, and the $DATA record of the
# FREM model re-applies what was left out - the conditions of the base model
# as they are, and a proxy column for the rest - so that NONMEM sees exactly
# the records it sees with the default data set.

## A copy of run30 in a temporary directory, with its $DATA and $INPUT
## records replaced when given.
run30Copy <- function(dataLines = NULL, inputLines = NULL, env = parent.frame()) {
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  td <- withr::local_tempdir(.local_envir = env)
  file.copy(file.path(src, c("run30.mod", "run30.ext", "run30.lst", "run30.phi")), td)
  mod <- readLines(file.path(td, "run30.mod"))
  swap <- function(mod, record, lines) {
    if (is.null(lines)) {
      return(mod)
    }
    start <- grep(paste0("^\\", record), mod)
    nxt <- grep("^\\$", mod)
    end <- nxt[nxt > start][1] - 1
    c(mod[seq_len(start - 1)], lines, mod[(end + 1):length(mod)])
  }
  mod <- swap(mod, "$DATA", dataLines)
  mod <- swap(mod, "$INPUT", inputLines)
  writeLines(mod, file.path(td, "run30.mod"))
  td
}

## createFREMmodel() on a base model directory, with and without the option.
fremBoth <- function(baseDir, dataFile, covariates = c("AGE", "WT"), ...) {
  run <- function(keep) {
    out <- withr::local_tempdir(.local_envir = parent.frame(2))
    suppressWarnings(createFREMmodel(
      modName = "run30", modDevDir = baseDir, dataFile = dataFile,
      covariates = covariates, numSkipOm = 2,
      cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "RATE", "FOOD", "DV"),
      outputDir = out, fremModName = "frem", quiet = TRUE,
      keepIgnoredRecords = keep, ...
    ))
  }
  list(default = run(FALSE), kept = run(TRUE))
}

## The records NONMEM reads from a model and its data set: the IGNORE /
## ACCEPT lists applied, the =DROP columns removed.
seenByNonmem <- function(modelPath, dataPath = NULL, modelLines = NULL) {
  mod <- if (is.null(modelLines)) readLines(modelPath) else modelLines
  if (is.null(dataPath)) dataPath <- modelPath
  items <- PMXFrem:::.fremInputItems(findrecord(mod, record = "^\\$INP(U(T)?)?\\s", quiet = TRUE))
  num <- utils::read.csv(dataPath)
  raw <- PMXFrem:::.readDataAsText(dataPath)
  names(num) <- names(raw) <- items$name
  mf <- withr::local_tempfile(fileext = ".mod")
  writeLines(mod, mf)
  keep <- PMXFrem:::.fremFilterKeep(mf, num, quiet = TRUE, rawData = raw)
  out <- num[keep, !items$dropped, drop = FALSE]
  rownames(out) <- NULL
  out
}

defaultData <- function(res) {
  d <- utils::read.csv(res$data)
  rownames(d) <- NULL
  d
}

src <- system.file("extdata", "SimNeb", package = "PMXFrem")
origData <- file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")

test_that("NONMEM sees the default records: run30 and its IGNORE(BLQ.EQN.1)", {
  res <- fremBoth(run30Copy(), origData)
  kept <- utils::read.csv(res$kept$data)
  expect_gt(nrow(kept), nrow(defaultData(res$default)))
  expect_equal(seenByNonmem(res$kept$model, res$kept$data), defaultData(res$default))

  ## The condition of the base model is carried as it is, and BLQ is in the
  ## data set for it, dropped in $INPUT.
  mod <- readLines(res$kept$model)
  expect_true(any(grepl("IGNORE(BLQ.EQN.1)", mod, fixed = TRUE)))
  expect_true(any(grepl("\\bBLQ=DROP\\b", mod)))
})

test_that("removing the condition of the base model brings back exactly the records it left out", {
  res <- fremBoth(run30Copy(), origData)
  mod <- readLines(res$kept$model)
  mod <- sub("IGNORE(BLQ.EQN.1)", "", mod, fixed = TRUE)
  seen <- seenByNonmem(res$kept$model, res$kept$data, modelLines = mod)
  default <- defaultData(res$default)

  orig <- utils::read.csv(origData)
  back <- orig[orig$BLQ == 1 & orig$ID %in% default$ID & (orig$LNDV != -99 | orig$EVID != 0), ]
  expect_gt(nrow(back), 0)
  expect_equal(sum(seen$FREMTYPE == 0), sum(default$FREMTYPE == 0) + nrow(back))
  expect_equal(seen[seen$FREMTYPE > 0, ], default[default$FREMTYPE > 0, ], ignore_attr = TRUE)
})

test_that("a subject the base model excludes altogether is excluded, and has covariate records", {
  base <- run30Copy(dataLines = c(
    "$DATA      ../data.csv IGNORE=@",
    "           IGNORE(BLQ.EQN.1,AGE.GT.60)"
  ))
  res <- fremBoth(base, origData)
  expect_equal(seenByNonmem(res$kept$model, res$kept$data), defaultData(res$default))

  kept <- utils::read.csv(res$kept$data)
  orig <- utils::read.csv(origData)
  old <- unique(orig$ID[orig$AGE > 60])
  expect_gt(length(old), 0)
  expect_false(any(defaultData(res$default)$ID %in% old))
  expect_true(all(old %in% kept$ID[kept$FREMTYPE > 0]))
})

test_that("an ACCEPT list of the base model becomes part of the proxy", {
  base <- run30Copy(dataLines = c(
    "$DATA      ../data.csv IGNORE=@",
    "           ACCEPT(BLQ.EQN.0)"
  ))
  res <- fremBoth(base, origData)
  expect_equal(seenByNonmem(res$kept$model, res$kept$data), defaultData(res$default))
  ## NONMEM allows no ACCEPT list next to an IGNORE list.
  dataRec <- findrecord(readLines(res$kept$model), record = "\\$DATA", quiet = TRUE)
  expect_false(any(grepl("ACCEPT", sub(";.*", "", dataRec))))
  expect_true(any(grepl("ACCEPT(BLQ.EQN.0)", dataRec, fixed = TRUE)))
})

test_that("a condition on a dropped column, and subjects with only dose records", {
  ## DAY is dropped; one subject keeps only its dose records.
  base <- run30Copy(
    dataLines = c("$DATA      ../data.csv IGNORE=@", "           IGNORE(BLQ.EQN.1,DAY.GT.20)"),
    inputLines = c(
      "$INPUT      NO ID STUDYID TAD TIME DAY=DROP AMT RATE ODV DV EVID BLQ DOSE",
      "            FOOD FORM TYPE WT HT LBWT BSA SEX RACE AGE AST ALT BILI",
      "            CRCL BMI NCI GENO2 ETHNIC SMOK RACEL NCIL"
    )
  )
  orig <- utils::read.csv(origData)
  doseOnly <- orig$ID[1]
  orig <- orig[!(orig$ID == doseOnly & orig$EVID == 0), ]
  dataFile <- file.path(withr::local_tempdir(), "data.csv")
  utils::write.csv(orig, dataFile, row.names = FALSE, quote = FALSE)

  res <- fremBoth(base, dataFile)
  expect_equal(seenByNonmem(res$kept$model, res$kept$data), defaultData(res$default))
  expect_false(doseOnly %in% defaultData(res$default)$ID)

  kept <- utils::read.csv(res$kept$data)
  expect_true(doseOnly %in% kept$ID)
  expect_true(all(kept$FREMEXCL[kept$ID == doseOnly] == 1))
  expect_true(any(grepl("IGNORE=(FREMEXCL.EQN.1)", readLines(res$kept$model), fixed = TRUE)))
})
