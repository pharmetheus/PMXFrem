## The FFEM data set keeps the subjects of the FREM fit (PRRS-335).
##
## The bundled FREM model run31 was fitted on all 760 subjects of the data,
## 30 of them without an observation that IGNORE(BLQ.EQN.1) keeps. A FREM
## model from createFREMmodel() leaves those 30 out, so its .phi file has 730
## subjects. The fixture copies run31 with the 30 removed from its .phi file.

simNeb <- system.file("extdata", "SimNeb", package = "PMXFrem")
simNebData <- file.path(simNeb, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")

## The subjects without an observation that the base model keeps.
doseOnlyIds <- function() {
  d <- utils::read.csv(simNebData)
  setdiff(unique(d$ID), unique(d$ID[d$EVID == 0 & d$BLQ != 1]))
}

## A copy of the FREM model run31, and the base model run30, in a temporary
## folder, with `dropIds` removed from every table of the FREM .phi file.
fremFitWithout <- function(dropIds) {
  td <- tempfile("frem-fit-")
  dir.create(td)
  for (f in list.files(simNeb, pattern = "^run3[01]\\.")) file.copy(file.path(simNeb, f), td)
  phi <- readLines(file.path(td, "run31.phi"))
  fields <- strsplit(trimws(phi), "\\s+")
  isRow <- vapply(fields, function(x) length(x) > 1 && !is.na(suppressWarnings(as.numeric(x[1]))), TRUE)
  rowId <- vapply(fields, function(x) if (length(x) > 1) x[2] else "", "")
  writeLines(phi[!(isRow & suppressWarnings(as.numeric(rowId)) %in% dropIds)], file.path(td, "run31.phi"))
  td
}

ffemSubjects <- function(dir, ...) {
  res <- createFFEMdata(
    modName = "run31", modDevDir = dir, parNames = c("CL", "V", "MAT"),
    numNonFREMThetas = 7, numSkipOm = 2, dataFile = simNebData,
    newDataFile = NULL, quiet = TRUE, ...
  )
  res$newData
}

test_that("the FFEM data set has the subjects of the FREM fit", {
  drop <- doseOnlyIds()
  expect_length(drop, 30)
  dir <- fremFitWithout(drop)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  ffem <- ffemSubjects(dir)
  phiIds <- getPhi(file.path(dir, "run31.phi"), warn = FALSE)$ID
  expect_setequal(unique(ffem$ID), phiIds)
  expect_length(unique(ffem$ID), 730)
})

test_that("keepDoseOnlySubjects = TRUE keeps every subject of the data", {
  dir <- fremFitWithout(doseOnlyIds())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  all <- ffemSubjects(dir, keepDoseOnlySubjects = TRUE)
  expect_length(unique(all$ID), 760)
  expect_identical(nrow(all), nrow(utils::read.csv(simNebData)))
})

test_that("the subjects kept have the same records and covariate effects as before", {
  drop <- doseOnlyIds()
  dir <- fremFitWithout(drop)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  kept <- ffemSubjects(dir)
  all <- ffemSubjects(dir, keepDoseOnlySubjects = TRUE)
  expected <- all[!all$ID %in% drop, , drop = FALSE]
  rownames(expected) <- NULL
  rownames(kept) <- NULL
  expect_equal(kept, expected)
})

test_that("a FREM model with all subjects in its fit loses none", {
  ffem <- ffemSubjects(simNeb)
  expect_length(unique(ffem$ID), 760)
})

test_that("without a .phi file every subject is kept, with a warning", {
  dir <- fremFitWithout(numeric(0))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  unlink(file.path(dir, "run31.phi"))

  expect_warning(ffem <- ffemSubjects(dir), "phi")
  expect_length(unique(ffem$ID), 760)
})

test_that("createFFEMmodel() writes the FFEM data set with the subjects of the FREM fit", {
  drop <- doseOnlyIds()
  dir <- fremFitWithout(drop)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  ffemSubjectsFromModel <- function(...) {
    out <- tempfile("ffem-", tmpdir = dir, fileext = ".csv")
    suppressMessages(createFFEMmodel(
      modName = "run31", modDevDir = dir, baseModName = "run30",
      parNames = c("CL", "V", "MAT"), numNonFREMThetas = 7, numSkipOm = 2,
      dataFile = simNebData, newDataFile = out, ffemModName = NULL,
      quiet = TRUE, ...
    ))
    unique(utils::read.csv(out)$ID)
  }

  written <- ffemSubjectsFromModel()
  expect_length(written, 730)
  expect_false(any(drop %in% written))
  ## The argument reaches createFFEMdata().
  expect_length(ffemSubjectsFromModel(keepDoseOnlySubjects = TRUE), 760)
})
