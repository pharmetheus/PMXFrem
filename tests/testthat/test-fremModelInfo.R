# Tests for fremModelInfo() and the auto-derivation of numNonFREMThetas / numSkipOm
# in getExplainedVar() and getForestDFFREM().
#
# Written before the implementation (TDD): every block below fails until
# fremModelInfo() exists and the two entry points accept NULL for the two counts
# (and getForestDFFREM() gains an optional modFile). The existing explicit-argument
# tests in the suite must keep passing (backwards compatibility).

fremFix <- function(runno = "run31") {
  list(
    mod = system.file(sprintf("extdata/SimNeb/%s.mod", runno), package = "PMXFrem"),
    ext = system.file(sprintf("extdata/SimNeb/%s.ext", runno), package = "PMXFrem")
  )
}

# ---------------------------------------------------------------------------
# fremModelInfo(): pure derivation
# ---------------------------------------------------------------------------

test_that("fremModelInfo derives the FREM structure from model + ext (run31)", {
  f <- fremFix("run31")
  de <- getExt(extFile = f$ext)

  info <- fremModelInfo(modFile = f$mod, dfext = de)

  expect_type(info, "list")
  expect_equal(info$numFREMThetas, 18)
  expect_equal(info$numNonFREMThetas, 7)
  expect_equal(info$numParCov, 3)
  expect_equal(info$numSkipOm, 2)
  expect_equal(info$numSigmas, 3)
  expect_equal(info$numTotEta, 23)
  expect_setequal(info$covNames, getCovNames(f$mod)$covNames)
})

test_that("fremModelInfo derives the FREM structure for a second model (run22-3)", {
  mod <- system.file("extdata/SimVal/run22-3.mod", package = "PMXForest")
  ext <- system.file("extdata/SimVal/run22-3.ext", package = "PMXForest")
  skip_if(mod == "" || ext == "", "PMXForest SimVal fixtures not installed")

  info <- fremModelInfo(modFile = mod, dfext = getExt(extFile = ext))

  expect_equal(info$numNonFREMThetas, 13)
  expect_equal(info$numSkipOm, 2)
  expect_equal(info$numParCov, 4)
  expect_equal(info$numFREMThetas, 11)
})

test_that("fremModelInfo accepts an ext file path as well as a data.frame", {
  f <- fremFix("run31")
  a <- fremModelInfo(modFile = f$mod, dfext = getExt(extFile = f$ext))
  b <- fremModelInfo(modFile = f$mod, dfext = f$ext)
  keys <- c("numNonFREMThetas", "numSkipOm", "numParCov", "numFREMThetas", "numSigmas")
  expect_equal(a[keys], b[keys])
})

test_that("fremModelInfo accepts a getSamples()-shaped frame (no ITERATION/OBJ)", {
  f <- fremFix("run31")
  de <- getExt(extFile = f$ext)
  samples_like <- de[, setdiff(names(de), c("ITERATION", "OBJ")), drop = FALSE]

  info <- fremModelInfo(modFile = f$mod, dfext = samples_like)
  expect_equal(info$numNonFREMThetas, 7)
  expect_equal(info$numSkipOm, 2)
})

# ---------------------------------------------------------------------------
# fremModelInfo(): overrides + validation (warn, keep the explicit value)
# ---------------------------------------------------------------------------

test_that("fremModelInfo warns and keeps the explicit numNonFREMThetas on mismatch", {
  f <- fremFix("run31")
  de <- getExt(extFile = f$ext)

  expect_warning(
    info <- fremModelInfo(modFile = f$mod, dfext = de, numNonFREMThetas = 8),
    "numNonFREMThetas"
  )
  expect_equal(info$numNonFREMThetas, 8) # explicit value wins
})

test_that("fremModelInfo warns and keeps the explicit numSkipOm on mismatch", {
  f <- fremFix("run31")
  de <- getExt(extFile = f$ext)

  expect_warning(
    info <- fremModelInfo(modFile = f$mod, dfext = de, numSkipOm = 1),
    "numSkipOm"
  )
  expect_equal(info$numSkipOm, 1)
})

test_that("fremModelInfo is silent when explicit values match the derived ones", {
  f <- fremFix("run31")
  de <- getExt(extFile = f$ext)
  expect_silent(
    fremModelInfo(modFile = f$mod, dfext = de, numNonFREMThetas = 7, numSkipOm = 2)
  )
})

test_that("fremModelInfo errors on a non-FREM model", {
  base <- system.file("extdata/SimNeb/run30.mod", package = "PMXFrem")
  ext <- system.file("extdata/SimNeb/run30.ext", package = "PMXFrem")
  expect_error(fremModelInfo(modFile = base, dfext = getExt(extFile = ext)))
})

test_that("fremModelInfo rejects a dfext that is neither a data.frame nor a path", {
  f <- fremFix("run31")
  expect_error(
    fremModelInfo(modFile = f$mod, dfext = 42),
    "must be a data.frame"
  )
})

test_that("fremModelInfo errors when dfext has no THETA / OMEGA columns", {
  f <- fremFix("run31")
  expect_error(
    fremModelInfo(modFile = f$mod, dfext = data.frame(x = 1, y = 2)),
    "Could not find THETA / OMEGA columns"
  )
})

test_that("fremModelInfo errors when the OMEGA column count is not triangular", {
  f <- fremFix("run31")
  de <- getExt(extFile = f$ext)
  # drop a single OMEGA column so the remaining count is no longer k(k+1)/2
  omCols <- grep("OMEGA", names(de), value = TRUE)
  de2 <- de[, setdiff(names(de), omCols[1]), drop = FALSE]
  expect_error(
    fremModelInfo(modFile = f$mod, dfext = de2),
    "not a triangular number"
  )
})

test_that("fremModelInfo errors when the model has no explicit $OMEGA BLOCK(N)", {
  f <- fremFix("run31")
  de <- getExt(extFile = f$ext)
  td <- withr::local_tempdir()
  mod <- readLines(f$mod)
  mod <- sub("\\$OMEGA\\s+BLOCK\\(21\\)", "$OMEGA ; block removed", mod)
  noBlock <- file.path(td, "noblock.mod")
  writeLines(mod, noBlock)
  expect_error(
    fremModelInfo(modFile = noBlock, dfext = de),
    "Could not find a '\\$OMEGA BLOCK\\(N\\)' record"
  )
})

# ---------------------------------------------------------------------------
# getExplainedVar(): derive when the counts are omitted
# ---------------------------------------------------------------------------

test_that("getExplainedVar derives numNonFREMThetas / numSkipOm when omitted", {
  modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
  fremRunno <- 31
  modFile <- file.path(modDevDir, paste0("run", fremRunno, ".mod"))

  dfCovs <- setupDfCovsEV(modFile)
  cstrCovariates <- c("All", names(dfCovs))
  functionList2 <- list(
    function(basethetas, covthetas, dfrow, etas, ...) basethetas[2] * exp(covthetas[1] + etas[3]),
    function(basethetas, covthetas, dfrow, etas, ...) basethetas[3] * exp(covthetas[2] + etas[4])
  )

  common <- list(
    type = 0, data = NULL, dfCovs = dfCovs,
    functionList = functionList2, functionListName = c("CL", "V"),
    cstrCovariates = cstrCovariates, modDevDir = modDevDir, runno = fremRunno,
    ncores = 1, quiet = TRUE, seed = 123
  )

  explicit <- do.call(getExplainedVar, c(common, list(numNonFREMThetas = 7, numSkipOm = 2)))
  derived <- do.call(getExplainedVar, common) # both counts omitted

  expect_equal(as.data.frame(derived), as.data.frame(explicit))
})

test_that("getExplainedVar still accepts explicit counts without warning", {
  modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
  fremRunno <- 31
  modFile <- file.path(modDevDir, paste0("run", fremRunno, ".mod"))
  dfCovs <- setupDfCovsEV(modFile)

  expect_no_warning(
    getExplainedVar(
      type = 0, data = NULL, dfCovs = dfCovs,
      numNonFREMThetas = 7, numSkipOm = 2,
      functionList = list(function(basethetas, covthetas, dfrow, etas, ...) {
        basethetas[2] * exp(covthetas[1] + etas[3])
      }),
      functionListName = "CL",
      cstrCovariates = c("All", names(dfCovs)),
      modDevDir = modDevDir, runno = fremRunno,
      ncores = 1, quiet = TRUE, seed = 123
    )
  )
})

# ---------------------------------------------------------------------------
# getForestDFFREM(): runno / modName + derive when the counts are omitted
# ---------------------------------------------------------------------------

test_that("getForestDFFREM derives covNames / counts from runno / modName when omitted", {
  extFile <- system.file("extdata/SimVal/run22-3.ext", package = "PMXForest")
  covFile <- system.file("extdata/SimVal/run22-3.cov", package = "PMXForest")
  modFile <- system.file("extdata/SimVal/run22-3.mod", package = "PMXForest")
  datFile <- system.file("extdata/SimVal/DAT-1-MI-PMX-2.csv", package = "PMXForest")
  skip_if(
    any(c(extFile, covFile, modFile, datFile) == ""),
    "PMXForest SimVal fixtures not installed"
  )

  covNames <- getCovNames(modFile = modFile)
  dfData <- read.csv(datFile)
  dfCovs <- PMXForest::createInputForestData(
    PMXForest::getCovStats(dfData, covNames$orgCovNames, probs = c(0.05, 0.95))
  )
  paramFun <- function(basethetas, covthetas, dfrow, ...) {
    c(
      basethetas[1] * exp(covthetas[1]),
      basethetas[2] * exp(covthetas[2]),
      5 / (basethetas[1] * exp(covthetas[1]))
    )
  }

  set.seed(123)
  dfSamplesCOV <- PMXForest::getSamples(covFile, extFile = extFile, n = 25)

  common <- list(
    dfCovs = dfCovs, functionList = list(paramFun),
    functionListName = c("CL", "V", "AUC"),
    dfParameters = dfSamplesCOV, probs = c(0.05, 0.95), dfRefRow = NULL,
    quiet = TRUE, ncores = 1, cstrPackages = c("PMXFrem", "dplyr")
  )

  explicit <- suppressWarnings(do.call(getForestDFFREM, c(common, list(
    covNames = covNames$covNames, numNonFREMThetas = 13, numSkipOm = 2
  ))))
  derived <- suppressWarnings(do.call(getForestDFFREM, c(common, list(
    modName = "run22-3", modDevDir = dirname(modFile) # covNames + counts derived
  ))))

  expect_equal(as.data.frame(derived), as.data.frame(explicit))
})

# ===========================================================================
# T2 rollout: fremParameterTable / createFFEMmodel / createFFEMdata / calcEtas
# derive numNonFREMThetas / numSkipOm when omitted (identical result to the
# explicit call). These fail until the rollout is implemented.
# ===========================================================================

.simNeb <- function() system.file("extdata/SimNeb/", package = "PMXFrem")

.ffemInputData <- function() {
  utils::read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")) %>% dplyr::filter(BLQ != 1)
}

test_that("fremParameterTable derives the counts when they are omitted", {
  common <- list(
    runno = 31, modDevDir = .simNeb(),
    thetaNum = 1:7, omegaNum = 1:5, sigmaNum = 1:2,
    availCov = "all", quiet = TRUE
  )
  set.seed(1)
  explicit <- do.call(
    fremParameterTable,
    c(common, list(numNonFREMThetas = 7, numSkipOm = 2))
  )
  set.seed(1)
  derived <- do.call(fremParameterTable, common)
  expect_equal(derived, explicit)
})

test_that("createFFEMdata derives the counts when they are omitted", {
  data <- .ffemInputData()
  common <- list(
    modName = "run31", modDevDir = .simNeb(),
    parNames = c("CL", "V", "MAT"),
    dataFile = data, newDataFile = NULL, quiet = TRUE
  )
  explicit <- do.call(createFFEMdata, c(common, list(numNonFREMThetas = 7, numSkipOm = 2)))
  derived <- do.call(createFFEMdata, common)
  expect_equal(derived$newData, explicit$newData)
  expect_equal(derived$Omega, explicit$Omega)
  expect_equal(derived$Coefficients, explicit$Coefficients)
})

test_that("createFFEMmodel derives the counts when they are omitted", {
  td <- withr::local_tempdir()
  common <- list(
    runno = 31, modDevDir = system.file("extdata/SimNeb", package = "PMXFrem"),
    parNames = c("CL", "V", "MAT"),
    dataFile = system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"),
    newDataFile = file.path(td, "ffemdata.csv"),
    baserunno = 30, quiet = TRUE
  )
  explicit <- do.call(createFFEMmodel, c(common, list(numNonFREMThetas = 7, numSkipOm = 2)))
  derived <- do.call(createFFEMmodel, common)
  expect_equal(derived, explicit)
})

test_that("calcEtas derives the counts when they are omitted", {
  data <- .ffemInputData()
  common <- list(
    modName = "run31", modDevDir = .simNeb(),
    parNames = c("CL", "V", "MAT"), dataFile = data
  )
  explicit <- do.call(calcEtas, c(common, list(numNonFREMThetas = 7, numSkipOm = 2)))
  derived <- do.call(calcEtas, common)
  expect_equal(derived, explicit)
})

# ---------------------------------------------------------------------------
# Reading the FREM $OMEGA block from control streams that are not run31 as-is
# ---------------------------------------------------------------------------

.fmiMod <- function(td, edit, name = "m.mod") {
  l <- readLines(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), warn = FALSE)
  l <- edit(l)
  f <- file.path(td, name)
  writeLines(l, f)
  f
}
.fmiExt <- function() getExt(extFile = system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"))
.afterBlock <- function(l) {
  # index of the last line of the FREM BLOCK(21) record
  s <- grep("^\\$OMEGA\\s+BLOCK\\(21\\)", l)
  nxt <- grep("^\\s*\\$", l)
  nxt[nxt > s][1] - 1L
}

test_that("fremModelInfo is not misled by an $OMEGA record after the FREM block", {
  # numSkipOm was numTotEta - blockN: every eta outside the block counted as
  # skipped, so an IIV added after the block made numSkipOm 3 instead of 2.
  td <- withr::local_tempdir()
  m <- .fmiMod(td, function(l) {
    i <- .afterBlock(l)
    l <- append(l, "$OMEGA  0.1 ; 24. IIV on KA", after = i)
    l[grep("^KA\\s*=", l)] <- "KA    = 1 / (MAT-D1) * EXP(ETA(24))"
    l
  })
  ext <- .fmiExt()
  for (j in 1:24) ext[[sprintf("OMEGA.24.%d.", j)]] <- if (j == 24) 0.1 else 0
  info <- fremModelInfo(modFile = m, dfext = ext)
  expect_equal(info$numSkipOm, 2)
  expect_equal(info$numParCov, 3)
})

test_that("fremModelInfo ignores BLOCK(n) written in a comment", {
  td <- withr::local_tempdir()
  m <- .fmiMod(td, function(l) {
    i <- grep("BSV_SMOK", l, fixed = TRUE)
    stopifnot(length(i) == 1L) # the edit has to apply, or the test checks nothing
    l[i] <- paste(l[i], "(was BLOCK(22) in run30)")
    l
  })
  info <- fremModelInfo(modFile = m, dfext = .fmiExt())
  expect_equal(info$numSkipOm, 2)
  expect_equal(info$numParCov, 3)
})

test_that("fremModelInfo ignores prior records after the FREM block", {
  td <- withr::local_tempdir()
  m <- .fmiMod(td, function(l) append(l, c("$OMEGAPD BLOCK(2) FIX", "0.1 0.01 0.1"), after = .afterBlock(l)))
  info <- fremModelInfo(modFile = m, dfext = .fmiExt())
  expect_equal(info$numSkipOm, 2)
  expect_equal(info$numParCov, 3)
})

test_that("fremModelInfo reads an abbreviated or lower-case $OMEGA record", {
  td <- withr::local_tempdir()
  for (spelling in c("$OME", "$omega")) {
    m <- .fmiMod(td, function(l) {
      i <- grep("^\\$OMEGA\\s+BLOCK\\(21\\)", l)
      l[i] <- sub("^\\$OMEGA", spelling, l[i], fixed = FALSE)
      l
    }, name = paste0("sp", nchar(spelling), ".mod"))
    info <- fremModelInfo(modFile = m, dfext = .fmiExt())
    expect_equal(info$numSkipOm, 2, info = spelling)
    expect_equal(info$numParCov, 3, info = spelling)
  }
})

test_that("fremModelInfo does not call an unused trailing eta a stale ext", {
  # The theta check warns only when the model references MORE than the ext
  # has; the eta check used !=, so an ext with an eta the code never references
  # was reported as coming from a different run.
  ext <- .fmiExt()
  for (j in 1:24) ext[[sprintf("OMEGA.24.%d.", j)]] <- 0
  w <- capture_warnings(fremModelInfo(
    modFile = system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), dfext = ext
  ))
  expect_false(any(grepl("not from the same run", w)))
})

test_that("fremModelInfo warns when an overridden numSkipOm leaves no FREM parameters", {
  # (the "supplied numSkipOm differs" warning fires too, so capture both)
  w <- capture_warnings(fremModelInfo(
    modFile = system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"),
    dfext = .fmiExt(), numSkipOm = 5
  ))
  expect_true(any(grepl("numSkipOm = 5 leaves numParCov = 0", w)))
})

test_that("fremModelInfo warns when the FREM block is larger than the ext's omega matrix", {
  td <- withr::local_tempdir()
  file.copy(system.file("extdata/SimNeb", c("run31.mod", "run31.ext"), package = "PMXFrem"), td)
  mod <- file.path(td, "run31.mod")
  L <- readLines(mod)
  h <- grep("OMEGA *BLOCK\\(21\\)", L)
  stopifnot(length(h) == 1)
  L[h] <- sub("BLOCK(21)", "BLOCK(99)", L[h], fixed = TRUE)
  writeLines(L, mod)
  expect_warning(
    fremModelInfo(mod, file.path(td, "run31.ext")),
    "define 101 eta\\(s\\) up to the end of the FREM block and the ext describes 23"
  )
})
