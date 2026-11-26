test_that("getForestDFFREM works", {
  runno <- "22-3"

  extFile <- system.file("extdata", paste0("SimVal/run", runno, ".ext"), package = "PMXForest")
  covFile <- system.file("extdata", paste0("SimVal/run", runno, ".cov"), package = "PMXForest")
  modFile <- system.file("extdata", paste0("SimVal/run", runno, ".mod"), package = "PMXForest")
  datFile <- system.file("extdata", paste0("SimVal/DAT-1-MI-PMX-2.csv"), package = "PMXForest")

  dfData <- read.csv(datFile)

  covNames <- getCovNames(modFile = modFile)

  dfCovs <- PMXForest::createInputForestData(PMXForest::getCovStats(dfData, covNames$orgCovNames, probs = c(0.05, 0.95)))

  paramFun <- function(basethetas, covthetas, dfrow, ...) {
    CL <- basethetas[1] * exp(covthetas[1])
    V <- basethetas[2] * exp(covthetas[2])
    AUC <- 5 / CL

    return(c(CL, V, AUC))
  }

  functionListName2 <- c("CL", "V", "AUC")

  set.seed(123)
  dfSamplesCOV <- PMXForest::getSamples(covFile, extFile = extFile, n = 25)

  # Suppress warnings which are a known issue in the source code
  dfresFREM <- suppressWarnings(getForestDFFREM(
    dfCovs           = dfCovs,
    covNames         = covNames$covNames,
    functionList     = list(paramFun),
    functionListName = functionListName2,
    numNonFREMThetas = 13,
    numSkipOm        = 2,
    dfParameters     = dfSamplesCOV,
    probs            = c(0.05, 0.95),
    dfRefRow         = NULL,
    quiet            = TRUE,
    ncores           = 1,
    cstrPackages     = c("PMXFrem", "dplyr")
  ))

  ## Snapshot only what is reproducible off this machine; the sampled
  ## columns go through eigen() and are LAPACK-dependent, so they are
  ## checked for shape instead.
  expect_snapshot_value(
    stabilizeRows(dfresFREM[, setdiff(names(dfresFREM), forestSampledCols)]),
    style = "serialize",
    cran = TRUE
  )
  # 66 rows: one per covariate level in dfCovs, per returned parameter
  expect_forest_sampling_sane(dfresFREM, rows = 66)

  covlabels <- c(
    "Age 25 y", "Age 61 y", "ALT 14 IU", "ALT 43 IU", "AST 15 IU", "AST 34 IU",
    "Bilirubin 5 µmol/L", "Bilirubin 15 µmol/L", "BMI 23 kg/m^2", "BMI 39 kg/m^2",
    "CRCL 83 mL/min", "CRCL 150 mL/min", "Other", "Caucasian",
    "HT 152 cm", "HT 185 cm",
    "NCI=0", "NCI>0", "White", "Other",
    "Male", "Female"
  )

  dfresFREM2 <- suppressWarnings(getForestDFFREM(
    dfCovs           = dfCovs,
    cdfCovsNames     = covlabels,
    covNames         = covNames$covNames,
    functionList     = list(paramFun),
    functionListName = functionListName2,
    numNonFREMThetas = 13,
    numSkipOm        = 2,
    dfParameters     = dfSamplesCOV,
    probs            = c(0.05, 0.95),
    dfRefRow         = NULL,
    quiet            = TRUE,
    ncores           = 1,
    cstrPackages     = c("PMXFrem", "dplyr")
  ))

  ## Snapshot only what is reproducible off this machine; the sampled
  ## columns go through eigen() and are LAPACK-dependent, so they are
  ## checked for shape instead.
  expect_snapshot_value(
    stabilizeRows(dfresFREM2[, setdiff(names(dfresFREM2), forestSampledCols)]),
    style = "serialize",
    cran = TRUE
  )
  expect_forest_sampling_sane(dfresFREM2, rows = 66)
})


test_that("getForestDFFREM keeps relative CI endpoints ordered when paramFunction is negative", {
  runno <- "22-3"
  extFile <- system.file("extdata", paste0("SimVal/run", runno, ".ext"), package = "PMXForest")
  covFile <- system.file("extdata", paste0("SimVal/run", runno, ".cov"), package = "PMXForest")
  modFile <- system.file("extdata", paste0("SimVal/run", runno, ".mod"), package = "PMXForest")
  datFile <- system.file("extdata", paste0("SimVal/DAT-1-MI-PMX-2.csv"), package = "PMXForest")

  dfData <- read.csv(datFile)
  covNames <- getCovNames(modFile = modFile)
  dfCovs <- PMXForest::createInputForestData(
    PMXForest::getCovStats(dfData, covNames$orgCovNames, probs = c(0.05, 0.95))
  )

  set.seed(123)
  dfSamplesCOV <- PMXForest::getSamples(covFile, extFile = extFile, n = 25)

  # Same structure as the positive control below, but returning a NEGATIVE value
  # so the reference value (and func_base / true_base) are negative too.
  paramFunNeg <- function(basethetas, covthetas, dfrow, ...) -basethetas[1] * exp(covthetas[1])
  paramFunPos <- function(basethetas, covthetas, dfrow, ...) basethetas[1] * exp(covthetas[1])

  resNeg <- suppressWarnings(getForestDFFREM(
    dfCovs = dfCovs, covNames = covNames$covNames,
    functionList = list(paramFunNeg), functionListName = "CL",
    numNonFREMThetas = 13, numSkipOm = 2, dfParameters = dfSamplesCOV,
    probs = c(0.05, 0.95), dfRefRow = NULL, quiet = TRUE, ncores = 1
  ))

  resPos <- suppressWarnings(getForestDFFREM(
    dfCovs = dfCovs, covNames = covNames$covNames,
    functionList = list(paramFunPos), functionListName = "CL",
    numNonFREMThetas = 13, numSkipOm = 2, dfParameters = dfSamplesCOV,
    probs = c(0.05, 0.95), dfRefRow = NULL, quiet = TRUE, ncores = 1
  ))

  # Absolute quantile columns are always ascending (Q1 = lower prob, Q2 = upper prob)
  expect_true(all(resNeg$Q1 <= resNeg$Q2))

  # The relative CI columns must also stay ascending: Q1_REL_* is the lower limit,
  # Q2_REL_* the upper limit, since positions 1 and 2 of `probs` are used as the
  # plotted uncertainty. This holds for the positive function ...
  expect_true(all(resPos$Q1_REL_REFFUNC <= resPos$Q2_REL_REFFUNC))
  expect_true(all(resPos$Q1_REL_REFFINAL <= resPos$Q2_REL_REFFINAL))

  # ... and must equally hold for the negative function. Dividing the ascending
  # absolute quantiles by a negative reference reverses their order, so without
  # the fix Q1_REL_* ends up above Q2_REL_* and the forest CI is drawn reversed.
  expect_true(all(resNeg$Q1_REL_REFFUNC <= resNeg$Q2_REL_REFFUNC))
  expect_true(all(resNeg$Q1_REL_REFFINAL <= resNeg$Q2_REL_REFFINAL))
})


test_that("getForestDFFREM covers edge cases", {
  # THIS IS THE FIX: Added the complete setup block to this test
  runno <- "22-3"
  extFile <- system.file("extdata", paste0("SimVal/run", runno, ".ext"), package = "PMXForest")
  covFile <- system.file("extdata", paste0("SimVal/run", runno, ".cov"), package = "PMXForest")
  modFile <- system.file("extdata", paste0("SimVal/run", runno, ".mod"), package = "PMXForest")
  datFile <- system.file("extdata", paste0("SimVal/DAT-1-MI-PMX-2.csv"), package = "PMXForest")
  dfData <- read.csv(datFile)
  covNames <- getCovNames(modFile = modFile)
  dfCovs <- PMXForest::createInputForestData(PMXForest::getCovStats(dfData, covNames$orgCovNames, probs = c(0.05, 0.95)))
  paramFun <- function(basethetas, covthetas, dfrow, ...) {
    return(basethetas[1] * exp(covthetas[1]))
  }
  set.seed(123)
  dfSamplesCOV <- PMXForest::getSamples(covFile, extFile = extFile, n = 5) # Use fewer samples

  # Suppress warnings for all calls in this block
  suppressWarnings({
    # Test case: dfCovs is just a list
    dfCovs_as_list <- as.list(dfCovs)
    res_list <- getForestDFFREM(
      dfCovs = dfCovs_as_list, covNames = covNames$covNames, functionList = list(paramFun),
      numNonFREMThetas = 13, dfParameters = dfSamplesCOV, quiet = TRUE
    )
    expect_s3_class(res_list, "data.frame")

    # Test case: cdfCovsNames and cGrouping are NULL
    res_defaults <- getForestDFFREM(
      dfCovs = dfCovs, covNames = covNames$covNames, functionList = list(paramFun),
      numNonFREMThetas = 13, dfParameters = dfSamplesCOV,
      cdfCovsNames = NULL, cGrouping = NULL, quiet = TRUE
    )
    expect_s3_class(res_defaults, "data.frame")

    # Test case: dfRefRow is provided
    dfRefRow_single <- dfCovs[1, , drop = FALSE]
    res_ref_row <- getForestDFFREM(
      dfCovs = dfCovs, covNames = covNames$covNames, functionList = list(paramFun),
      numNonFREMThetas = 13, dfParameters = dfSamplesCOV,
      dfRefRow = dfRefRow_single, quiet = TRUE
    )
    # Only the reproducible columns are snapshotted. The sampled ones (see
    # forestSampledCols) go through MASS::mvrnorm() -> eigen() and differ
    # between BLAS builds; this line used to snapshot them exactly, and without
    # cran = TRUE, so under R CMD check it skipped - aborting the block and
    # dropping the two tests below with it.
    expect_snapshot_value(
      stabilizeRows(res_ref_row[, setdiff(names(res_ref_row), forestSampledCols)]),
      style = "json2", tolerance = 1e-6, cran = TRUE
    )
    # Not expect_forest_sampling_sane(): paramFun here reads THETA(1), which
    # run22-3.mod fixes at 1 (TVFREL), so every POINT, Q1 and Q2 is exactly 1
    # by construction and a sanity check on the values has nothing to check.
    # This block is about argument handling, so assert that instead.
    expect_equal(nrow(res_ref_row), nrow(res_defaults))
    expect_true("REFROW" %in% names(res_ref_row))

    # Test case: dfRefRow has wrong number of rows
    dfRefRow_wrong <- dfCovs[1:2, , drop = FALSE]
    expect_error(
      getForestDFFREM(
        dfCovs = dfCovs, covNames = covNames$covNames, functionList = list(paramFun),
        numNonFREMThetas = 13, dfParameters = dfSamplesCOV,
        dfRefRow = dfRefRow_wrong, quiet = TRUE
      ),
      regexp = "The number of reference rows"
    )
  }) # End of suppressWarnings
})

## Apart from the cores this is the same call as in the test above; it is a test
## of its own so that it can be skipped where the workers cannot load the
## source tree (see skip_if_workers_load_source_tree()) without dropping the
## argument checks above.
test_that("getForestDFFREM runs with ncores > 1", {
  skip_if_workers_load_source_tree()
  runno <- "22-3"
  extFile <- system.file("extdata", paste0("SimVal/run", runno, ".ext"), package = "PMXForest")
  covFile <- system.file("extdata", paste0("SimVal/run", runno, ".cov"), package = "PMXForest")
  modFile <- system.file("extdata", paste0("SimVal/run", runno, ".mod"), package = "PMXForest")
  datFile <- system.file("extdata", paste0("SimVal/DAT-1-MI-PMX-2.csv"), package = "PMXForest")
  dfData <- read.csv(datFile)
  covNames <- getCovNames(modFile = modFile)
  dfCovs <- PMXForest::createInputForestData(PMXForest::getCovStats(dfData, covNames$orgCovNames, probs = c(0.05, 0.95)))
  paramFun <- function(basethetas, covthetas, dfrow, ...) {
    return(basethetas[1] * exp(covthetas[1]))
  }
  set.seed(123)
  dfSamplesCOV <- PMXForest::getSamples(covFile, extFile = extFile, n = 5)
  args <- list(
    dfCovs = dfCovs, covNames = covNames$covNames, functionList = list(paramFun),
    numNonFREMThetas = 13, dfParameters = dfSamplesCOV, quiet = TRUE
  )

  res_parallel <- suppressWarnings(do.call(
    getForestDFFREM, c(args, list(ncores = 2, cstrPackages = c("PMXFrem", "dplyr")))
  ))
  expect_s3_class(res_parallel, "data.frame")
  ## the same rows and columns as on one core; the sampled columns are not
  ## compared, as they differ with the draws (see forestSampledCols)
  res_serial <- suppressWarnings(do.call(getForestDFFREM, args))
  expect_equal(dim(res_parallel), dim(res_serial))
  expect_equal(names(res_parallel), names(res_serial))
})

test_that("getForestDFFREM oneHot matches a manually pre-encoded dfCovs", {
  skip_on_cran()

  modFile <- system.file("extdata", "SimNeb/run31.mod", package = "PMXFrem")
  extFile <- system.file("extdata", "SimNeb/run31.ext", package = "PMXFrem")
  covFile <- system.file("extdata", "SimNeb/run31.cov", package = "PMXFrem")

  covNames <- getCovNames(modFile = modFile)

  paramFun <- function(basethetas, covthetas, dfrow, ...) {
    basethetas[1] * exp(covthetas[1])
  }

  set.seed(123)
  dfSamples <- PMXForest::getSamples(covFile, extFile = extFile, n = 15)

  ## dfCovs built with raw multi-level NCIL / RACEL columns
  dfCovsRaw <- PMXForest::createInputForestData(
    list(NCIL = c(0, 1, 2), RACEL = c(1, 2, 3))
  )
  spec <- list(NCIL = list(ref = 0), RACEL = list(ref = 1))

  common <- list(
    covNames         = covNames$covNames,
    functionList     = list(paramFun),
    functionListName = "CL",
    numNonFREMThetas = 7,
    numSkipOm        = 2,
    dfParameters     = dfSamples,
    quiet            = TRUE,
    ncores           = 1
  )

  res_onehot <- suppressWarnings(do.call(
    getForestDFFREM, c(list(dfCovs = dfCovsRaw, oneHot = spec), common)
  ))

  ## Written out, not produced by PMXForest::oneHotEncode() - that is the call
  ## getForestDFFREM(oneHot = ) makes internally, so building the expectation
  ## with it would only prove that calling it twice agrees with itself.
  ## NCIL 0 and RACEL 1 are the reference levels, so they get no column; -99
  ## is the missing marker createInputForestData() puts in the other group's
  ## columns.
  dfCovsPre <- data.frame(
    NCIL_1 = c(0, 1, 0, -99, -99, -99),
    NCIL_2 = c(0, 0, 1, -99, -99, -99),
    RACEL_2 = c(-99, -99, -99, 0, 1, 0),
    RACEL_3 = c(-99, -99, -99, 0, 0, 1),
    COVARIATEGROUPS = c("NCIL", "NCIL", "NCIL", "RACEL", "RACEL", "RACEL"),
    stringsAsFactors = FALSE
  )
  ## and confirm the encoder still agrees with it, so a deliberate change to
  ## the convention shows up here rather than silently passing
  expect_equal(
    PMXForest::oneHotEncode(dfCovsRaw, spec = spec, sep = "_", dropOriginal = TRUE),
    dfCovsPre
  )
  res_pre <- suppressWarnings(do.call(
    getForestDFFREM, c(list(dfCovs = dfCovsPre), common)
  ))

  expect_equal(res_onehot, res_pre)
})

test_that("getForestDFFREM accepts a tibble dfCovs and a single covariate", {
  skip_on_cran()
  skip_if_not_installed("tibble")

  runno <- "22-3"
  extFile <- system.file("extdata", paste0("SimVal/run", runno, ".ext"), package = "PMXForest")
  covFile <- system.file("extdata", paste0("SimVal/run", runno, ".cov"), package = "PMXForest")
  modFile <- system.file("extdata", paste0("SimVal/run", runno, ".mod"), package = "PMXForest")

  covNames <- getCovNames(modFile = modFile)
  paramFun <- function(basethetas, covthetas, dfrow, ...) basethetas[1] * exp(covthetas[1])

  set.seed(123)
  dfS <- PMXForest::getSamples(covFile, extFile = extFile, n = 15, quiet = TRUE)

  common <- list(
    covNames = covNames$covNames, functionList = list(paramFun),
    functionListName = "CL", numNonFREMThetas = 13, numSkipOm = 2,
    dfParameters = dfS, quiet = TRUE
  )

  ## tibble dfCovs == data.frame dfCovs
  dfCovs2 <- PMXForest::createInputForestData(list(AGE = c(30, 60), SEX = c(0, 1)))
  ref <- suppressWarnings(do.call(getForestDFFREM, c(list(dfCovs = dfCovs2), common)))
  tbl <- suppressWarnings(do.call(
    getForestDFFREM,
    c(list(dfCovs = tibble::as_tibble(dfCovs2)), common)
  ))
  expect_equal(tbl, ref)

  ## single covariate: column keeps its name, not "dfCovs[i, ]"
  dfCovs1 <- PMXForest::createInputForestData(list(AGE = c(30, 60)))
  res1 <- suppressWarnings(do.call(getForestDFFREM, c(list(dfCovs = dfCovs1), common)))
  expect_false(any(grepl("dfCovs", names(res1))))
  expect_equal(as.character(unique(res1$GROUPNAME)), "AGE")
  expect_setequal(res1$COVNAME, c("AGE=30", "AGE=60"))
})

test_that("a dfRefRow from setupDfRefRow() is accepted without coercion warnings", {
  # setupDfRefRow() returns the same COVARIATEGROUPS column setupDfCovs() does.
  # getForestDFFREM() dropped it from dfCovs but not from dfRefRow, so every
  # row coerced "AGE" and friends to NA - 378 warnings on the four-covariate
  # example in the Forest plot references vignette.
  md <- system.file("extdata/SimNeb", package = "PMXFrem")
  data <- read.csv(file.path(md, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"))
  covs <- c("AGE", "WT")

  dfCovs <- PMXForest::setupDfCovs(
    data = data, covariates = covs, probs = c(0.05, 0.9), idVar = "ID", missVal = -99
  )
  dfRefRow <- PMXForest::setupDfRefRow(
    dfCovs = dfCovs, data = data, covariates = covs, singleRef = TRUE
  )
  expect_true("COVARIATEGROUPS" %in% names(dfRefRow)) # what the caller is handed

  ext <- getExt(extFile = file.path(md, "run31.ext"))
  dfPar <- ext[ext$ITERATION == -1000000000, -1][rep(1, 3), ]
  fl <- list(function(basethetas, covthetas, ...) basethetas[2] * exp(covthetas[1]))

  expect_no_warning(
    res <- getForestDFFREM(
      dfCovs = dfCovs, runno = 31, modDevDir = md, dfParameters = dfPar,
      functionList = fl, functionListName = "CL", dfRefRow = dfRefRow, quiet = TRUE
    )
  )
  # and the reference is the one setupDfRefRow() asked for, not a fallback
  expect_equal(length(unique(res$REFFUNC)), 1)
  expect_true(all(is.finite(res$POINT)))
})

# ---------------------------------------------------------------------------
# covariate names of dfCovs against the FREM model and the parameter function
# ---------------------------------------------------------------------------

## run31 with the function from createFREMParamFunction(), which reads the
## structural covariate FOOD, and two copies of the final estimates.
.covCheckSetup <- function() {
  md <- system.file("extdata/SimNeb", package = "PMXFrem")
  mod <- file.path(md, "run31.mod")
  ext <- getExt(extFile = file.path(md, "run31.ext"))
  out <- createFREMParamFunction(mod, parameters = c("CL", "V", "MAT"), quiet = TRUE)
  fun <- eval(parse(text = out$code))
  list(
    md = md,
    covNames = getCovNames(modFile = mod)$covNames,
    fun = fun,
    ## the same function written by hand: same values, no list of covariates
    handFun = function(basethetas, covthetas, dfrow, ...) {
      fun(basethetas, covthetas, dfrow, ...)
    },
    functionListName = out$functionListName,
    dfPar = ext[ext$ITERATION == -1000000000, -1][rep(1, 2), ]
  )
}

## The pmxCovariateMismatch warnings of one call, muffled; others pass through.
.covMismatches <- function(s, dfCovs, functionList = list(s$fun),
                           functionListName = s$functionListName, ...) {
  ws <- list()
  withCallingHandlers(
    getForestDFFREM(
      dfCovs = dfCovs, runno = 31, modDevDir = s$md, dfParameters = s$dfPar,
      functionList = functionList, functionListName = functionListName,
      quiet = TRUE, ...
    ),
    pmxCovariateMismatch = function(w) {
      ws[[length(ws) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  ws
}

## RACEL2 / RACEL3 for the FREM covariates RACEL_2 / RACEL_3: a separator error
.dfCovsSep <- data.frame(
  COVARIATEGROUPS = c("WT", "WT", "RACEL", "RACEL"),
  WT = c(50, 100, -99, -99),
  RACEL2 = c(-99, -99, 1, 0),
  RACEL3 = c(-99, -99, 0, 1),
  stringsAsFactors = FALSE
)

test_that("a FREM covariate named with the wrong separator gives one pmxCovariateMismatch warning", {
  s <- .covCheckSetup()
  ws <- .covMismatches(s, .dfCovsSep)
  expect_length(ws, 1)
  w <- ws[[1]]
  expect_s3_class(w, "warning")
  expect_setequal(w[["unread"]], c("RACEL2", "RACEL3"))
  ## every FREM covariate but WT, and the structural FOOD
  expect_setequal(w[["absent"]], c(setdiff(s$covNames, "WT"), "FOOD"))
  expect_match(conditionMessage(w), "RACEL2, RACEL3")
  expect_match(conditionMessage(w), "oneHotSep")
  ## another reference level is recoded, so the hint is about levels the model lacks
  expect_match(conditionMessage(w), "a level that the model lacks")
})

test_that("a misspelled structural covariate gives a pmxCovariateMismatch warning", {
  s <- .covCheckSetup()
  dfCovs <- data.frame(
    COVARIATEGROUPS = c("WT", "WT", "FOOD"),
    WT = c(50, 100, -99),
    FOD = c(-99, -99, 0)
  )
  ws <- .covMismatches(s, dfCovs)
  expect_length(ws, 1)
  expect_identical(ws[[1]][["unread"]], "FOD")
  expect_true("FOOD" %in% ws[[1]][["absent"]])
})

test_that("a structural covariate the function reads is not reported as unread", {
  s <- .covCheckSetup()
  dfCovs <- data.frame(
    COVARIATEGROUPS = c("WT", "WT", "FOOD"),
    WT = c(50, 100, -99),
    FOOD = c(-99, -99, 0)
  )
  expect_length(.covMismatches(s, dfCovs), 0)
})

test_that("a subset of the covariates alone gives no pmxCovariateMismatch warning", {
  s <- .covCheckSetup()
  dfCovs <- PMXForest::createInputForestData(list(WT = c(50, 100), SEX = c(1, 2)))
  expect_length(.covMismatches(s, dfCovs), 0)
})

test_that("an unread column alone gives no pmxCovariateMismatch warning", {
  s <- .covCheckSetup()
  ## every FREM covariate and FOOD, plus a column nothing reads
  cols <- c(s$covNames, "FOOD", "EXTRA")
  dfCovs <- as.data.frame(matrix(-99, nrow = 2, ncol = length(cols), dimnames = list(NULL, cols)))
  dfCovs$WT <- c(50, -99)
  dfCovs$EXTRA <- c(-99, 1)
  expect_length(.covMismatches(s, dfCovs), 0)
})

test_that("no covariate-name check is made when a function in functionList is written by hand", {
  s <- .covCheckSetup()
  expect_null(attr(s$handFun, "pmxCovariates"))
  expect_length(.covMismatches(s, .dfCovsSep, functionList = list(s$handFun)), 0)
  ## one generated function and one hand-written: the columns the second
  ## reads are not known either
  expect_length(
    .covMismatches(s, .dfCovsSep,
      functionList = list(s$fun, s$handFun),
      functionListName = c(s$functionListName, paste0(s$functionListName, "2"))
    ),
    0
  )
})

test_that("the covariate-name check compares the columns after one-hot encoding", {
  s <- .covCheckSetup()
  dfCovsRaw <- PMXForest::createInputForestData(list(WT = c(50, 100), RACEL = c(1, 2, 3)))
  spec <- list(RACEL = list(ref = 1))
  expect_length(.covMismatches(s, dfCovsRaw, oneHot = spec), 0)
  ws <- .covMismatches(s, dfCovsRaw, oneHot = spec, oneHotSep = "")
  expect_length(ws, 1)
  expect_setequal(ws[[1]][["unread"]], c("RACEL2", "RACEL3"))
})

## The warnings of one call with quiet = FALSE, muffled; the FFEM printout of
## quiet = FALSE is captured and dropped.
.allWarnings <- function(s, dfCovs, functionList = list(s$fun)) {
  ws <- list()
  utils::capture.output(withCallingHandlers(
    getForestDFFREM(
      dfCovs = dfCovs, runno = 31, modDevDir = s$md, dfParameters = s$dfPar,
      functionList = functionList, functionListName = s$functionListName,
      quiet = FALSE
    ),
    warning = function(w) {
      ws[[length(ws) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  ))
  ws
}

test_that("a subset of FREM covariates and a structural covariate give no warning with quiet = FALSE", {
  ## This call used to warn "Not all covariates in frem model are present in
  ## dfCovs" once, and "Can't find some of the covariates: FOOD" once per FOOD
  ## row and parameter vector, though every name is right.
  s <- .covCheckSetup()
  dfCovs <- data.frame(
    COVARIATEGROUPS = c("WT", "WT", "FOOD"),
    WT = c(50, 100, -99),
    FOOD = c(-99, -99, 0)
  )
  expect_length(.allWarnings(s, dfCovs), 0)
  ## a hand-written function too, which the covariate-name check skips
  expect_length(.allWarnings(s, dfCovs, functionList = list(s$handFun)), 0)
})

test_that("a misspelled name gives the covariate-name warning alone with quiet = FALSE", {
  s <- .covCheckSetup()
  ws <- .allWarnings(s, .dfCovsSep)
  expect_length(ws, 1)
  expect_s3_class(ws[[1]], "pmxCovariateMismatch")
})

# ---------------------------------------------------------------------------
# columns of dfRefRow against dfCovs
# ---------------------------------------------------------------------------

.dfCovsWtSex <- data.frame(WT = c(50, 100, -99), SEX = c(-99, -99, 2))

test_that("a dfRefRow column that dfCovs lacks gives one pmxCovariateMismatch warning", {
  s <- .covCheckSetup()
  ## wt for WT
  ws <- .covMismatches(s, .dfCovsWtSex, dfRefRow = data.frame(wt = 70, SEX = 1))
  expect_length(ws, 1)
  expect_identical(ws[[1]][["refOnly"]], "wt")
  expect_match(conditionMessage(ws[[1]]), "dfRefRow has columns that dfCovs lacks: wt.", fixed = TRUE)
  expect_match(conditionMessage(ws[[1]]), "setupDfRefRow()", fixed = TRUE)
})

test_that("a misspelled dfRefRow column is never read", {
  s <- .covCheckSetup()
  run <- function(dfRefRow) {
    withCallingHandlers(
      getForestDFFREM(
        dfCovs = .dfCovsWtSex, runno = 31, modDevDir = s$md, dfParameters = s$dfPar,
        functionList = list(s$fun), functionListName = s$functionListName,
        quiet = TRUE, dfRefRow = dfRefRow
      ),
      pmxCovariateMismatch = function(w) invokeRestart("muffleWarning")
    )
  }
  sexOnly <- run(data.frame(SEX = 1))$REFFUNC
  expect_equal(run(data.frame(wt = 70, SEX = 1))$REFFUNC, sexOnly)
  expect_false(isTRUE(all.equal(run(data.frame(WT = 70, SEX = 1))$REFFUNC, sexOnly)))
})

test_that("the dfRefRow check is made with a hand-written function too", {
  s <- .covCheckSetup()
  ws <- .covMismatches(s, .dfCovsWtSex,
    functionList = list(s$handFun), dfRefRow = data.frame(wt = 70, SEX = 1)
  )
  expect_length(ws, 1)
  expect_identical(ws[[1]][["refOnly"]], "wt")
})

test_that("a FREM covariate set in dfRefRow and in no column of dfCovs is reported", {
  s <- .covCheckSetup()
  ## SEX is added to dfCovs as a missing column later; the check comes first
  ws <- .covMismatches(s, data.frame(WT = c(50, 100)), dfRefRow = data.frame(WT = 70, SEX = 1))
  expect_length(ws, 1)
  expect_identical(ws[[1]][["refOnly"]], "SEX")
})

test_that("a dfRefRow with fewer columns than dfCovs, or with COVARIATEGROUPS, gives no warning", {
  s <- .covCheckSetup()
  expect_length(.covMismatches(s, .dfCovsWtSex, dfRefRow = data.frame(WT = 70)), 0)
  expect_length(
    .covMismatches(s, .dfCovsWtSex, dfRefRow = data.frame(COVARIATEGROUPS = "REF", WT = 70)),
    0
  )
})

test_that("the dfRefRow check compares the columns after one-hot encoding", {
  s <- .covCheckSetup()
  dfCovsRaw <- PMXForest::createInputForestData(list(WT = c(50, 100), RACEL = c(1, 2, 3)))
  spec <- list(RACEL = list(ref = 1))
  ## a raw RACEL column in both
  expect_length(
    .covMismatches(s, dfCovsRaw, oneHot = spec, dfRefRow = data.frame(WT = 70, RACEL = 1)),
    0
  )
  ## indicator columns already in dfRefRow, a raw column in dfCovs
  expect_length(
    .covMismatches(s, dfCovsRaw, oneHot = spec, dfRefRow = data.frame(WT = 70, RACEL_2 = 0, RACEL_3 = 0)),
    0
  )
})

test_that("a raw categorical column in a one-row dfRefRow is encoded like dfCovs", {
  ## Encoded on its own, a one-row reference has one level, gets no indicator
  ## columns and keeps the raw column, so the reference subject was not
  ## conditioned on the covariate.
  s <- .covCheckSetup()
  dfCovsRaw <- PMXForest::createInputForestData(list(WT = c(50, 100), RACEL = c(1, 2, 3)))
  run <- function(dfRefRow) {
    getForestDFFREM(
      dfCovs = dfCovsRaw, runno = 31, modDevDir = s$md, dfParameters = s$dfPar,
      functionList = list(s$fun), functionListName = s$functionListName,
      quiet = TRUE, oneHot = list(RACEL = list(ref = 1)), dfRefRow = dfRefRow
    )
  }
  indicators <- run(data.frame(WT = 70, RACEL_2 = 1, RACEL_3 = 0))
  expect_equal(run(data.frame(WT = 70, RACEL = 2))$REFFUNC, indicators$REFFUNC)
  ## and the reference level gives zeros in both indicator columns
  expect_equal(
    run(data.frame(WT = 70, RACEL = 1))$REFFUNC,
    run(data.frame(WT = 70, RACEL_2 = 0, RACEL_3 = 0))$REFFUNC
  )
})

test_that("a oneHot reference level that dfCovs lacks is still reported by the encoder", {
  s <- .covCheckSetup()
  dfCovsRaw <- PMXForest::createInputForestData(list(WT = c(50, 100), RACEL = c(1, 2, 3)))
  expect_warning(
    .covMismatches(s, dfCovsRaw, oneHot = list(RACEL = list(ref = 9))),
    "reference level 9"
  )
})

# ---------------------------------------------------------------------------
# covNames against the FREM covariates of the model
# ---------------------------------------------------------------------------

## The FFEM coefficients are attached to covNames by position. In run31.mod,
## RACEL_3 is the 12th FREM covariate and RACEL_2 the 13th, so a covNames
## copied from the columns of a dfCovs (RACEL_2 before RACEL_3) swaps the two
## effects.
.dfCovsRacel <- data.frame(
  COVARIATEGROUPS = c("RACEL", "RACEL"),
  RACEL_2 = c(1, 0),
  RACEL_3 = c(0, 1),
  stringsAsFactors = FALSE
)
.swapRacel <- function(covNames) {
  i <- match(c("RACEL_2", "RACEL_3"), covNames)
  covNames[i] <- covNames[rev(i)]
  covNames
}

test_that("a covNames in another order than the model stops getForestDFFREM()", {
  s <- .covCheckSetup()
  expect_error(
    .covMismatches(s, .dfCovsRacel, covNames = .swapRacel(s$covNames)),
    "Position 12: RACEL_2 in `covNames`, RACEL_3 in the model"
  )
})

test_that("a covNames with other names than the model stops getForestDFFREM()", {
  ## the indicators of a dfCovs built with level 3 of RACEL as the reference
  s <- .covCheckSetup()
  other <- s$covNames
  other[other == "RACEL_3"] <- "RACEL_1"
  expect_error(
    .covMismatches(s, .dfCovsRacel, covNames = other),
    "Position 12: RACEL_1 in `covNames`, RACEL_3 in the model"
  )
  expect_error(
    .covMismatches(s, .dfCovsRacel, covNames = s$covNames[-18]),
    "`covNames` has 17 names and the model 18. Not in `covNames`: SMOK"
  )
})

test_that("the covNames of the model give the result of a covNames left out", {
  s <- .covCheckSetup()
  run <- function(covNames) {
    getForestDFFREM(
      dfCovs = .dfCovsRacel, covNames = covNames, runno = 31, modDevDir = s$md,
      dfParameters = s$dfPar, functionList = list(s$fun),
      functionListName = s$functionListName, quiet = TRUE
    )
  }
  expect_equal(run(s$covNames), run(NULL))
})

test_that("without the model, a covNames in another order is used as given", {
  ## Nothing can be compared, so a renamed covNames stays possible.
  s <- .covCheckSetup()
  run <- function(covNames) {
    getForestDFFREM(
      dfCovs = .dfCovsRacel, covNames = covNames, numNonFREMThetas = 7,
      numSkipOm = 2, dfParameters = s$dfPar, functionList = list(s$fun),
      functionListName = s$functionListName, quiet = TRUE
    )
  }
  right <- run(s$covNames)
  swapped <- run(.swapRacel(s$covNames))
  ## the effects of RACEL_2 and RACEL_3 change places
  cl <- function(r) r$POINT[r$PARAMETER == "CL"]
  expect_equal(cl(swapped), rev(cl(right)))
})

# ---------------------------------------------------------------------------
# A dfRefRow that sets no FREM covariate
# ---------------------------------------------------------------------------

## FOOD is a structural covariate of run31, read by MAT; WT is a FREM
## covariate. The FFEM of a reference row without FREM covariates has no
## covariate terms, as for a row of dfCovs without FREM covariates.
.dfCovsFood <- data.frame(
  COVARIATEGROUPS = c("WT", "WT", "FOOD"),
  WT = c(50, 100, -99),
  FOOD = c(-99, -99, 1),
  stringsAsFactors = FALSE
)
.runFood <- function(s, dfRefRow) {
  getForestDFFREM(
    dfCovs = .dfCovsFood, runno = 31, modDevDir = s$md, dfParameters = s$dfPar,
    functionList = list(s$fun), functionListName = s$functionListName,
    quiet = TRUE, dfRefRow = dfRefRow
  )
}

test_that("a dfRefRow with only a structural covariate gives the reference without covariate effects", {
  s <- .covCheckSetup()
  res <- .runFood(s, data.frame(WT = -99, FOOD = 1))
  ## the reference equals the FOOD = 1 row of dfCovs, which sets no FREM covariate
  noRef <- .runFood(s, NULL)
  foodRow <- noRef[noRef$COVNAME == "FOOD=1", ]
  expect_equal(unique(res$REFFUNC[res$PARAMETER == "MAT"]), foodRow$POINT[foodRow$PARAMETER == "MAT"])
  expect_equal(unique(res$REFFUNC[res$PARAMETER == "CL"]), foodRow$POINT[foodRow$PARAMETER == "CL"])
})

test_that("a dfRefRow with every covariate missing gives the reference of no dfRefRow", {
  s <- .covCheckSetup()
  res <- .runFood(s, data.frame(WT = -99, FOOD = -99))
  expect_equal(res$REFFUNC, .runFood(s, NULL)$REFFUNC)
})

# ---------------------------------------------------------------------------
# Indicator columns of another reference level than the model
# ---------------------------------------------------------------------------

## run31 codes RACEL (levels 1, 2, 3) with the indicators RACEL_2 and RACEL_3,
## so level 1 is the reference of the model. A dfCovs built with level 2 as the
## reference (catRef = list(RACEL = 2) in setupDfCovs()) has RACEL_1 and
## RACEL_3 instead. The rows below are levels 1, 2, 3 of RACEL and two WT rows
## with RACEL in the background at level 2.
.dfRacelRef2 <- data.frame(
  COVARIATEGROUPS = c("WT", "WT", "RACEL", "RACEL", "RACEL"),
  WT = c(56, 126, -99, -99, -99),
  RACEL_1 = c(0, 0, 1, 0, 0),
  RACEL_3 = c(0, 0, 0, 0, 1),
  stringsAsFactors = FALSE
)
## the same rows in the coding of the model
.dfRacelModel <- data.frame(
  COVARIATEGROUPS = c("WT", "WT", "RACEL", "RACEL", "RACEL"),
  WT = c(56, 126, -99, -99, -99),
  RACEL_2 = c(1, 1, 0, 1, 0),
  RACEL_3 = c(0, 0, 0, 0, 1),
  stringsAsFactors = FALSE
)
.runRacel <- function(s, dfCovs, dfRefRow = NULL, quiet = TRUE) {
  getForestDFFREM(
    dfCovs = dfCovs, runno = 31, modDevDir = s$md, dfParameters = s$dfPar,
    functionList = list(s$fun), functionListName = s$functionListName,
    quiet = quiet, dfRefRow = dfRefRow
  )
}
.numCols <- function(r) r[, vapply(r, is.numeric, logical(1))]

test_that("indicator columns of another reference level are recoded to the columns of the model", {
  s <- .covCheckSetup()
  expect_equal(
    .numCols(.runRacel(s, .dfRacelRef2)),
    .numCols(.runRacel(s, .dfRacelModel))
  )
  ## and the covariate-name check has nothing to report
  expect_length(.covMismatches(s, .dfRacelRef2), 0)
})

test_that("a reference subject at another level than the reference of the model needs no editing", {
  s <- .covCheckSetup()
  ref2 <- .runRacel(s, .dfRacelRef2, dfRefRow = data.frame(WT = 84.3, RACEL_1 = 0, RACEL_3 = 0))
  model <- .runRacel(s, .dfRacelModel, dfRefRow = data.frame(WT = 84.3, RACEL_2 = 1, RACEL_3 = 0))
  expect_equal(ref2$REFFUNC, model$REFFUNC)
  expect_equal(ref2$POINT_REL_REFFUNC, model$POINT_REL_REFFUNC)
  ## a dfRefRow in the coding of the model with a dfCovs in another coding
  mixed <- .runRacel(s, .dfRacelRef2, dfRefRow = data.frame(WT = 84.3, RACEL_2 = 1, RACEL_3 = 0))
  expect_equal(mixed$POINT_REL_REFFUNC, model$POINT_REL_REFFUNC)
})

test_that("setupDfCovs() with catRef at another level gives the result of the edits by hand", {
  ## The edits a user made before: the coding of the model, then level 2 of
  ## RACEL written into the background of dfCovs and into dfRefRow.
  s <- .covCheckSetup()
  data <- utils::read.csv(file.path(s$md, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"))
  covs <- c("WT", "RACEL")
  dfCovs2 <- PMXForest::setupDfCovs(data, covs, catRef = list(RACEL = 2), useMissVal = FALSE, idVar = "ID")
  dfRef2 <- PMXForest::setupDfRefRow(dfCovs2, data = data, covariates = covs, idVar = "ID")

  dfCovs1 <- PMXForest::setupDfCovs(data, covs, useMissVal = FALSE, idVar = "ID")
  bg <- dfCovs1$COVARIATEGROUPS != "RACEL"
  dfCovs1$RACEL_2[bg] <- 1
  dfCovs1$RACEL_3[bg] <- 0
  dfRef1 <- PMXForest::setupDfRefRow(dfCovs1, data = data, covariates = covs, idVar = "ID")
  dfRef1$RACEL_2 <- 1
  dfRef1$RACEL_3 <- 0

  byHand <- .runRacel(s, dfCovs1, dfRefRow = dfRef1)
  recoded <- .runRacel(s, dfCovs2, dfRefRow = dfRef2)
  expect_equal(recoded$POINT, byHand$POINT)
  expect_equal(recoded$REFFUNC, byHand$REFFUNC)
})

test_that("the recoding keeps the other columns when the indicator columns come first", {
  s <- .covCheckSetup()
  first <- .dfRacelRef2[, c("RACEL_1", "RACEL_3", "WT", "COVARIATEGROUPS")]
  res <- .runRacel(s, first, dfRefRow = data.frame(RACEL_1 = 0, RACEL_3 = 0, WT = 84.3))
  model <- .runRacel(s, .dfRacelModel, dfRefRow = data.frame(WT = 84.3, RACEL_2 = 1, RACEL_3 = 0))
  expect_equal(res$WT, model$WT)
  expect_equal(res$POINT, model$POINT)
  expect_equal(res$REFFUNC, model$REFFUNC)
})

test_that("missing indicator rows stay missing and a row missing in part stops", {
  s <- .covCheckSetup()
  withMiss <- rbind(.dfRacelRef2, data.frame(COVARIATEGROUPS = "WT", WT = 70, RACEL_1 = -99, RACEL_3 = -99))
  modelMiss <- rbind(.dfRacelModel, data.frame(COVARIATEGROUPS = "WT", WT = 70, RACEL_2 = -99, RACEL_3 = -99))
  expect_equal(.numCols(.runRacel(s, withMiss)), .numCols(.runRacel(s, modelMiss)))

  partly <- rbind(.dfRacelRef2, data.frame(COVARIATEGROUPS = "WT", WT = 70, RACEL_1 = 0, RACEL_3 = -99))
  expect_error(.runRacel(s, partly), "Row 6 of dfCovs")
})

test_that("fractions of the levels are recoded as well", {
  ## a population with 20% at level 1, 30% at level 2 and 50% at level 3
  s <- .covCheckSetup()
  frac2 <- data.frame(COVARIATEGROUPS = "RACEL", RACEL_1 = 0.2, RACEL_3 = 0.5)
  fracModel <- data.frame(COVARIATEGROUPS = "RACEL", RACEL_2 = 0.3, RACEL_3 = 0.5)
  expect_equal(.runRacel(s, frac2)$POINT, .runRacel(s, fracModel)$POINT)
})

test_that("a message names the recoded columns with quiet = FALSE", {
  s <- .covCheckSetup()
  expect_message(
    utils::capture.output(.runRacel(s, .dfRacelRef2, quiet = FALSE)),
    "RACEL_1, RACEL_3 of dfCovs (reference level 2) are recoded to the indicator columns RACEL_2, RACEL_3",
    fixed = TRUE
  )
  ## counted by hand: expect_no_message() of testthat 3.1.7 never fails (it
  ## looks for the class "messsage")
  n <- 0
  withCallingHandlers(
    .runRacel(s, .dfRacelRef2, quiet = TRUE),
    message = function(m) {
      n <<- n + 1
      invokeRestart("muffleMessage")
    }
  )
  expect_equal(n, 0)
})

test_that("indicator columns that cannot be a recoding are left to the covariate-name check", {
  ## RACEL_1 alone: two levels are missing from the columns, so the level of a
  ## row of zeros is not known.
  s <- .covCheckSetup()
  one <- data.frame(COVARIATEGROUPS = c("RACEL", "RACEL"), RACEL_1 = c(1, 0))
  ws <- .covMismatches(s, one)
  expect_length(ws, 1)
  expect_equal(ws[[1]][["unread"]], "RACEL_1")
})
