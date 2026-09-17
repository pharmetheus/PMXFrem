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

    # Test case: ncores > 1
    if (requireNamespace("doParallel", quietly = TRUE)) {
      res_parallel <- getForestDFFREM(
        dfCovs = dfCovs, covNames = covNames$covNames, functionList = list(paramFun),
        numNonFREMThetas = 13, dfParameters = dfSamplesCOV,
        ncores = 2, cstrPackages = c("PMXFrem", "dplyr"), quiet = TRUE
      )
      expect_s3_class(res_parallel, "data.frame")
    }
  }) # End of suppressWarnings
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
