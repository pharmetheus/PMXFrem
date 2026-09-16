# Register a parallel backend with a safe number of cores for testing
doParallel::registerDoParallel(cores = 2)

# Ensure the cluster is stopped when the test file finishes
# This prevents leftover processes and is good practice
withr::defer(doParallel::stopImplicitCluster(), teardown_env())

test_that("getExplainedVar works on main paths", {
  modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
  fremRunno <- 31
  modFile <- file.path(modDevDir, paste0("run", fremRunno, ".mod"))
  covNames <- getCovNames(modFile = modFile)

  ## Set up dfCovs
  dfData <- read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")) %>%
    dplyr::filter(BLQ == 0) %>%
    dplyr::distinct(ID, .keep_all = TRUE)

  dfCovs <- setupDfCovsEV(modFile)

  cstrCovariates <- c("All", names(dfCovs))

  ## The parameter function list
  functionList2 <- list(
    function(basethetas, covthetas, dfrow, etas, ...) {
      return(basethetas[2] * exp(covthetas[1] + etas[3]))
    },
    function(basethetas, covthetas, dfrow, etas, ...) {
      return(basethetas[3] * exp(covthetas[2] + etas[4]))
    }
  )

  ## The function that returns a list of return values
  functionList22 <- function(basethetas, covthetas, dfrow, etas, ...) {
    return(
      c(
        basethetas[2] * exp(covthetas[1] + etas[3]),
        basethetas[3] * exp(covthetas[2] + etas[4])
      )
    )
  }
  functionListName2 <- c("CL", "V")

  dfres0 <- getExplainedVar(
    type = 0,
    data = NULL,
    dfCovs = dfCovs,
    numNonFREMThetas = 7,
    numSkipOm = 2,
    functionList = functionList2,
    functionListName = functionListName2,
    cstrCovariates = cstrCovariates,
    modDevDir = modDevDir,
    runno = fremRunno,
    ncores = 1,
    quiet = TRUE,
    seed = 123
  )
  expect_snapshot_value(stabilize(as.data.frame(dfres0)), style = "serialize", cran = TRUE)

  ## Test that the delta rule can handle 2 return values
  dfres02 <- getExplainedVar(
    type = 0,
    data = NULL,
    dfCovs = dfCovs,
    numNonFREMThetas = 7,
    numSkipOm = 2,
    functionList = list(functionList22),
    functionListName = functionListName2,
    cstrCovariates = cstrCovariates,
    modDevDir = modDevDir,
    runno = fremRunno,
    ncores = 1,
    quiet = TRUE,
    seed = 123
  )
  expect_snapshot_value(stabilize(as.data.frame(dfres02)), style = "serialize", cran = TRUE)


  dfres1 <- getExplainedVar(
    type = 1,
    data = dfData,
    dfCovs = dfCovs,
    numNonFREMThetas = 7,
    numSkipOm = 2,
    functionList = functionList2,
    functionListName = functionListName2,
    cstrCovariates = cstrCovariates,
    modDevDir = modDevDir,
    runno = fremRunno,
    ncores = 2,
    quiet = TRUE,
    seed = 123
  )
  expect_snapshot_value(stabilize(as.data.frame(dfres1)), style = "serialize", cran = TRUE)

  ## Check that you can base the calculations on a subset of the covariates
  dfres1a <- getExplainedVar(
    type = 1,
    data = dfData,
    dfCovs = dfCovs %>% dplyr::select(AGE, WT) %>% dplyr::slice(1, 2, 17),
    numNonFREMThetas = 7,
    numSkipOm = 2,
    functionList = functionList2,
    functionListName = functionListName2,
    cstrCovariates = c("ALL", "AGE", "WT"),
    modDevDir = modDevDir,
    runno = fremRunno,
    availCov = c("AGE", "WT"),
    ncores = 1,
    quiet = TRUE,
    seed = 123
  )
  expect_snapshot_value(stabilize(as.data.frame(dfres1a)), style = "serialize", cran = TRUE)
  expect_gt(dfres1 %>% dplyr::select(TOTCOVVAR) %>% dplyr::slice(1), dfres1a %>% dplyr::select(TOTCOVVAR) %>% dplyr::slice(1))
  val1 <- dfres1 %>%
    dplyr::select(TOTVAR) %>%
    dplyr::slice(1)
  val2 <- dfres1a %>%
    dplyr::select(TOTVAR) %>%
    dplyr::slice(1)
  expect_equal(val1, val2)


  ## Check that you can base the calculations on one covariate
  dfres1b <- getExplainedVar(
    type = 1,
    data = dfData,
    dfCovs = dfCovs %>% dplyr::select(AGE) %>% dplyr::slice(1, 2),
    numNonFREMThetas = 7,
    numSkipOm = 2,
    functionList = functionList2,
    functionListName = functionListName2,
    cstrCovariates = c("ALL", "AGE"),
    modDevDir = modDevDir,
    runno = fremRunno,
    availCov = c("AGE"),
    ncores = 1,
    quiet = TRUE,
    seed = 123
  )
  expect_snapshot_value(stabilize(as.data.frame(dfres1b)), style = "serialize", cran = TRUE)
  val1 <- as.numeric(dfres1b %>% dplyr::select(TOTCOVVAR) %>% dplyr::slice(1))
  val2 <- as.numeric(dfres1b %>% dplyr::select(COVVAR) %>% dplyr::slice(1))
  expect_equal(val1, val2)

  # THIS IS THE FIX: Wrap the call in expect_warning()
  expect_warning(
    dfres2 <- getExplainedVar(
      type = 2,
      data = dfData,
      dfCovs = dfCovs,
      numNonFREMThetas = 7,
      numSkipOm = 2,
      functionList = functionList2,
      functionListName = functionListName2,
      cstrCovariates = cstrCovariates,
      modDevDir = modDevDir,
      runno = fremRunno,
      ncores = 2,
      numETASamples = 10,
      quiet = TRUE,
      seed = 123
    ),
    regexp = "Presence of FFEM covariates is indicated"
  )
  expect_snapshot_value(stabilize(as.data.frame(dfres2)), style = "serialize", cran = TRUE)

  dfres3 <- getExplainedVar(
    type = 3,
    data = dfData,
    dfCovs = dfCovs,
    numNonFREMThetas = 7,
    numSkipOm = 2,
    functionList = functionList2,
    functionListName = functionListName2,
    cstrCovariates = cstrCovariates,
    modDevDir = modDevDir,
    runno = fremRunno,
    ncores = 2,
    numETASamples = 10,
    quiet = TRUE,
    seed = 123
  )
  expect_snapshot_value(stabilize(as.data.frame(dfres3)), style = "serialize", cran = TRUE)
})



# This block of tests is already robust and does not use snapshots. No changes needed.
test_that("getExplainedVar input checks and edge cases", {
  modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
  fremRunno <- 31
  modFile <- file.path(modDevDir, paste0("run", fremRunno, ".mod"))

  dfData <- read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")) %>%
    dplyr::filter(BLQ == 0) %>%
    dplyr::distinct(ID, .keep_all = TRUE)

  dfCovs <- setupDfCovsEV(modFile)

  # Test error: type > 0 but data is missing
  expect_error(
    getExplainedVar(
      type = 1, data = NULL, dfCovs = dfCovs, numNonFREMThetas = 7,
      runno = fremRunno, modDevDir = modDevDir
    ),
    regexp = "data can not be missing with type 1-3"
  )

  # Test error: cstrCovariates length does not match nrow(dfCovs)
  expect_error(
    getExplainedVar(
      type = 0, dfCovs = dfCovs, cstrCovariates = "wrong_length", numNonFREMThetas = 7,
      runno = fremRunno, modDevDir = modDevDir
    ),
    regexp = "must have the same length as the number of rows"
  )

  # Test error: number of etas does not match number of subjects for type = 1
  expect_error(
    getExplainedVar(
      type = 1, data = dfData, etas = dfData[1:10, ], dfCovs = dfCovs, numNonFREMThetas = 7,
      runno = fremRunno, modDevDir = modDevDir
    ),
    regexp = "number of etas should be the same as the number of subjects"
  )

  # Test warning: for type=2, check for presence of FFEM covariates in dfCovs
  expect_warning(
    getExplainedVar(
      type = 2, data = dfData, dfCovs = dfCovs[1:5, ], numNonFREMThetas = 7, numSkipOm = 2,
      runno = fremRunno, quiet = TRUE, seed = 123, numETASamples = 10, modDevDir = modDevDir
    ),
    regexp = "Presence of FFEM covariates is indicated"
  )

  # Test verbose output with quiet = FALSE
  expect_output(
    getExplainedVar(
      type = 0, dfCovs = dfCovs, numNonFREMThetas = 7, runno = fremRunno,
      quiet = FALSE, modDevDir = modDevDir
    )
  )

  # Test providing etas directly as an argument for type = 1
  phiFile <- system.file("extdata/SimNeb/run31.phi", package = "PMXFrem")
  etas_from_file <- getPhi(phiFile)[, 3:9]

  data_subset <- dfData[seq_len(nrow(etas_from_file)), ]

  res_with_etas <- getExplainedVar(
    type = 1, data = data_subset, etas = etas_from_file, dfCovs = dfCovs,
    numNonFREMThetas = 7, numSkipOm = 2, runno = fremRunno,
    quiet = TRUE, modDevDir = modDevDir
  )

  expect_s3_class(res_with_etas, "data.frame")
  expect_gt(nrow(res_with_etas), 0)
})

# --- Setup Context for the Tests ---
library(dplyr)
modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
fremRunno <- 31
modFile <- file.path(modDevDir, paste0("run", fremRunno, ".mod"))

# Load full longitudinal dataset
dfData <- read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")) %>%
  filter(BLQ == 0)

dfCovs <- setupDfCovsEV(modFile)

# Minimal function to bypass complex math during logic testing
dummyFunc <- list(function(basethetas, covthetas, dfrow, etas, ...) {
  1
})


# --- Test 1: Longitudinal Subsetting (The strID Fix) ---
test_that("getExplainedVar correctly subsets longitudinal data to one row per ID", {
  # The raw dfData has multiple rows per ID.
  # If the !duplicated("ID") bug is present, dataI becomes the full dataset,
  # triggering the eta length mismatch error.
  # With !duplicated(data[[strID]]), it correctly reduces to 1 row per ID.
  # We expect this to run completely without throwing the eta mismatch error
  expect_error(
    getExplainedVar(
      type             = 1,
      data             = dfData,
      dfCovs           = dfCovs,
      numNonFREMThetas = 7,
      numSkipOm        = 2,
      functionList     = dummyFunc,
      functionListName = "TEST",
      modDevDir        = modDevDir,
      runno            = fremRunno,
      quiet            = TRUE
    ),
    NA # NA means "Expect NO error to be thrown"
  )
})


# --- Test 2: Missing Covariates (The Fake Exit Fix) ---
test_that("getExplainedVar strictly stops if a model categorical covariate is missing", {
  # Remove 'RACEL' which is a polychotomous categorical covariate in SimNeb.
  # This directly triggers the loop over `fremCovs` that we patched.
  dfData_missing <- dfData
  dfData_missing$RACEL <- NULL

  expect_error(
    getExplainedVar(
      type             = 1,
      data             = dfData_missing,
      dfCovs           = dfCovs,
      numNonFREMThetas = 7,
      numSkipOm        = 2,
      functionList     = dummyFunc,
      functionListName = "TEST",
      modDevDir        = modDevDir,
      runno            = fremRunno,
      quiet            = TRUE
    ),
    "Can't find RACEL in the dataset" # Verifies our exact stop() triggers
  )
})


test_that("getExplainedVar handles data.table inputs robustly without NSE scoping errors", {
  # Setup paths to our validated integration test data
  modDevDir <- system.file("extdata", "SimNeb", package = "PMXFrem")
  run_frem <- 31
  mod_path <- file.path(modDevDir, "run31.mod")

  # Load the real dataset as a STRICT data.table to trigger the vulnerability
  data_file <- system.file("extdata", "SimNeb", "DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")
  mock_dt <- data.table::fread(data_file, header = TRUE)

  # Filter out BLQ exactly as the walk-through does to maintain dimension alignment
  mock_dt <- mock_dt[mock_dt$BLQ == 0, ]

  # Setup valid covariates and functions
  dfCovsEV <- setupDfCovsEV(mod_path)

  funcList_var <- list(
    function(basethetas, covthetas, dfrow, etas, ...) {
      basethetas[2] * exp(covthetas[1] + etas[3])
    }
  )

  # The function should now successfully cast the data.table to a base data.frame
  # and process the entire variance calculation without throwing the 'j symbol' error.
  expect_silent({
    res <- getExplainedVar(
      type             = 1,
      data             = mock_dt,
      dfCovs           = dfCovsEV,
      cstrCovariates   = c("ALL", names(dfCovsEV)),
      numNonFREMThetas = 7,
      numSkipOm        = 2,
      functionList     = funcList_var,
      functionListName = "CL",
      modDevDir        = modDevDir,
      runno            = run_frem,
      numETASamples    = 2, # Keep low for fast testing
      quiet            = TRUE,
      seed             = 123
    )
  })

  expect_s3_class(res, "data.frame")
})

# ---------------------------------------------------------------------------
# A decomposition with a closed-form answer
#
# For a parameter linear in one eta, P = theta + b * ETA(3), the delta rule is
# exact, so the variance components can be written down from the model's own
# OMEGA and compared with what getExplainedVar() reports:
#
#   TOTVAR              = b^2 * OMEGA(3,3)
#   TOTCOVVAR (WT only) = b^2 * OMEGA(6,3)^2 / OMEGA(6,6)
#
# and with WT the only covariate in play, COVVAR must equal TOTCOVVAR - all of
# the explainable variability is explained by the only covariate there is.
# ---------------------------------------------------------------------------

test_that("getExplainedVar reproduces a closed-form variance decomposition", {
  modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
  ext <- file.path(modDevDir, "run31.ext")
  fin <- getExt(extFile = ext)
  fin <- fin[fin$ITERATION == -1000000000, , drop = FALSE]
  om <- function(i, j) as.numeric(fin[[paste0("OMEGA.", i, ".", j, ".")]])

  b <- 2.5
  totVarHand <- b^2 * om(3, 3)
  totCovVarHand <- b^2 * om(6, 3)^2 / om(6, 6)

  fl <- list(function(basethetas, covthetas, dfrow, etas, ...) {
    basethetas[2] + b * etas[3]
  })

  res <- getExplainedVar(
    type = 0, data = NULL, dfCovs = data.frame(WT = 1),
    numNonFREMThetas = 7, numSkipOm = 2,
    functionList = fl, functionListName = "CLlin",
    cstrCovariates = "WT", modDevDir = modDevDir, modName = "run31",
    availCov = "WT", quiet = TRUE
  )

  # delta rule via numDeriv::grad(), so a finite-difference tolerance
  expect_equal(res$TOTVAR[1], totVarHand, tolerance = 1e-8)
  expect_equal(res$TOTCOVVAR[1], totCovVarHand, tolerance = 1e-8)
  # WT is the only covariate, so it accounts for all of TOTCOVVAR. A
  # single-column dfCovs used to collapse to a numeric vector, leaving
  # names() empty, no covariate active, and COVVAR silently 0.
  expect_equal(res$COVVAR[1], res$TOTCOVVAR[1], tolerance = 1e-8)
})
