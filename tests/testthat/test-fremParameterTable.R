test_that("fremParameterTable input validation", {
  # --- Setup for error checking ---
  runno <- 31
  modDevDir <- system.file("extdata/SimNeb/", package = "PMXFrem")
  numNonFREMThetas <- 7

  # --- Tests for each stop() condition ---

  # Missing runno and modName
  expect_error(
    fremParameterTable(
      runno = NULL, modName = NULL, numNonFREMThetas = numNonFREMThetas,
      thetaNum = 1, omegaNum = 1, sigmaNum = 1
    ),
    regexp = "Either runno or modName has to be specified"
  )

  # Missing availCov
  expect_error(
    fremParameterTable(
      runno = runno, modDevDir = modDevDir, numNonFREMThetas = numNonFREMThetas,
      thetaNum = 1, omegaNum = 1, sigmaNum = 1, availCov = NULL
    ),
    regexp = "availCov must be part of the FREM model or 'all'"
  )

  # Non-existent model or ext file
  expect_error(
    fremParameterTable(
      runno = 99, modDevDir = modDevDir, numNonFREMThetas = numNonFREMThetas,
      thetaNum = 1, omegaNum = 1, sigmaNum = 1
    ),
    regexp = "Can not find model file"
  )

  # Non-existent RSE file
  expect_error(
    fremParameterTable(
      runno = runno, modDevDir = modDevDir, numNonFREMThetas = numNonFREMThetas,
      thetaNum = 1, omegaNum = 1, sigmaNum = 1,
      includeRSE = TRUE, bsFile = "non_existent_file.csv"
    ),
    regexp = "Can not find the file for RSE calculations"
  )

  # Mismatched label lengths
  expect_error(
    fremParameterTable(
      runno = runno, modDevDir = modDevDir, numNonFREMThetas = numNonFREMThetas,
      thetaNum = 1:2, omegaNum = 1, sigmaNum = 1, thetaLabels = "one"
    ),
    regexp = "number of theta labels must be the same"
  )
  expect_error(
    fremParameterTable(
      runno = runno, modDevDir = modDevDir, numNonFREMThetas = numNonFREMThetas,
      thetaNum = 1, omegaNum = 1:2, sigmaNum = 1, omegaLabels = "one"
    ),
    regexp = "number of omega labels must be the same"
  )
  expect_error(
    fremParameterTable(
      runno = runno, modDevDir = modDevDir, numNonFREMThetas = numNonFREMThetas,
      thetaNum = 1, omegaNum = 1, sigmaNum = 1:2, sigmaLabels = "one"
    ),
    regexp = "number of sigma labels must be the same"
  )

  # Invalid availCov value
  expect_error(
    fremParameterTable(
      runno = runno, modDevDir = modDevDir, numNonFREMThetas = numNonFREMThetas,
      thetaNum = 1, omegaNum = 1, sigmaNum = 1, availCov = "NOT_A_COV"
    ),
    regexp = "availCov must be part of the FREM model or 'all'"
  )
})


test_that("fremParameterTable works for standard outputs", {
  set.seed(123)
  runno <- 31
  modDevDir <- system.file("extdata/SimNeb/", package = "PMXFrem")
  bsFile <- system.file("extdata/SimNeb/bs31.dir/raw_results_run31.csv", package = "PMXFrem")
  numNonFREMThetas <- 7
  numSkipOm <- 2

  # All covariates - NO RSE
  expect_snapshot_value(stabilize(fremParameterTable(
    runno = runno,
    modDevDir = modDevDir,
    thetaNum = 1:7,
    omegaNum = 1:5,
    sigmaNum = 1:2,
    numNonFREMThetas = numNonFREMThetas,
    numSkipOm = numSkipOm,
    availCov = "all",
    quiet = TRUE
  )), style = "serialize", cran = TRUE)

  # Selection of covariates - NO RSE
  expect_snapshot_value(stabilize(fremParameterTable(
    runno = runno,
    modDevDir = modDevDir,
    thetaNum = 1:7,
    omegaNum = 1:5,
    sigmaNum = 1:2,
    numNonFREMThetas = numNonFREMThetas,
    numSkipOm = numSkipOm,
    availCov = c("SEX", "WT"),
    quiet = TRUE
  )), style = "serialize", cran = TRUE)

  # Test providing a pre-read ext file
  pre_read_ext <- getExt(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"))
  expect_snapshot_value(
    stabilize(fremParameterTable(
      runno = runno,
      modDevDir = modDevDir,
      dfext = pre_read_ext,
      numNonFREMThetas = numNonFREMThetas,
      numSkipOm = numSkipOm,
      thetaNum = 1:7,
      omegaNum = 1:5,
      sigmaNum = 1:2,
      availCov = "all",
      quiet = TRUE
    )),
    style = "serialize",
    cran = TRUE
  )

  # Test RSE calculation with default SD transformations.
  # The expect_warning is removed, as the function is now robust and does not warn.
  expect_snapshot_value(
    stabilize(dropSampledRSE(fremParameterTable(
      runno = runno,
      modDevDir = modDevDir,
      bsFile = bsFile,
      includeRSE = TRUE,
      omegaSD = TRUE,
      sigmaSD = TRUE,
      numNonFREMThetas = numNonFREMThetas,
      numSkipOm = numSkipOm,
      thetaNum = 1:7,
      omegaNum = 1:5,
      sigmaNum = 1:2,
      availCov = "all",
      quiet = TRUE
    ))),
    style = "serialize",
    cran = TRUE
  )
  # 176 = the 175 sampled parameter vectors plus the estimates row
  expect_rse_sane(fremParameterTable(
    runno = runno, modDevDir = modDevDir, bsFile = bsFile, includeRSE = TRUE,
    numNonFREMThetas = numNonFREMThetas, numSkipOm = numSkipOm,
    thetaNum = 1:7, omegaNum = 1:5, sigmaNum = 1:2, availCov = "all", quiet = TRUE
  ), n = 176)

  # No SD transformation (Existing test, keep for regression)
  # The expect_warning is removed, as the function is now robust and does not warn.
  expect_snapshot_value(
    stabilize(dropSampledRSE(fremParameterTable(
      runno = runno,
      modDevDir = modDevDir,
      thetaNum = 2:7,
      omegaNum = c(1, 3, 4, 5),
      sigmaNum = 1,
      includeRSE = TRUE,
      omegaSD = FALSE,
      sigmaSD = FALSE,
      numNonFREMThetas = numNonFREMThetas,
      numSkipOm = numSkipOm,
      availCov = "all",
      quiet = TRUE
    ))),
    style = "serialize",
    cran = TRUE
  )
})

test_that("fremParameterTable orchestrates unified base and coefficient tables", {
  modDevDir <- system.file("extdata/SimNeb/", package = "PMXFrem")

  res_full <- fremParameterTable(
    runno            = 31,
    modDevDir        = modDevDir,
    thetaNum         = 2:7,
    omegaNum         = c(1, 3, 4, 5),
    sigmaNum         = 1,
    parNames         = c("CL_L_h", "V_L", "MAT_h"),
    includeRSE       = TRUE,
    uncertainty      = "RSE",
    numNonFREMThetas = 7,
    numSkipOm        = 2,
    availCov         = "all",
    quiet            = TRUE
  )

  # Check that the wide table correctly split the parameters and RSEs into separate columns
  expected_names <- c("Covariate", "CL_L_h", "CL_L_h RSE", "V_L", "V_L RSE", "MAT_h", "MAT_h RSE")
  expect_equal(names(res_full$coefficientTable_wide), expected_names)

  # Check that the Estimate column is purely numeric-formatted (no parenthesis)
  expect_false(grepl("\\(", res_full$coefficientTable_wide$CL_L_h[1]))

  # Check that the new isolated RSE column contains the parenthesis format "(X%)"
  expect_true(grepl("\\(", res_full$coefficientTable_wide$`CL_L_h RSE`[1]))
})

test_that("fremParameterTable produces a CI column when uncertainty = 'CI'", {
  modDevDir <- system.file("extdata/SimNeb/", package = "PMXFrem")
  bsFile <- system.file("extdata/SimNeb/bs31.dir/raw_results_run31.csv", package = "PMXFrem")

  res <- fremParameterTable(
    runno            = 31,
    modDevDir        = modDevDir,
    thetaNum         = 1:7,
    omegaNum         = 1:5,
    sigmaNum         = 1:2,
    parNames         = c("CL", "V", "MAT"),
    numNonFREMThetas = 7,
    numSkipOm        = 2,
    availCov         = "all",
    includeRSE       = TRUE,
    uncertainty      = "CI",
    ciLevel          = 0.90,
    bsFile           = bsFile,
    n                = 25,
    seed             = 42,
    quiet            = TRUE
  )

  expect_true("90% CI" %in% names(res$parameterTable))
  # The base-table CI cells are "[lo - hi]". Match the shape, not just "a hyphen
  # somewhere": the sprintf separator is a literal " - ", so a looser pattern
  # would pass even for "[NA - NA]" or a lo/lo copy-paste bug.
  ciCells <- res$parameterTable$`90% CI`
  expect_true(all(grepl("^\\[.+ - .+\\]$", ciCells)))
  ciNums <- lapply(
    strsplit(gsub("^\\[|\\]$", "", ciCells), " - ", fixed = TRUE),
    as.numeric
  )
  expect_true(all(lengths(ciNums) == 2L))
  expect_false(any(vapply(ciNums, anyNA, logical(1))))
  # every interval is ordered, and at least one is non-degenerate (THETA1 is
  # `1 FIX`, so its bounds legitimately coincide)
  expect_true(all(vapply(ciNums, function(x) x[1] <= x[2], logical(1))))
  expect_true(any(vapply(ciNums, function(x) x[1] < x[2], logical(1))))

  # the wide coefficient table splits into <Par> and "<Par> 90% CI"
  expect_true(all(c("CL", "CL 90% CI") %in% names(res$coefficientTable_wide)))
  # the CI cell carries a two-number bracketed interval, the estimate is a lone number
  expect_match(res$coefficientTable_wide$`CL 90% CI`[1], "\\[.*-.*\\]")
  expect_false(grepl("[[(]", res$coefficientTable_wide$CL[1]))
})

test_that("fremParameterTable appends a Shrinkage column when includeShrinkage = TRUE", {
  modDevDir <- system.file("extdata/SimNeb/", package = "PMXFrem")

  res <- fremParameterTable(
    runno            = 31,
    modDevDir        = modDevDir,
    thetaNum         = 1:7,
    omegaNum         = 1:5,
    sigmaNum         = 1:2,
    numNonFREMThetas = 7,
    numSkipOm        = 2,
    availCov         = c("SEX", "WT"),
    includeShrinkage = TRUE,
    ffemModName      = "run31max1-2",
    shrinkageType    = "ETA_SD",
    quiet            = TRUE
  )

  expect_true("Shrinkage (%)" %in% names(res$parameterTable))
  # THETA / SIGMA rows are dashes; OMEGA rows carry a value
  omegaRows <- res$parameterTable$Type == "OMEGA"
  expect_true(all(res$parameterTable$`Shrinkage (%)`[!omegaRows] == "-"))
  expect_true(any(res$parameterTable$`Shrinkage (%)`[omegaRows] != "-"))
})

test_that("fremParameterTable rejects bad shrinkage arguments", {
  modDevDir <- system.file("extdata/SimNeb/", package = "PMXFrem")
  base_args <- list(
    runno = 31, modDevDir = modDevDir,
    thetaNum = 1:7, omegaNum = 1:5, sigmaNum = 1:2,
    numNonFREMThetas = 7, numSkipOm = 2, availCov = "all", quiet = TRUE
  )

  # includeShrinkage without an ffemModName
  expect_error(
    do.call(fremParameterTable, c(base_args, includeShrinkage = TRUE)),
    "ffemModName must be provided"
  )

  # an unknown shrinkageType
  expect_error(
    do.call(fremParameterTable, c(
      base_args,
      list(
        includeShrinkage = TRUE, ffemModName = "run31max1-2",
        shrinkageType = "NOT_A_TYPE"
      )
    )),
    "Invalid shrinkageType"
  )
})

test_that("the uncertainty sampling is reproducible, and leaves the caller's RNG alone", {
  ## RSE and CI come from `n` draws, so they are Monte-Carlo quantities. With
  ## the seed left alone they inherited whatever RNG state the caller arrived
  ## with: two consecutive calls differed by up to 20%, and the value therefore
  ## depended on which tests had run before this one. No snapshot of it could be
  ## stable, which is what produced the long-running snapshot drift (T17).
  modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
  bsFile <- system.file("extdata/SimNeb/bs31.dir/raw_results_run31.csv", package = "PMXFrem")
  rse <- function(...) {
    t <- fremParameterTable(
      runno = "31", modDevDir = modDevDir, bsFile = bsFile,
      includeRSE = TRUE, numNonFREMThetas = 7, numSkipOm = 2,
      thetaNum = 1:7, omegaNum = 1:5, sigmaNum = 1:2,
      availCov = "all", quiet = TRUE, ...
    )
    as.character(t$parameterTable[["RSE (%)"]])
  }

  ## deliberately perturb the RNG between the calls: the result must not care
  a <- rse()
  invisible(stats::runif(37))
  b <- rse()
  expect_identical(a, b)

  ## an explicit seed still works, and a different one gives something different
  expect_identical(rse(seed = 99), rse(seed = 99))
  expect_false(identical(rse(seed = 99), rse(seed = 100)))

  ## and it does not move the caller's stream
  set.seed(4242)
  before <- stats::runif(3)
  set.seed(4242)
  invisible(rse())
  expect_identical(stats::runif(3), before)
})
