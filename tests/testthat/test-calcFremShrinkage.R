### FILE: tests/testthat/test-calcFremShrinkage.R ###

library(testthat)
library(PMXFrem)

modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
modName <- "run31max1-2"

test_that("calcFremShrinkage computes valid shrinkages and returns a strictly formatted data.frame", {
  # Run the native shrinkage function
  shrinkages <- calcFremShrinkage(
    modName = modName,
    modDevDir = modDevDir,
    quiet = TRUE
  )

  # Check object type
  expect_s3_class(shrinkages, "data.frame")
  expect_true(nrow(shrinkages) > 0)

  # Check expected columns
  expected_cols <- c("Parameter", "ETA_Var", "ETA_SD", "EBV_Var", "EBV_SD")
  expect_true(all(expected_cols %in% names(shrinkages)))

  # Check column data types
  expect_type(shrinkages$Parameter, "character")
  expect_type(shrinkages$ETA_Var, "double")
  expect_type(shrinkages$ETA_SD, "double")
  expect_type(shrinkages$EBV_Var, "double")
  expect_type(shrinkages$EBV_SD, "double")

  # Verify specific structural ETAs were parsed
  expect_true("ETA1" %in% shrinkages$Parameter)

  # ---- the values, which nothing above constrains -------------------------
  # Without these the test's name is a claim it does not check: NaN, -500 and
  # 1e6 all satisfy "is a double" and "has these columns".
  num <- shrinkages[, c("ETA_Var", "ETA_SD", "EBV_Var", "EBV_SD")]
  expect_true(all(vapply(num, function(c) all(is.finite(c)), TRUE)))
  expect_true(all(vapply(num, function(c) all(c >= 0 & c <= 100), TRUE)),
    info = "a shrinkage is a percentage"
  )
  expect_equal(nrow(shrinkages), 5) # run31max1-2 has five etas

  # An SD shrinkage is an exact function of the variance shrinkage:
  #   sd = 100 * (1 - sqrt(1 - var/100))
  # so this pins the arithmetic rather than its plausibility.
  expect_equal(
    shrinkages$ETA_SD, 100 * (1 - sqrt(1 - shrinkages$ETA_Var / 100)),
    tolerance = 1e-4
  )
  expect_equal(
    shrinkages$EBV_SD, 100 * (1 - sqrt(1 - shrinkages$EBV_Var / 100)),
    tolerance = 1e-4
  )

  # and one value anchored outright, so a wholesale change cannot pass
  expect_equal(shrinkages$ETA_Var[shrinkages$Parameter == "ETA1"], 60.8814,
    tolerance = 1e-4
  )
  # EBV_SD is derived from EBV_Var, so the relationship above holds even when
  # EBV_Var itself is computed wrongly. Anchor it outright too.
  expect_equal(shrinkages$EBV_Var[shrinkages$Parameter == "ETA1"], 56.8161,
    tolerance = 1e-4
  )
})

test_that("calcFremShrinkage safely aborts if required files are missing", {
  # Test with a non-existent model name to trigger missing .ext file error
  expect_error(
    calcFremShrinkage(
      modName = "missing_phantom_model",
      modDevDir = modDevDir,
      quiet = TRUE
    ),
    "Cannot find .ext file at"
  )
})

test_that("calcFremShrinkage aborts when the .ext is present but the .phi is not", {
  td <- withr::local_tempdir()
  file.copy(file.path(modDevDir, c("run31max1-2.mod", "run31max1-2.ext")), td)
  expect_error(
    calcFremShrinkage(modName = "run31max1-2", modDevDir = td, quiet = TRUE),
    "Cannot find .phi file at"
  )
})

test_that("calcFremShrinkage reports dropped uninformative subjects when quiet = FALSE", {
  expect_message(
    calcFremShrinkage(
      modName = modName, modDevDir = modDevDir,
      dropUninformative = TRUE, quiet = FALSE
    ),
    "uninformative subjects"
  )
  # keeping them is silent
  expect_no_message(
    calcFremShrinkage(
      modName = modName, modDevDir = modDevDir,
      dropUninformative = FALSE, quiet = FALSE
    )
  )
})
