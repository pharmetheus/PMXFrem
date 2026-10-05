test_that("setupDfCovsEV builds one row per covariate, plus an All row", {
  modFile <- system.file("extdata", "SimNeb", "run31.mod", package = "PMXFrem")

  full <- setupDfCovsEV(modFile)
  covs <- getCovNames(modFile)$orgCovNames
  expect_named(full, covs)
  expect_equal(nrow(full), length(covs) + 1L)
  ## row 1 is All; row i + 1 isolates covariate i
  expect_true(all(full[1, ] == 1))
  expect_equal(as.matrix(full[-1, ]),
    {
      m <- matrix(-99, length(covs), length(covs), dimnames = list(NULL, covs))
      diag(m) <- 1
      m
    },
    ignore_attr = "dimnames"
  )

  expect_error(setupDfCovsEV(modFile, fremCovs = "tmp"))

  sub <- setupDfCovsEV(modFile, fremCovs = c("AGE", "SEX"))
  expect_named(sub, c("AGE", "SEX"))
  expect_equal(sub$AGE, c(1, 1, -99))
  expect_equal(sub$SEX, c(1, -99, 1))

  ## missVal is used for the covariates that are off
  expect_equal(setupDfCovsEV(modFile, fremCovs = c("AGE", "SEX"), missVal = -999)$AGE, c(1, 1, -999))
})

test_that("a conditional covariate is on in every row, not only its own", {
  modFile <- system.file("extdata", "SimNeb", "run31.mod", package = "PMXFrem")

  dfc <- setupDfCovsEV(modFile, fremCovs = c("AGE", "SEX"), conditionalCovs = "FORM")
  expect_named(dfc, c("AGE", "SEX", "FORM"))
  expect_equal(nrow(dfc), 4)
  ## FORM is what every row is conditioned on, so it is never missing
  expect_equal(dfc$FORM, c(1, 1, 1, 1))
  ## the FREM covariates are still isolated one row at a time
  expect_equal(dfc$AGE, c(1, 1, -99, -99))
  expect_equal(dfc$SEX, c(1, -99, 1, -99))

  ## and with every FREM covariate
  all <- setupDfCovsEV(modFile, conditionalCovs = "FORM")
  expect_true(all(all$FORM == 1))
  expect_equal(sum(all$AGE != -99), 2) # the All row and its own
})

test_that("additionalCovs is deprecated in favour of conditionalCovs", {
  modFile <- system.file("extdata", "SimNeb", "run31.mod", package = "PMXFrem")

  ## the new name works silently
  expect_silent(new <- setupDfCovsEV(modFile,
    fremCovs = c("AGE", "SEX"), conditionalCovs = "FORM"
  ))
  expect_true("FORM" %in% names(new))

  ## the old name still works, warns, and gives exactly the same answer - a
  ## deprecation that changed the result would be worse than no deprecation
  expect_warning(
    old <- setupDfCovsEV(modFile, fremCovs = c("AGE", "SEX"), additionalCovs = "FORM"),
    "additionalCovs.*deprecated"
  )
  expect_identical(old, new)

  ## supplying both is a mistake, not a silent precedence rule
  expect_error(
    setupDfCovsEV(modFile,
      fremCovs = c("AGE", "SEX"),
      conditionalCovs = "FORM", additionalCovs = "FORM"
    ),
    "not both"
  )

  ## and neither is still fine
  expect_silent(setupDfCovsEV(modFile, fremCovs = c("AGE", "SEX")))
})
