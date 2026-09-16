test_that("setupDfCovsEV works", {
  modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
  fremRunno <- 31
  modFile <- file.path(modDevDir, paste0("run", fremRunno, ".mod"))

  dfData <- read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"))

  expect_snapshot_value(stabilize(setupDfCovsEV(modFile)), style = "serialize")
  expect_error(setupDfCovsEV(modFile, fremCovs = "tmp"))

  expect_snapshot_value(stabilize(setupDfCovsEV(modFile, fremCovs = c("AGE", "SEX"))), style = "serialize")
  expect_snapshot_value(stabilize(setupDfCovsEV(modFile, conditionalCovs = "FORM")), style = "serialize")
  expect_snapshot_value(stabilize(setupDfCovsEV(modFile, fremCovs = c("AGE", "SEX"), conditionalCovs = "FORM")), style = "serialize")
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
