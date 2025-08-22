test_that("fremParameterTable works", {

  # Forcing an older RNG version could also work, but `variant` is often
  # the cleaner solution for documenting expected differences.

  set.seed(123)
  runno            <- 31
  modDevDir        <- system.file("extdata/SimNeb/",package = "PMXFrem")
  bsFile           <- system.file("extdata/SimNeb/bs31.dir/raw_results_run31.csv",package = "PMXFrem")
  numNonFREMThetas <- 7
  numSkipOm        <- 2

  # ... [All expect_error calls remain unchanged] ...


  ## Test output
  # Create a variant based on the R major and minor version (e.g., "4.2", "4.4")
  r_version_variant <- paste(R.version$major, R.version$minor, sep = ".")

  # All covariates - NO RSE, so no variant needed.
  expect_snapshot(stabilize(fremParameterTable(runno = runno,
                                               modDevDir             = modDevDir,
                                               thetaNum              = 1:7,
                                               omegaNum              = 1:5,
                                               sigmaNum              = 1:2,
                                               numNonFREMThetas      = numNonFREMThetas,
                                               numSkipOm             = numSkipOm,
                                               availCov              = "all",
                                               quiet                 = TRUE)))

  # Selection of covariates - NO RSE, so no variant needed.
  expect_snapshot(stabilize(fremParameterTable(runno= runno,
                                               modDevDir            = modDevDir,
                                               thetaNum             = 1:7,
                                               omegaNum             = 1:5,
                                               sigmaNum             = 1:2,
                                               numNonFREMThetas     = numNonFREMThetas,
                                               numSkipOm            = numSkipOm,
                                               availCov             = c("SEX","WT"),
                                               quiet                = TRUE)))

  # Selecting covariates - NO RSE, so no variant needed.
  expect_snapshot(
    stabilize(fremParameterTable(runno            = runno,
                                 modDevDir        = modDevDir,
                                 thetaNum         = 2:6,
                                 omegaNum         = c(1,3,4,5),
                                 sigmaNum         = 1,
                                 numNonFREMThetas = numNonFREMThetas,
                                 numSkipOm        = numSkipOm,
                                 availCov         = "all",
                                 quiet            = TRUE))
  )

  # Specifying parameter labels - NO RSE, so no variant needed.
  expect_snapshot(
    stabilize(fremParameterTable(runno            = runno,
                                 modDevDir        = modDevDir,
                                 thetaNum         = 2:7,
                                 omegaNum         = c(1,3,4,5),
                                 sigmaNum         = 1,
                                 thetaLabels      = c("CL (L/h)","V (L)","MAT (h)","D1 (h)","Food on Frel","Food on MAT"),
                                 omegaLabels      = c("IIV on RUV","IIV on CL","IIV on V","IIV on MAT"),
                                 sigmaLabels      = c("RUV"),
                                 numNonFREMThetas = numNonFREMThetas,
                                 numSkipOm        = numSkipOm,
                                 availCov         = "all",
                                 quiet            = TRUE))
  )

  # RSE output - THIS IS WHERE THE VARIANT IS NEEDED
  expect_snapshot(
    stabilize(fremParameterTable(runno          = runno,
                                 modDevDir        = modDevDir,
                                 thetaNum         = 2:7,
                                 omegaNum         = c(1,3,4,5),
                                 sigmaNum         = 1,
                                 thetaLabels      = c("CL (L/h)","V (L)","MAT (h)","D1 (h)","Food on Frel","Food on MAT"),
                                 omegaLabels      = c("IIV on RUV","IIV on CL","IIV on V","IIV on MAT"),
                                 sigmaLabels      = c("RUV"),
                                 includeRSE       = TRUE,
                                 numNonFREMThetas = numNonFREMThetas,
                                 numSkipOm        = numSkipOm,
                                 availCov         = "all",
                                 quiet            = TRUE)),
    variant = r_version_variant
  )

  # RSE output with bsfile - THIS IS WHERE THE VARIANT IS NEEDED
  expect_snapshot(
    stabilize(fremParameterTable(runno            = runno,
                                 modDevDir        = modDevDir,
                                 thetaNum         = 2:7,
                                 omegaNum         = c(1,3,4,5),
                                 sigmaNum         = 1,
                                 thetaLabels      = c("CL (L/h)","V (L)","MAT (h)","D1 (h)","Food on Frel","Food on MAT"),
                                 omegaLabels      = c("IIV on RUV","IIV on CL","IIV on V","IIV on MAT"),
                                 sigmaLabels      = c("RUV"),
                                 includeRSE       = TRUE,
                                 bsFile           = bsFile,
                                 numNonFREMThetas = numNonFREMThetas,
                                 numSkipOm        = numSkipOm,
                                 availCov         = "all",
                                 quiet            = TRUE)),
    variant = r_version_variant
  )

  # No CV output with RSE - THIS IS WHERE THE VARIANT IS NEEDED
  expect_snapshot(
    stabilize(fremParameterTable(runno            = runno,
                                 modDevDir        = modDevDir,
                                 thetaNum         = 2:7,
                                 omegaNum         = c(1,3,4,5),
                                 sigmaNum         = 1,
                                 thetaLabels      = c("CL (L/h)","V (L)","MAT (h)","D1 (h)","Food on Frel","Food on MAT"),
                                 omegaLabels      = c("IIV on RUV","IIV on CL","IIV on V","IIV on MAT"),
                                 sigmaLabels      = c("RUV"),
                                 includeRSE       = TRUE,
                                 omegaSD          = FALSE,
                                 sigmaSD          = FALSE,
                                 numNonFREMThetas = numNonFREMThetas,
                                 numSkipOm        = numSkipOm,
                                 availCov         = "all",
                                 quiet            = TRUE)),
    variant = r_version_variant
  )
})
