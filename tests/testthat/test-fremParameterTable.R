test_that("fremParameterTable works", {

  set.seed(123)
  runno            <- 31
  modDevDir        <- system.file("extdata/SimNeb/",package = "PMXFrem")
  bsFile           <- system.file("extdata/SimNeb/bs31.dir/raw_results_run31.csv",package = "PMXFrem")
  numNonFREMThetas <- 7
  numSkipOm        <- 2


  # Test input checks
  expect_error(
    fremParameterTable(runno            = 21,
                       modDevDir        = modDevDir,
                       thetaNum         = 1:7,
                       omegaNum         = 1:5,
                       sigmaNum         = 1:2,
                       numNonFREMThetas = numNonFREMThetas,
                       numSkipOm        = numSkipOm,
                       availCov         = "all",
                       quiet            = FALSE),
    regexp = "model")

  expect_error(
    fremParameterTable(runno            = runno,
                       modDevDir        = modDevDir,
                       thetaNum         = 1:7,
                       omegaNum         = 1:5,
                       sigmaNum         = 1:2,
                       includeRSE       = TRUE,
                       bsFile           = "tmp",
                       numNonFREMThetas = numNonFREMThetas,
                       numSkipOm        = numSkipOm,
                       availCov         = "all",
                       quiet            = FALSE),
    regexp = "RSE")

  expect_error(
    fremParameterTable(runno            = runno,
                     modDevDir        = modDevDir,
                     thetaNum         = 1:7,
                     omegaNum         = 1:5,
                     sigmaNum         = 1:2,
                     thetaLabels      = "Test",
                     numNonFREMThetas = numNonFREMThetas,
                     numSkipOm        = numSkipOm,
                     availCov         = "all",
                     quiet            = FALSE),
      regexp = "theta")

  expect_error(
    fremParameterTable(runno            = runno,
                       modDevDir        = modDevDir,
                       thetaNum         = 1:7,
                       omegaNum         = 1:5,
                       sigmaNum         = 1:2,
                       omegaLabels      = "Test",
                       numNonFREMThetas = numNonFREMThetas,
                       numSkipOm        = numSkipOm,
                       availCov         = "all",
                       quiet            = FALSE),
    regexp = "omega")

  expect_error(
    fremParameterTable(runno            = runno,
                       modDevDir        = modDevDir,
                       thetaNum         = 1:7,
                       omegaNum         = 1:5,
                       sigmaNum         = 1:2,
                       sigmaLabels      = "Test",
                       numNonFREMThetas = numNonFREMThetas,
                       numSkipOm        = numSkipOm,
                       availCov         = "all",
                       quiet            = FALSE),
    regexp = "sigma")

  expect_error(fremParameterTable(runno            = runno,
                                  modDevDir        = modDevDir,
                                  thetaNum         = 1:7,
                                  omegaNum         = 1:5,
                                  sigmaNum         = 1:2,
                                  numNonFREMThetas = numNonFREMThetas,
                                  numSkipOm        = numSkipOm,
                                  availCov         = NULL,
                                  quiet            = FALSE),
               "availCov must be part")

  expect_error(fremParameterTable(runno            = runno,
                                  modDevDir        = modDevDir,
                                  thetaNum         = 1:7,
                                  omegaNum         = 1:5,
                                  sigmaNum         = 1:2,
                                  numNonFREMThetas = numNonFREMThetas,
                                  numSkipOm        = numSkipOm,
                                  availCov         = "",
                                  quiet            = FALSE),
               "availCov must be part")

  expect_error(fremParameterTable(runno            = runno,
                                  modDevDir        = modDevDir,
                                  thetaNum         = 1:7,
                                  omegaNum         = 1:5,
                                  sigmaNum         = 1:2,
                                  numNonFREMThetas = numNonFREMThetas,
                                  numSkipOm        = numSkipOm,
                                  availCov         = "NotPart of the Model",
                                  quiet            = FALSE),
               "availCov must be part")


  expect_error(fremParameterTable(runno            = runno,
                                  modDevDir        = modDevDir,
                                  thetaNum         = 2:7,
                                  omegaNum         = c(1,3,4,5),
                                  sigmaNum         = 1,
                                  thetaLabels      = c("CL (L/h)","V (L)","MAT (h)","D1 (h)","Food on Frel","Food on MAT"),
                                  omegaLabels      = c("IIV on RUV","IIV on CL","IIV on V","IIV on MAT"),
                                  sigmaLabels      = c("RUV"),
                                  includeRSE       = TRUE,
                                  bsFile           = "test",
                                  numNonFREMThetas = numNonFREMThetas,
                                  numSkipOm        = numSkipOm,
                                  availCov         = "all",
                                  quiet            = TRUE),
               "RSE calculations")


  ## Test output

  # All covariates
  expect_snapshot(fremParameterTable(runno = runno,
                     modDevDir             = modDevDir,
                     thetaNum              = 1:7,
                     omegaNum              = 1:5,
                     sigmaNum              = 1:2,
                     numNonFREMThetas      = numNonFREMThetas,
                     numSkipOm             = numSkipOm,
                     availCov              = "all",
                     quiet                 = TRUE))

  # Selection of covariates
  expect_snapshot(fremParameterTable(runno= runno,
                     modDevDir            = modDevDir,
                     thetaNum             = 1:7,
                     omegaNum             = 1:5,
                     sigmaNum             = 1:2,
                     numNonFREMThetas     = numNonFREMThetas,
                     numSkipOm            = numSkipOm,
                     availCov             = c("SEX","WT"),
                     quiet                = TRUE))

  # Selecting covariates
  expect_snapshot(
    fremParameterTable(runno            = runno,
                       modDevDir        = modDevDir,
                       thetaNum         = 2:6,
                       omegaNum         = c(1,3,4,5),
                       sigmaNum         = 1,
                       numNonFREMThetas = numNonFREMThetas,
                       numSkipOm        = numSkipOm,
                       availCov         = "all",
                       quiet            = TRUE)
  )

  # Specifying parameter labels
  expect_snapshot(
    fremParameterTable(runno            = runno,
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
                       quiet            = TRUE)
  )

  # RSE output
  expect_snapshot(
    fremParameterTable(runno          = runno,
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
                     quiet            = TRUE)
)

  # RSE output with bsfile
  expect_snapshot(
    fremParameterTable(runno            = runno,
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
                       quiet            = TRUE)
  )

  # No CV output
  expect_snapshot(
    fremParameterTable(runno            = runno,
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
                       quiet            = TRUE)
  )
})
