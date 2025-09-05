test_that("getForestDFFREM works", {

  runno   <- "22-3"

  extFile <- system.file("extdata",paste0("SimVal/run",runno,".ext"),package="PMXForest")
  covFile <- system.file("extdata",paste0("SimVal/run",runno,".cov"),package="PMXForest")
  modFile <- system.file("extdata",paste0("SimVal/run",runno,".mod"),package="PMXForest")
  datFile <- system.file("extdata",paste0("SimVal/DAT-1-MI-PMX-2.csv"),package="PMXForest")

  dfData <- read.csv(datFile)

  covNames  <- getCovNames(modFile = modFile)

  dfCovs <- PMXForest::createInputForestData(PMXForest::getCovStats(dfData,covNames$orgCovNames,probs=c(0.05,0.95)))

  paramFun <- function(basethetas,covthetas, dfrow, ...) {
    CL <- basethetas[1]*exp(covthetas[1])
    V <-  basethetas[2]*exp(covthetas[2])
    AUC <- 5/CL

    return(c(CL, V, AUC))
  }

  functionListName2 <- c("CL","V","AUC")

  set.seed(123)
  dfSamplesCOV     <- PMXForest::getSamples(covFile,extFile=extFile,n=25)

  r_version_variant <- paste(R.version$major, R.version$minor, sep = ".")

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
    cstrPackages     = c("PMXFrem","dplyr")
  ))

  expect_snapshot_value(stabilize(dfresFREM), variant = r_version_variant, style = "serialize")

  covlabels  <- c("Age 25 y","Age 61 y","ALT 14 IU","ALT 43 IU", "AST 15 IU","AST 34 IU",
                  "Bilirubin 5 µmol/L", "Bilirubin 15 µmol/L", "BMI 23 kg/m^2","BMI 39 kg/m^2",
                  "CRCL 83 mL/min","CRCL 150 mL/min", "Caucasian","Other",
                  "HT 152 cm","HT 185 cm",
                  "NCI=0","NCI>0","White","Other",
                  "Male","Female")

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
    cstrPackages     = c("PMXFrem","dplyr")
  ))

  expect_snapshot_value(stabilize(dfresFREM2), variant = r_version_variant, style = "serialize")
})


test_that("getForestDFFREM covers edge cases", {

  # THIS IS THE FIX: Added the complete setup block to this test
  runno   <- "22-3"
  extFile <- system.file("extdata",paste0("SimVal/run",runno,".ext"),package="PMXForest")
  covFile <- system.file("extdata",paste0("SimVal/run",runno,".cov"),package="PMXForest")
  modFile <- system.file("extdata",paste0("SimVal/run",runno,".mod"),package="PMXForest")
  datFile <- system.file("extdata",paste0("SimVal/DAT-1-MI-PMX-2.csv"),package="PMXForest")
  dfData <- read.csv(datFile)
  covNames  <- getCovNames(modFile = modFile)
  dfCovs <- PMXForest::createInputForestData(PMXForest::getCovStats(dfData,covNames$orgCovNames,probs=c(0.05,0.95)))
  paramFun <- function(basethetas,covthetas, dfrow, ...) {
    return(basethetas[1]*exp(covthetas[1]))
  }
  set.seed(123)
  dfSamplesCOV <- PMXForest::getSamples(covFile,extFile=extFile,n=5) # Use fewer samples
  r_version_variant <- paste(R.version$major, R.version$minor, sep = ".")

  # Suppress warnings for all calls in this block
  suppressWarnings({
    # Test case: dfCovs is just a list
    dfCovs_as_list <- as.list(dfCovs)
    res_list <- getForestDFFREM(dfCovs = dfCovs_as_list, covNames = covNames$covNames, functionList = list(paramFun),
                                numNonFREMThetas = 13, dfParameters = dfSamplesCOV, quiet = TRUE)
    expect_s3_class(res_list, "data.frame")

    # Test case: cdfCovsNames and cGrouping are NULL
    res_defaults <- getForestDFFREM(dfCovs = dfCovs, covNames = covNames$covNames, functionList = list(paramFun),
                                    numNonFREMThetas = 13, dfParameters = dfSamplesCOV,
                                    cdfCovsNames = NULL, cGrouping = NULL, quiet = TRUE)
    expect_s3_class(res_defaults, "data.frame")

    # Test case: dfRefRow is provided
    dfRefRow_single <- dfCovs[1, , drop = FALSE]
    res_ref_row <- getForestDFFREM(dfCovs = dfCovs, covNames = covNames$covNames, functionList = list(paramFun),
                                   numNonFREMThetas = 13, dfParameters = dfSamplesCOV,
                                   dfRefRow = dfRefRow_single, quiet = TRUE)
    expect_snapshot_value(stabilize(res_ref_row), variant = r_version_variant, style = "serialize")

    # Test case: dfRefRow has wrong number of rows
    dfRefRow_wrong <- dfCovs[1:2, , drop = FALSE]
    expect_error(
      getForestDFFREM(dfCovs = dfCovs, covNames = covNames$covNames, functionList = list(paramFun),
                      numNonFREMThetas = 13, dfParameters = dfSamplesCOV,
                      dfRefRow = dfRefRow_wrong, quiet = TRUE),
      regexp = "The number of reference rows"
    )

    # Test case: ncores > 1
    if (requireNamespace("doParallel", quietly = TRUE)) {
      res_parallel <- getForestDFFREM(dfCovs = dfCovs, covNames = covNames$covNames, functionList = list(paramFun),
                                      numNonFREMThetas = 13, dfParameters = dfSamplesCOV,
                                      ncores = 2, cstrPackages = c("PMXFrem", "dplyr"), quiet = TRUE)
      expect_s3_class(res_parallel, "data.frame")
    }
  }) # End of suppressWarnings
})
