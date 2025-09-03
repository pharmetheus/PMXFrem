test_that("getExplainedVar works on main paths", {

  modDevDir <- system.file("extdata/SimNeb",package="PMXFrem")
  fremRunno <- 31
  modFile   <- file.path(modDevDir,paste0("run",fremRunno,".mod"))
  covNames  <- getCovNames(modFile = modFile)

  ## Set up dfCovs
  dfData <- read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")) %>%
    dplyr::filter(BLQ == 0) %>%
    dplyr::distinct(ID,.keep_all = T)

  dfCovs <- setupdfCovs(modFile)

  cstrCovariates <- c("All",names(dfCovs))

  ## The parameter function list
  functionList2 <- list(
    function(basethetas,covthetas, dfrow, etas, ...){ return(basethetas[2]*exp(covthetas[1] + etas[3]))},
    function(basethetas,covthetas, dfrow, etas, ...){ return(basethetas[3]*exp(covthetas[2] + etas[4]))}
  )

  ## The function that returns a list of return values
  functionList22 <- function(basethetas,covthetas, dfrow, etas, ...) {
    return(
      c(basethetas[2]*exp(covthetas[1] + etas[3]),
        basethetas[3]*exp(covthetas[2] + etas[4]))
    )
  }
  functionListName2 <- c("CL","V")

  dfres0 <- getExplainedVar(type             = 0,
                            data             = NULL,
                            dfCovs           = dfCovs,
                            numNonFREMThetas = 7,
                            numSkipOm        = 2,
                            functionList     = functionList2,
                            functionListName = functionListName2,
                            cstrCovariates   = cstrCovariates,
                            modDevDir        = modDevDir,
                            runno            = fremRunno,
                            ncores           = 1,
                            quiet            = TRUE,
                            seed             = 123
  )
  expect_snapshot_value(stabilize(as.data.frame(dfres0)), style = "serialize")

  ## Test that the delta rule can handle 2 return values
  dfres02 <- getExplainedVar(type             = 0,
                             data             = NULL,
                             dfCovs           = dfCovs,
                             numNonFREMThetas = 7,
                             numSkipOm        = 2,
                             functionList     = list(functionList22),
                             functionListName = functionListName2,
                             cstrCovariates   = cstrCovariates,
                             modDevDir        = modDevDir,
                             runno            = fremRunno,
                             ncores           = 1,
                             quiet            = TRUE,
                             seed             = 123
  )
  expect_snapshot_value(stabilize(as.data.frame(dfres02)), style = "serialize")


  dfres1 <- getExplainedVar(type             = 1,
                            data             = dfData,
                            dfCovs           = dfCovs,
                            numNonFREMThetas = 7,
                            numSkipOm        = 2,
                            functionList     = functionList2,
                            functionListName = functionListName2,
                            cstrCovariates   = cstrCovariates,
                            modDevDir        = modDevDir,
                            runno            = fremRunno,
                            ncores           = 10,
                            quiet            = TRUE,
                            seed             = 123
  )
  expect_snapshot_value(stabilize(as.data.frame(dfres1)), style = "serialize")

  ## Check that you can base the calculations on a subset of the covariates
  dfres1a <- getExplainedVar(type             = 1,
                             data             = dfData,
                             dfCovs           = dfCovs %>% dplyr::select(AGE,WT) %>% dplyr::slice(1,2,17),
                             numNonFREMThetas = 7,
                             numSkipOm        = 2,
                             functionList     = functionList2,
                             functionListName = functionListName2,
                             cstrCovariates   = c("ALL","AGE","WT"),
                             modDevDir        = modDevDir,
                             runno            = fremRunno,
                             availCov         = c("AGE","WT"),
                             ncores           = 1,
                             quiet            = TRUE,
                             seed             = 123
  )
  expect_snapshot_value(stabilize(as.data.frame(dfres1a)), style = "serialize")
  expect_gt(dfres1 %>% dplyr::select(TOTCOVVAR) %>% dplyr::slice(1),dfres1a %>% dplyr::select(TOTCOVVAR) %>% dplyr::slice(1))
  val1 <- dfres1 %>% dplyr::select(TOTVAR) %>% dplyr::slice(1)
  val2 <- dfres1a %>% dplyr::select(TOTVAR) %>% dplyr::slice(1)
  expect_equal(val1,val2)


  ## Check that you can base the calculations on one covariate
  dfres1b <- getExplainedVar(type             = 1,
                             data             = dfData,
                             dfCovs           = dfCovs %>% dplyr::select(AGE) %>% dplyr::slice(1,2),
                             numNonFREMThetas = 7,
                             numSkipOm        = 2,
                             functionList     = functionList2,
                             functionListName = functionListName2,
                             cstrCovariates   = c("ALL","AGE"),
                             modDevDir        = modDevDir,
                             runno            = fremRunno,
                             availCov         = c("AGE"),
                             ncores           = 1,
                             quiet            = TRUE,
                             seed             = 123
  )
  expect_snapshot_value(stabilize(as.data.frame(dfres1b)), style = "serialize")
  val1 <- as.numeric(dfres1b %>% dplyr::select(TOTCOVVAR) %>% dplyr::slice(1))
  val2 <- as.numeric(dfres1b %>% dplyr::select(COVVAR) %>% dplyr::slice(1))
  expect_equal(val1,val2)

  # THIS IS THE FIX: Wrap the call in expect_warning()
  expect_warning(
    dfres2 <- getExplainedVar(type             = 2,
                              data             = dfData,
                              dfCovs           = dfCovs,
                              numNonFREMThetas = 7,
                              numSkipOm        = 2,
                              functionList     = functionList2,
                              functionListName = functionListName2,
                              cstrCovariates   = cstrCovariates,
                              modDevDir        = modDevDir,
                              runno            = fremRunno,
                              ncores           = 10,
                              numETASamples    = 10,
                              quiet            = TRUE,
                              seed             = 123
    ),
    regexp = "Presence of FFEM covariates is indicated"
  )
  expect_snapshot_value(stabilize(as.data.frame(dfres2)), style = "serialize")

  dfres3 <- getExplainedVar(type             = 3,
                            data             = dfData,
                            dfCovs           = dfCovs,
                            numNonFREMThetas = 7,
                            numSkipOm        = 2,
                            functionList     = functionList2,
                            functionListName = functionListName2,
                            cstrCovariates   = cstrCovariates,
                            modDevDir        = modDevDir,
                            runno            = fremRunno,
                            ncores           = 10,
                            numETASamples    = 10,
                            quiet            = TRUE,
                            seed             = 123
  )
  expect_snapshot_value(stabilize(as.data.frame(dfres3)), style = "serialize")
})



# This block of tests is already robust and does not use snapshots. No changes needed.
test_that("getExplainedVar input checks and edge cases", {

  modDevDir <- system.file("extdata/SimNeb",package="PMXFrem")
  fremRunno <- 31
  modFile   <- file.path(modDevDir,paste0("run",fremRunno,".mod"))

  dfData <- read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")) %>%
    dplyr::filter(BLQ == 0) %>%
    dplyr::distinct(ID,.keep_all = T)

  dfCovs <- setupdfCovs(modFile)

  # Test error: type > 0 but data is missing
  expect_error(
    getExplainedVar(type = 1, data = NULL, dfCovs = dfCovs, numNonFREMThetas = 7,
                    runno = fremRunno, modDevDir = modDevDir),
    regexp = "data can not be missing with type 1-3"
  )

  # Test error: cstrCovariates length does not match nrow(dfCovs)
  expect_error(
    getExplainedVar(type = 0, dfCovs = dfCovs, cstrCovariates = "wrong_length", numNonFREMThetas = 7,
                    runno = fremRunno, modDevDir = modDevDir),
    regexp = "must have the same length as the number of rows"
  )

  # Test error: number of etas does not match number of subjects for type = 1
  expect_error(
    getExplainedVar(type = 1, data = dfData, etas = dfData[1:10,], dfCovs = dfCovs, numNonFREMThetas = 7,
                    runno = fremRunno, modDevDir = modDevDir),
    regexp = "number of etas should be the same as the number of subjects"
  )

  # Test warning: for type=2, check for presence of FFEM covariates in dfCovs
  expect_warning(
    getExplainedVar(type = 2, data = dfData, dfCovs = dfCovs[1:5,], numNonFREMThetas = 7, numSkipOm = 2,
                    runno = fremRunno, quiet = TRUE, seed = 123, numETASamples = 10, modDevDir = modDevDir),
    regexp = "Presence of FFEM covariates is indicated"
  )

  # Test verbose output with quiet = FALSE
  expect_output(
    getExplainedVar(type = 0, dfCovs = dfCovs, numNonFREMThetas = 7, runno = fremRunno,
                    quiet = FALSE, modDevDir = modDevDir)
  )

  # Test providing etas directly as an argument for type = 1
  phiFile <- system.file("extdata/SimNeb/run31.phi", package = "PMXFrem")
  etas_from_file <- getPhi(phiFile)[, 3:9]

  data_subset <- dfData[1:nrow(etas_from_file),]

  res_with_etas <- getExplainedVar(type = 1, data = data_subset, etas = etas_from_file, dfCovs = dfCovs,
                                   numNonFREMThetas = 7, numSkipOm = 2, runno = fremRunno,
                                   quiet = TRUE, modDevDir = modDevDir)

  expect_s3_class(res_with_etas, "data.frame")
  expect_gt(nrow(res_with_etas), 0)

})
