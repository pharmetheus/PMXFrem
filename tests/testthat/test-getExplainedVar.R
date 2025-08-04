
test_that("getExplainedVar works", {

modDevDir <- system.file("extdata/SimNeb",package="PMXFrem")
fremRunno <- 31
modFile   <- file.path(modDevDir,paste0("run",fremRunno,".mod"))
covNames  <- getCovNames(modFile = modFile)

## Set up dfCovs
dfData <- read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")) %>%
  filter(BLQ == 0) %>%
  distinct(ID,.keep_all = T)

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


## Test that the inpout check for cstrCovariates work
expect_error(
dfres0 <- getExplainedVar(type             = 0,
                          data             = NULL,
                          dfCovs           = dfCovs,
                          numNonFREMThetas = 7,
                          numSkipOm        = 2,
                          functionList     = functionList2,
                          functionListName = functionListName2,
                          cstrCovariates   = c("ALL","AGE"),
                          modDevDir        = modDevDir,
                          runno            = fremRunno,
                          ncores           = 1,
                          quiet            = TRUE,
                          seed             = 123
)
)

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
expect_snapshot_value(dfres0,style = "deparse")

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
expect_snapshot_value(dfres02,style = "deparse")


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

expect_snapshot_value(dfres1,style = "deparse")

## Check that you can base the calculations on a subset of the covariates
dfres1a <- getExplainedVar(type             = 1,
                          data             = dfData,
                          dfCovs           = dfCovs %>% select(AGE,WT) %>% slice(1,2,17),
                          numNonFREMThetas = 7,
                          numSkipOm        = 2,
                          functionList     = functionList2,
                          functionListName = functionListName2,
                          cstrCovariates   = c("ALL","AGE","WT"),#cstrCovariates,
                          modDevDir        = modDevDir,
                          runno            = fremRunno,
                          availCov         = c("AGE","WT"),
                          ncores           = 1,
                          quiet            = TRUE,
                          seed             = 123
)

expect_snapshot_value(dfres1a,style = "deparse")
expect_gt(dfres1 %>% select(TOTCOVVAR) %>% slice(1),dfres1a %>% select(TOTCOVVAR) %>% slice(1))
val1 <- dfres1 %>% select(TOTVAR) %>% slice(1)
val2 <- dfres1a %>% select(TOTVAR) %>% slice(1)
expect_equal(val1,val2)


## Check that you can base the calculations on one covariate
dfres1b <- getExplainedVar(type             = 1,
                           data             = dfData,
                           dfCovs           = dfCovs %>% select(AGE) %>% slice(1,2),
                           numNonFREMThetas = 7,
                           numSkipOm        = 2,
                           functionList     = functionList2,
                           functionListName = functionListName2,
                           cstrCovariates   = c("ALL","AGE"),#cstrCovariates,
                           modDevDir        = modDevDir,
                           runno            = fremRunno,
                           availCov         = c("AGE"),
                           ncores           = 1,
                           quiet            = TRUE,
                           seed             = 123
)
expect_snapshot_value(dfres1b,style = "deparse")
val1 <- as.numeric(dfres1b %>% select(TOTCOVVAR) %>% slice(1))
val2 <- as.numeric(dfres1b %>% select(COVVAR) %>% slice(1))
expect_equal(val1,val2)

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
)

expect_snapshot_value(dfres2,style = "deparse")

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

expect_snapshot_value(dfres3,style = "deparse")

})
