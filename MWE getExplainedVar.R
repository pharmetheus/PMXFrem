# .libPaths(c(.libPaths(),"/Users/niclasj/R/R-4.2/pkgs/versioned/pmxfrem/1.1.1/"))
#
# library(PMXFrem)


PMXRenv::library.versioned("PMXFrem","1.1.1")

modDevDir <- system.file("extdata/SimNeb",package="PMXFrem")
fremRunno <- 31
modFile   <- file.path(modDevDir,paste0("run",fremRunno,".mod"))
covNames  <- getCovNames(modFile = modFile)

## Set up dfCovs
dfData <- read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")) %>%
  filter(BLQ == 0) %>%
  distinct(ID,.keep_all = T)

dfCovs <- dfData  %>%
  select(covNames$orgCovNames) %>%
  mutate_all(function(x) return(1)) %>%
  slice(rep(1,ncol(.)+1))

for(i in 2:nrow(dfCovs)) {
  dfCovs[i, names(dfCovs) != names(dfCovs)[i-1]] <- -99
}

cstrCovariates <- c("All",names(dfCovs))

## The parameter function list
functionList2 <- list(
  function(basethetas,covthetas, dfrow, etas, ...){ return(basethetas[2]*exp(covthetas[1] + etas[3]))},
  function(basethetas,covthetas, dfrow, etas, ...){ return(basethetas[3]*exp(covthetas[2] + etas[4]))}
)
functionListName2 <- c("CL","V")

## On windows, this produces an error wrt to stringr
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
                          ncores           = 10,
                          quiet            = TRUE,
                          seed             = 123
)


## On windows, this produces an error wrt to calcFFEM (which is a function in PMXFrem)
dfres0 <- getExplainedVar(type             = 1,
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
                          seed             = 123,
                          cstrPackages     = c("stringr")
)

## On windows, this produces an error wrt to not finding PMXFrem
dfres0 <- getExplainedVar(type             = 2,
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
                          seed             = 123,
                          cstrPackages     = c("stringr","PMXFrem")
)

