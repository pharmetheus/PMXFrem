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

functionList22 <- function(basethetas,covthetas, dfrow, etas, ...) {
return(list(basethetas[2]*exp(covthetas[1] + etas[3]),
            basethetas[3]*exp(covthetas[2] + etas[4])))
}

functionListFFEMCovs<-function(basethetas,covthetas, dfrow, etas, ...) {
  MATCOVTIME<-1
  if (any(names(dfrow)=="FD") && dfrow$FD !=-99 && dfrow$FD == 0) MATCOVTIME<-1+basethetas[6]

  
  MAT   = basethetas[4]*MATCOVTIME*exp(covthetas[3]  + etas[5]) 
  
  return(list(MAT,
              basethetas[2]*exp(covthetas[1] + etas[3]),
              basethetas[3]*exp(covthetas[2] + etas[4])))
}


functionListName2 <- c("MAT","CL","V")

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
dfres1 <- getExplainedVar(type             = 1,
                          data             = dfData,
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
                          seed             = 123,
                          cstrPackages     = c("stringr")
)

## On windows, this produces an error wrt to not finding PMXFrem
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
                          ncores           = 1,
                          quiet            = TRUE,
                          seed             = 123,
                          cstrPackages     = c("stringr","PMXFrem")
)


#### FFEM covariates

## On windows, this produces an error wrt to stringr
# No FOOD effect (FD==0)
dfres0 <- getExplainedVar(type             = 0,
                          data             = NULL,
                          dfCovs           = dfCovs,
                          numNonFREMThetas = 7,
                          numSkipOm        = 2,
                          functionList     = list(functionListFFEMCovs),
                          functionListName = functionListName2,
                          cstrCovariates   = cstrCovariates,
                          modDevDir        = modDevDir,
                          runno            = fremRunno,
                          ncores           = 10,
                          quiet            = TRUE,
                          seed             = 123
)

#FD=1
dfCovs1<-dfCovs
dfCovs1$FOOD<-1
dfres1 <- getExplainedVar(type             = 0,
                          data             = NULL,
                          dfCovs           = dfCovs1,
                          numNonFREMThetas = 7,
                          numSkipOm        = 2,
                          functionList     = list(functionListFFEMCovs),
                          functionListName = functionListName2,
                          cstrCovariates   = cstrCovariates,
                          modDevDir        = modDevDir,
                          runno            = fremRunno,
                          ncores           = 10,
                          quiet            = TRUE,
                          seed             = 123
)

#FD=2
dfCovs2<-dfCovs
dfCovs2$FOOD<-2
dfres2 <- getExplainedVar(type             = 0,
                          data             = NULL,
                          dfCovs           = dfCovs2,
                          numNonFREMThetas = 7,
                          numSkipOm        = 2,
                          functionList     = list(functionListFFEMCovs),
                          functionListName = functionListName2,
                          cstrCovariates   = cstrCovariates,
                          modDevDir        = modDevDir,
                          runno            = fremRunno,
                          ncores           = 10,
                          quiet            = TRUE,
                          seed             = 123
)

#All figures looks the same
plotExplainedVar(dfres0,maxVar = 2)
plotExplainedVar(dfres1,maxVar = 2)
plotExplainedVar(dfres2,maxVar = 2)

#Including Actual food effect of 0
dfCovs2<-dfCovs
dfCovs2$FD<-0
dfres2 <- getExplainedVar(type             = 0,
                          data             = NULL,
                          dfCovs           = dfCovs2,
                          numNonFREMThetas = 7,
                          numSkipOm        = 2,
                          functionList     = list(functionListFFEMCovs),
                          functionListName = functionListName2,
                          cstrCovariates   = cstrCovariates,
                          modDevDir        = modDevDir,
                          runno            = fremRunno,
                          ncores           = 10,
                          quiet            = TRUE,
                          seed             = 123
)

plotExplainedVar(dfres2,maxVar = 1)


### Type = 2


dfCovs2<-dfCovs
dfCovs2$FD<--99
dfCovs2$FD[1]<-1
arow<-dfCovs2[1,]
arow[,]<- -99
arow$FD<-1
dfCovs2<-rbind(dfCovs2,arow)
dfData$FD<-dfData$FOOD
dfres2 <- getExplainedVar(type             = 2,
                          data             = dfData,
                          dfCovs           = dfCovs2,
                          numNonFREMThetas = 7,
                          numSkipOm        = 2,
                          functionList     = list(functionListFFEMCovs),
                          functionListName = functionListName2,
                          cstrCovariates   = c(cstrCovariates,"FOOD"),
                          modDevDir        = modDevDir,
                          runno            = fremRunno,
                          ncores           = 1,
                          quiet            = TRUE,
                          seed             = 123
)

plotExplainedVar(dfres2,maxVar = 2)
plotExplainedVar(dfres2,maxVar = 1)

#Only showcase FOOD=0 or FOOD=1
### Type = 2
dfCovs2<-dfCovs
dfCovs2$FD<--99
dfCovs2$FD[1]<-1
arow<-dfCovs2[1,]
arow[,]<- -99
arow$FD<-1
dfCovs2<-rbind(dfCovs2,arow)
dfData$FD<-dfData$FOOD
dfDatatmp<-dfData
dfDatatmp$FD<-1
dfres2 <- getExplainedVar(type             = 2,
                          data             = dfDatatmp,
                          dfCovs           = dfCovs2,
                          numNonFREMThetas = 7,
                          numSkipOm        = 2,
                          functionList     = list(functionListFFEMCovs),
                          functionListName = functionListName2,
                          cstrCovariates   = c(cstrCovariates,"FOOD"),
                          modDevDir        = modDevDir,
                          runno            = fremRunno,
                          ncores           = 1,
                          quiet            = TRUE,
                          seed             = 123
)

plotExplainedVar(dfres2,maxVar = 1)
