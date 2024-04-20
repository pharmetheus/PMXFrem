library(data.table)
library(tidyverse)


modDevDir <- system.file("extdata/SimNeb/",package = "PMXFrem")
fremRun <- 31
numNonFREMThetas <- 7
numSkipOm <- 2

fread(file.path(modDevDir,"DAT-2-MI-PMX-2-onlyTYPE2-new.csv"),data.table=FALSE) %>%
  distinct(GENO2)

fread(file.path(modDevDir,"frem_dataset.dta"),data.table=FALSE) %>%
  filter(FREMTYPE==1600) %>%
  distinct(DV)

fileNames <- getFileNames(runno=fremRun,modDevDir = modDevDir)

phis  <- getPhi(fileNames$phi) %>%
  select(starts_with("ETA"))

dfext <- getExt(fileNames$ext)

getCovNames(fileNames$mod)

myCovs <- getCovNames(modFile = modFile)$orgCovNames[!(getCovNames(modFile = modFile)$orgCovNames %in%
                                                         c("AST","HT","ALT","BILI","RACEL",
                                                           "ETHNIC","AGE","SMOK","LBWT","NCIL"))]
tmp <- calcFFEM(dfext            = dfext,
                numNonFREMThetas = numNonFREMThetas,
                numSkipOm        = numSkipOm,
                covNames = getCovNames(fileNames$mod)$covNames,
                availCov = myCovs)



## Explained variability
#
# modDevDir <- system.file("extdata/SimNeb",package="PMXFrem")
# fremRunno <- 31
modFile   <- file.path(modDevDir,paste0("run",fremRun,".mod"))
# covNames  <- getCovNames(modFile = modFile)

## Set up dfCovs
dfData <- read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")) %>%
  filter(BLQ == 0) %>%
  distinct(ID,.keep_all = T)

myCovs <- getCovNames(modFile = modFile)$orgCovNames[!(getCovNames(modFile = modFile)$orgCovNames %in%
                                                         c("AST","HT","ALT","BILI","RACEL",
                                                           "ETHNIC","AGE","SMOK","LBWT","NCIL"))]

dfCovs <- setupdfCovs(modFile,fremCovs=myCovs)

dfCovs <- dfCovs %>%  mutate_all(function(x) return(ifelse(x==-99,1,-99)))
dfCovs[1,] <- 1
cstrCovariates <- c("All",paste("No",names(dfCovs)))

# dfCovs[nrow(dfCovs)+1,] <- 1
# dfCovs[nrow(dfCovs),names(dfCovs) == "GENO2"] <- -99
#
# cstrCovariates <- c("All",names(dfCovs),"No GENO2")

## The parameter function list
# functionList2 <- list(
#   CL = function(basethetas,covthetas, dfrow, etas, ...){ return(basethetas[2]*exp(covthetas[1] + etas[3]))},
#   V = function(basethetas,covthetas, dfrow, etas, ...){ return(basethetas[3]*exp(covthetas[2] + etas[4]))},
#   MAT = function(basethetas,covthetas, dfrow, etas, ...){ return(basethetas[3]*exp(covthetas[3] + etas[5]))}
# )

paramFunction <- function(basethetas,covthetas, dfrow, etas, ...) {
  CL  <- basethetas[2]*exp(covthetas[1] + etas[3])
  V   <- basethetas[3]*exp(covthetas[2] + etas[4])
  MAT <- basethetas[4]*exp(covthetas[3] + etas[5])
  D1  <- basethetas[5]

  KA <- 1 / (MAT-D1)
  KE <- CL/V

  Tmax <- log(KA/KE)/(KA-KE)
  Cmax <- (KA*1/(V*(KA-KE))) * (exp(-1*KE*Tmax) - exp(-1*KA*Tmax))
  return(list(CL,V,Cmax))

}
functionListName2 <- c("CL","V","Cmax")

dfres1 <- getExplainedVar(type             = 3,
                          data             = dfData,
                          dfCovs           = dfCovs,
                          numNonFREMThetas = 7,
                          numSkipOm        = 2,
                          functionList     = list(paramFunction),
                          functionListName = functionListName2,
                          cstrCovariates   = cstrCovariates,
                          modDevDir        = modDevDir,
                          runno            = fremRun,
                          ncores           = 10,
                          quiet            = TRUE,
                          seed             = 123
)

plotExplainedVar(dfres1,maxVar = 1,parameters = "Cmax") + scale_x_continuous(breaks=seq(0,100,by=10),limits = c(NA,100))

dfData %>% select(starts_with(cstrCovariates)) %>% mutate(GENO2 = ifelse(GENO2==-99,NA,GENO2)) %>%
  cor(.,use="pair")
