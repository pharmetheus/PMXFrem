library(data.table)
library(tidyverse)


modDevDir <- system.file("extdata/SimNeb/",package = "PMXFrem")
fremRun <- 31
numNonFREMThetas <- 7
numSkipOm <- 2

fileNames <- getFileNames(runno=fremRun,modDevDir = modDevDir)

phis  <- getPhi(fileNames$phi) %>%
  select(starts_with("ETA"))

dfext <- getExt(fileNames$ext)

tmp <- calcFFEM(dfext=dfext,numNonFREMThetas=numNonFREMThetas,numSkipOm=numSkipOm,fremETA=as.numeric(phis[1,]))

names(tmp)
tmp$Eta_prim
