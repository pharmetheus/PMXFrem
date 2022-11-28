library(data.table)
library(tidyverse)
library(pmxvpc)

modDevDir <- "/PMX/Projects/Pharmetheus/PMX-FREMcourse-RnD-1/Analysis/Model/"

fremRun <- 31

## The object tmp is a list and contains the Omega', names of the new columns for $INP and the name of the new data file.
tmp <- createFFEMdata(runno            = fremRun,
                     modDevDir        = modDevDir,
                     numNonFREMThetas = 7,
                     numSkipOm        = 2,
                     #availCov         = NULL,
                     parNames         = c("CL","V","MAT"),
                     dataFile         = "/PMX/Projects/Pharmetheus/PMX-FREMcourse-RnD-1/Analysis/ProducedData/Dataset/DAT-2-MI-PMX-2-onlyTYPE2-new.csv",
                     newDataFile      = NULL)


ffemMod <- createFFEMmodel(runno            = 31,
                           modDevDir        = modDevDir,
                           numNonFREMThetas = 7,
                           numSkipOm        = 2,
                           parNames         = c("CL","V","MAT"),
                           dataFile         = "/PMX/Projects/Pharmetheus/PMX-FREMcourse-RnD-1/Analysis/ProducedData/Dataset/DAT-2-MI-PMX-2-onlyTYPE2-new.csv",
                           newDataFile      = "testFileName.csv",
                           quiet            = FALSE,
                           baserunno        = 30)

