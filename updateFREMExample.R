library(data.table)
library(tidyverse)
library(pmxvpc)
library(tools)

modDevDir <- "/PMX/Projects/Pharmetheus/PMX-FREMcourse-RnD-1/Analysis/Model/"

fremRun <- 31

updateFREM(
  strFREMModel      = system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"),
  strFREMData       = system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem"),
  strFFEMData       = system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"),
  cstrRemoveCov     = c("SEX"),
  cstrCatCovsToAdd  = NULL,
  cstrContCovsToAdd = NULL,
  strID             = "ID",
  strNewFREMData    = "frem_dataset_noSEX.csv",
  numNonFREMThetas  = 7,
  numSkipOm         = 2,
  bWriteData        = TRUE,
  quiet             = F,
  bWriteFIX         = TRUE,
  sortAGE           = FALSE,
  cstrKeepCols = c("ID","TAD","AMT","EVID","RATE","DV","FREMTYPE"))


updateFREM(
  strFREMModel      = system.file("extdata/SimNeb/run31_new.mod", package = "PMXFrem"),
  strFREMData       = system.file("extdata/SimNeb/frem_dataset_noSEX.csv", package = "PMXFrem"),
  strFFEMData       = system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"),
  cstrRemoveCov     = NULL,
  cstrCatCovsToAdd  = "SEX",
  cstrContCovsToAdd = NULL,
  strID             = "ID",
  strNewFREMData    = "frem_dataset_withSEX.csv",
  numNonFREMThetas  = 7,
  numSkipOm         = 2,
  bWriteData        = FALSE,
  quiet             = F,
  bWriteFIX         = TRUE,
  sortAGE           = FALSE,
  cstrKeepCols = c("ID","TAD","AMT","EVID","RATE","DV","FREMTYPE"))

# ## The object tmp is a list and contains the Omega', names of the new columns for $INP and the name of the new data file.
# tmp <- createFFEMdata(runno            = fremRun,
#                      modDevDir        = modDevDir,
#                      numNonFREMThetas = 7,
#                      numSkipOm        = 2,
#                      #availCov         = NULL,
#                      parNames         = c("CL","V","MAT"),
#                      dataFile         = "/PMX/Projects/Pharmetheus/PMX-FREMcourse-RnD-1/Analysis/ProducedData/Dataset/DAT-2-MI-PMX-2-onlyTYPE2-new.csv",
#                      newDataFile      = NULL)
#
#
# ffemMod <- createFFEMmodel(runno            = 31,
#                            modDevDir        = modDevDir,
#                            numNonFREMThetas = 7,
#                            numSkipOm        = 2,
#                            parNames         = c("CL","V","MAT"),
#                            dataFile         = "/PMX/Projects/Pharmetheus/PMX-FREMcourse-RnD-1/Analysis/ProducedData/Dataset/DAT-2-MI-PMX-2-onlyTYPE2-new.csv",
#                            newDataFile      = "testFileName.csv",
#                            quiet            = FALSE,
#                            baserunno        = 30)

