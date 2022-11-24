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
                     newDataFile      = file.path(modDevDir,"newData4.csv"))

# det(tmp$FullVars)
# det(tmpAll$FullVars)
# det(tmpAll2$FullVars)
#
# det(tmpAll3$FullVars)
#
# baseOm <- getExt(paste0(modDevDir,"/run3.ext")) %>%
#   filter(ITERATION==-1000000000) %>%
#   select(starts_with("OMEGA"))
#
# OM <- matrix(0,ncol=6,nrow=6)
# OM[upper.tri(OM,diag = TRUE)] <-
# (as.numeric(baseOm))
#
# tOM <- t(OM)
# OM[lower.tri(OM,diag = FALSE)] <- tOM[lower.tri(tOM,diag = FALSE)]
#
# det(OM)
