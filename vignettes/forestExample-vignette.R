## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE, warning=FALSE,message=FALSE------------------------------


library(PMXFrem) # Read in the FREMfunctions package

library(PMXForest) #Read in the forest package for showing the covariate effects

library(ggplot2) #Read in the ggplot2 package for plotting

set.seed(865765) #Set seed for reproducability



## ------------------------------------------------------------------------
#Get the Vingette paths to ext, cov and mod file
runno<-"run7"
extFile <- system.file("extdata", paste0(runno,".ext"), package = "PMXFrem")
modFile <- system.file("extdata", paste0(runno,".mod"), package = "PMXFrem")
covFile <- system.file("extdata", paste0(runno,".cov"), package = "PMXFrem")

## ------------------------------------------------------------------------

dfExt    <- subset(PMXFrem::getExt(extFile = extFile),ITERATION=="-1000000000") # Get the final parameter estimates
covNames <- PMXFrem::getCovNames(modFile = modFile)                             # Get the covariate names from the annotated FREM model file


## ------------------------------------------------------------------------

dfCovs<-data.frame(LBWT  = c(3.12,4.6,4.3,-99,-99,-99,-99,-99),
                   LBLBW = c(3.3,-99,-99,-99,-99,3.3,3.8,-99),
                   LBBSA = c(-99,-99,-99,0.56,0.75,-99,-99,-99))

print(dfCovs)

# There is also a utility function to accomplish the same if the setup is more complicated.
dfCovs <- PMXForest::createInputForestData(
  list(
    list("LBWT" = c(3.12,4.6,4.3),"LBLBW"=c(3.3)), #First 3 rows
    "LBBSA" = c(0.56,0.75),                        #Row 4,5
    "LBLBW" = c(3.3,3.8),                          #Row 6,7   
    list("LBWT"=NA,"LBBSA"=NA,"LBLBW"=NA)),        #Last row, reference row
  iMiss=-99)

print(dfCovs)

## ------------------------------------------------------------------------
#Get 100 samples from the cov file
dfParameters<-PMXForest::getSamples(covFile,extFile = extFile,n = 100)

## Set the first sample to the final estimates
dfParameters[1,]<-dfExt[1,-1]

## ------------------------------------------------------------------------
## The parameter function list
functionList <- list(
  function(basethetas,covthetas,...){ return(basethetas[1]*exp(covthetas[1]))}, 
  function(basethetas,covthetas,...){ return(basethetas[2]*exp(covthetas[2]))}
  )

## Define the names of the parameters that each of the functions in the list above returns
functionListName <- c("CL","V1")

## ---- warning=FALSE------------------------------------------------------
dfres <- PMXForest::getForestDFFREM(dfCovs = dfCovs,
                     noBaseThetas = 6,
                     noCovThetas = length(covNames$covNames),
                     noSigmas = 2,
                     dfParameters=dfParameters,covNames=covNames,
                     functionList=functionList,functionListName=functionListName,
                     quiet=TRUE)

print(dfres)

## ---- fig1,fig.height = 4, fig.width = 16, fig.align="center", out.width=700, out.height=175----
theme_set(theme_bw(base_size=16)) #Change ggplot defaults to get nice plot
PMXForest::plotForestDF(dfres,textsize=5)+xlim(c(0,2.5))

