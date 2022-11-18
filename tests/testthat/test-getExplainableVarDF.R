test_that("R/getExplainableVarDF works", {
#Add test here
  
})

library(forcats)

modFile <- system.file("extdata/SimVal/run12.mod", package = "PMXFrem")
covNames<-getCovNames(modFile = modFile)

dfData <- read.csv(system.file("extdata/SimVal/DAT-1-MI-PMX-2.csv", package = "PMXFrem"))
dfData <- subset(dfData, TYPE == 2 & BLQ == 0 & TSLD < 100)
dfData <- subset(dfData, !duplicated(ID))
# dfData$WT<--99
dfCovs <- dfData[1, (names(dfData) %in% covNames$orgCovNames)]
# dfCovs$WT<-75
dfCovs <- rbind(dfCovs, dfCovs, dfCovs, dfCovs, dfCovs, dfCovs)
dfCovs[2, names(dfCovs) != "WT"] <- -99
dfCovs[3, names(dfCovs) != "BMI"] <- -99
dfCovs[4, names(dfCovs) != "CRCL"] <- -99
dfCovs[5, (names(dfCovs) != "CRCL" & names(dfCovs) != "BMI")] <- -99
dfCovs[6, (names(dfCovs) != "CRCL" & names(dfCovs) != "WT")] <- -99
cstrCovariates<-c("All covs","Only WT","Only BMI","Only CRCL","BMI & CRCL", "WT & CRCL")

paramFunc <- function(basethetas, covthetas, dfrow, etas, ...) {
  # CL
  CLWT <- 1
  CL   <- basethetas[2] * CLWT
  return(CL*exp(covthetas[2]+etas[4]))
}

paramFunc2 <- function(basethetas, covthetas, dfrow, etas, ...) {
  # V
  V    <- basethetas[3]
  return(V * exp(covthetas[3] + etas[5]))
}


dfres0<-getExplainableVarDF(type=0,data=NULL,dfCovs = dfCovs,
                            numNonFREMThetas =7,numSkipOm=2,functionList=list(paramFunc,paramFunc2),
                            functionListName=c("CL","V"),cstrCovariates=cstrCovariates,
                            modDevDir = system.file("extdata/SimVal",package="PMXFrem"),
                            runno = 12, ncores = 1,quiet=TRUE,seed=123)

dfres1<-getExplainableVarDF(type=1,data=dfData,dfCovs = dfCovs,
                            numNonFREMThetas =7,numSkipOm=2,functionList=list(paramFunc,paramFunc2),
                            functionListName=c("CL","V"),cstrCovariates=cstrCovariates,
                            modDevDir = system.file("extdata/SimVal",package="PMXFrem"),
                            runno = 12, ncores = 10,quiet=TRUE,seed=123)

dfres2<-getExplainableVarDF(type=2,data=dfData,dfCovs = dfCovs,
                           numNonFREMThetas =7,numSkipOm=2,functionList=list(paramFunc,paramFunc2),
                           functionListName=c("CL","V"),cstrCovariates=cstrCovariates,
                           modDevDir = system.file("extdata/SimVal",package="PMXFrem"),
                           runno = 12,ncores = 10,numETASamples = 100,quiet=TRUE,seed=123)

dfres3<-getExplainableVarDF(type=3,data=dfData,dfCovs = dfCovs,
                           numNonFREMThetas =7,numSkipOm=2,functionList=list(paramFunc,paramFunc2),
                           functionListName=c("CL","V"),cstrCovariates=cstrCovariates,
                           modDevDir = system.file("extdata/SimVal",package="PMXFrem"),
                           runno = 12,ncores = 10,numETASamples = 100,quiet=TRUE,seed=123)


### plot functionality
dfres0$TYPE="Delta rule"
dfres1$TYPE="EBE based"
dfres2$TYPE="ETA based, using FFEM covariates"
dfres3$TYPE="ETA based, assuming no FFEM covariates"

dfres<-rbind(dfres0,dfres1,dfres2,dfres3)

#Sort according to dfCovs
dfres$COVNAME <- fct_reorder(dfres$COVNAME, -dfres$COVNUM)

p1<-ggplot(data=dfres,aes(x=COVVAR/TOTVAR*100,y=COVNAME,fill=TYPE))
p1<-p1+ geom_bar(position="dodge", stat="identity")
p1<-p1+facet_wrap(~PARAMETER,scales = "free_x")
p1<-p1+xlab("Explained part of total variability (%)")+ylab("")
p1


p2<-ggplot(data=dfres,aes(x=COVVAR/TOTCOVVAR*100,y=COVNAME,fill=TYPE))
p2<-p2+ geom_bar(position="dodge", stat="identity")
p2<-p2+facet_wrap(~PARAMETER,scales = "free_x")
p2<-p2+xlab("Explained part of covariate variability (%)")+ylab("")
p2