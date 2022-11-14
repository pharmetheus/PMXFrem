test_that("R/getExplainableVarDF works", {
#Add test here
  
})


# 
#                                  44.3028 ; TV_AGE
#                                  24.9468 ; TV_AST
#                                  9.65924 ; TV_BILI
#                                  117.632 ; TV_CRCL
#                                  30.145 ; TV_BMI
#                                  27.5527 ; TV_ALT
#                                  1.44045 ; TV_SEX
#                                  0.0245749 ; TV_RACEL_3
#                                  0.207003 ; TV_RACEL_2
#                                  0.0163928 ; TV_NCI_2
#                                  0.134599 ; TV_NCI_1
#                                  0.0560756 ; TV_GENO_4
#                                  0.0986099 ; TV_GENO_3
#                                  0.817952 ; TV_GENO_2
#                                  
modFile <- system.file("extdata/SimVal/run9.mod",package="PMXFrem")
extFile <- system.file("extdata/SimVal/run9.ext",package="PMXFrem")
phiFile <- system.file("extdata/SimVal/run9.phi",package="PMXFrem")
dfExt<-getExt(extFile =  extFile)
dfPhi<-getPhi(phiFile)
dfPhi<-dfPhi[,3:8] #Include the structural model  etas

modFile <- system.file("extdata/SimVal/run12.mod",package="PMXFrem")
extFile <- system.file("extdata/SimVal/run12.ext",package="PMXFrem")
phiFile <- system.file("extdata/SimVal/run12.phi",package="PMXFrem")
dfExt<-getExt(extFile =  extFile)
dfPhi<-getPhi(phiFile)
dfPhi<-dfPhi[,3:8] #Include the structural model  etas



covNames<-getCovNames(modFile = modFile)
#dfCovs<-PMXForest::createInputForestData(list("AGE"=c(30,44.3028,95),SEX=c(1,2)))
# (modFile)

dfCovs<-data.frame(AGE=c(30,-99,30),SEX=c(-99,2,2))
dfCovs<-data.frame(AGE=c(30,-99,30),SEX=c(-99,2,2),CRCL=c(75,75,75),WT=c(1,1,1))
cstrCovariates<-c("AGE CRCL WT","SEX CRCL WT","AGE, SEX CRCL WT")

#dfData<-read.csv(system.file("extdata/SimVal/Frem8.dir/frem_dataset.dta",package="PMXFrem"))
dfData<-read.csv(system.file("extdata/SimVal/DAT-1-MI-PMX-2.csv",package="PMXFrem"))
dfData<-subset(dfData,TYPE==2 & BLQ==0 & TSLD<100)
dfData<-subset(dfData,!duplicated(ID))
#dfData$WT<--99
dfCovs<-dfData[1,(names(dfData) %in% covNames$orgCovNames)]
#dfCovs$WT<-75
dfCovs<-rbind(dfCovs,dfCovs,dfCovs,dfCovs,dfCovs,dfCovs)
dfCovs[2,names(dfCovs)!="WT"]<--99
dfCovs[3,names(dfCovs)!="BMI"]<--99
dfCovs[4,names(dfCovs)!="CRCL"]<--99
dfCovs[5,(names(dfCovs)!="CRCL" & names(dfCovs)!="BMI")]<--99
dfCovs[6,(names(dfCovs)!="CRCL" & names(dfCovs)!="WT")]<--99

#dfCovs$WT<-75

#Check covariates NCI, GENO, RACE not consistent with dataset

cstrCovariates<-c("All covs","Only WT","Only BMI","Only CRCL","BMI & CRCL", "WT & CRCL")
paramFunc<-function(basethetas,covthetas,dfrow,etas,...){
  #CL
#  browser()
  CLWT=1
  #if (any(names(dfrow)=="WT") && dfrow$WT!=-99) CLWT    = (dfrow$WT/75)^basethetas[1]
  CL<-basethetas[2] * CLWT
  V<-basethetas[3]
#  return(list(CL*exp(covthetas[2]+etas[4]),V*exp(covthetas[3]+etas[5])))
  return(CL*exp(covthetas[2]+etas[4]))
}


parf<-function(x,basethetas,covthetas,dfrow,...) {
  return(unlist(paramFunc(basethetas,covthetas,dfrow,x,...)))
}

# covm<-diag(nrow=4,ncol=4,c(1.13E-01,1.00E-04,2.82E-01,1.54E-01))
# covm[3,4]<--2.96E-02
# covm[4,3]<--2.96E-02
# 
# deltarule(params=c(0,0,0,0),covmatrix = covm,transform_fun = parf,basethetas=c(1.00E+00,7.60E+00,1.48E+02),covthetas=c(0,0,0))
# 
# covm<-diag(nrow=4,ncol=4,c(0.25122896,0.12293947,0.20058422,0.14643955))
# 
# deltarule(params=c(0,0,0,0),covmatrix = covm,transform_fun = parf,basethetas=c(1.00E+00,7.60E+00,1.48E+02),covthetas=c(0,0,0))

#Remove covNames
#Only use orgCovs to specify dfCovs

dfres1<-getExplainableVarDF(type=1,data=dfData,dfCovs = dfCovs,dfext=dfExt,
                    numNonFREMThetas =7,numSkipOm=2,functionList=list(paramFunc),
                    functionListName=c("CL","V"),cstrCovariates=cstrCovariates,
                    modDevDir = "C:/PMX/github/PMXFrem/inst/extdata/SimVal/",
                    runno = 12, covNames = covNames$covNames,
                    ncores = 1,numETASamples = 100,etas=dfPhi,quiet=TRUE,seed=123)

dfres2<-getExplainableVarDF(type=2,data=dfData,dfCovs = dfCovs,dfext=dfExt,
                           numNonFREMThetas =7,numSkipOm=2,functionList=list(paramFunc),
                           functionListName=c("CL","V"),cstrCovariates=cstrCovariates,
                           modDevDir = "C:/PMX/github/PMXFrem/inst/extdata/SimVal/",
                           runno = 12, covNames = covNames$covNames,
                           ncores = 1,numETASamples = 100,etas=dfPhi,quiet=TRUE,seed=123)


dfres3<-getExplainableVarDF(type=3,data=dfData,dfCovs = dfCovs,dfext=dfExt,
                           numNonFREMThetas =7,numSkipOm=2,functionList=list(paramFunc),
                           functionListName=c("CL","V"),cstrCovariates=cstrCovariates,
                           modDevDir = "C:/PMX/github/PMXFrem/inst/extdata/SimVal/",
                           runno = 12, covNames = covNames$covNames,
                           ncores = 1,numETASamples = 100,etas=dfPhi,quiet=TRUE,seed=123)
