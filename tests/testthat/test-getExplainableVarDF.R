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


covNames<-getCovNames(modFile = modFile)
dfCovs<-PMXForest::createInputForestData(list("AGE"=c(30,44.3028,95),SEX=c(1,2)))
# (modFile)

dfCovs<-data.frame(AGE=c(30,-99,30),SEX=c(-99,2,2))
#dfData<-read.csv(system.file("extdata/SimVal/Frem8.dir/frem_dataset.dta",package="PMXFrem"))
dfData<-read.csv(system.file("extdata/SimVal/DAT-1-MI-PMX-2.csv",package="PMXFrem"))
dfData<-subset(dfData,TYPE==2 & BLQ==0 & TSLD<100)
dfData<-subset(dfData,!duplicated(ID))

paramFunc<-function(basethetas,covthetas,dfrow,etas,...){
  #CL

  CLWT=1
  if (any(names(dfrow)=="WT") && dfrow$WT!=-99) CLWT    = (dfrow$WT/75)^basethetas[1]
  CL<-basethetas[4] * CLWT
  #if (etas[4]==0) return(exp(log(CL)+covthetas[2]))
  return(CL*exp(covthetas[2]+etas[4]))
}

getExplainableVarDF(type=3,data=dfData,dfCovs = dfCovs,dfext=dfExt,
                    numNonFREMThetas =9,numSkipOm=2,functionList=list(paramFunc),
                    functionListName="CL",cstrCovariates=c("AGE","SEX","AGE and SEX"),
                    modDevDir = "C:/PMX/github/PMXFrem/inst/extdata/SimVal/",
                    runno = 9, covNames = covNames$covNames,
                    ncores = 1,numETASamples = 100,etas=dfPhi)
