# PMXRenv::activate.unqualified.packages()
test_that("calcFFEM output works", {

modFile <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
extFile <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
phiFile <- system.file("extdata/SimNeb/run31.phi", package = "PMXFrem")

dfExt   <- getExt(extFile = extFile)
dfPhi   <- getPhi(phiFile = phiFile)

## Basic usage
calcFFEMtestout <-calcFFEM(dfExt,numNonFREMThetas = 7,numSkipOm = 2,quiet = T)

expect_snapshot_value(calcFFEMtestout$Coefficients,style = "deparse")
expect_snapshot_value(calcFFEMtestout$FullVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout$Expr,style = "deparse")
expect_snapshot_value(calcFFEMtestout$Vars,style = "deparse")
expect_snapshot_value(calcFFEMtestout$UpperVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout$Eta_prim,style = "deparse")

expect_snapshot_output(calcFFEM(dfExt,numNonFREMThetas = 7,numSkipOm = 2,quiet = F))

## Compute eta_prim
calcFFEMtestout2 <-calcFFEM(dfExt,numNonFREMThetas = 7,numSkipOm = 2,quiet = T,
                            etaFREM=as.numeric(dfPhi[1,]))

expect_snapshot_value(calcFFEMtestout2$Coefficients,style = "deparse")
expect_snapshot_value(calcFFEMtestout2$FullVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout2$Expr,style = "deparse")
expect_snapshot_value(calcFFEMtestout2$Vars,style = "deparse")
expect_snapshot_value(calcFFEMtestout2$UpperVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout2$Eta_prim,style = "deparse")

expect_snapshot_output(calcFFEM(dfExt,numNonFREMThetas = 7,numSkipOm = 2,quiet = F,
                                etaFREM=as.numeric(dfPhi[1,])))

## Specify availCov with named covariates
covNames <- getCovNames(modFile)$covNames
calcFFEMtestout3 <-calcFFEM(dfExt,numNonFREMThetas = 7,numSkipOm = 2,quiet = T,
                            covNames = covNames,availCov = c("SEX","WT"))

expect_snapshot_value(calcFFEMtestout3$Coefficients,style = "serialize")
expect_snapshot_value(calcFFEMtestout3$FullVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout3$Expr,style = "deparse")
expect_snapshot_value(calcFFEMtestout3$Vars,style = "deparse")
expect_snapshot_value(calcFFEMtestout3$UpperVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout3$Eta_prim,style = "deparse")

expect_snapshot_output(calcFFEM(dfExt,numNonFREMThetas = 7,numSkipOm = 2,quiet = T,
                                covNames = covNames,availCov = c("SEX","WT")))


## Specify availCov with generic covariate names
calcFFEMtestout4 <-calcFFEM(dfExt,numNonFREMThetas = 7,numSkipOm = 2,quiet = T,
                            availCov = c("Cov1","Cov2"))

expect_snapshot_value(calcFFEMtestout4$Coefficients,style = "serialize")
expect_snapshot_value(calcFFEMtestout4$FullVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout4$Expr,style = "deparse")
expect_snapshot_value(calcFFEMtestout4$Vars,style = "deparse")
expect_snapshot_value(calcFFEMtestout4$UpperVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout4$Eta_prim,style = "deparse")

expect_snapshot_output(calcFFEM(dfExt,numNonFREMThetas = 7,numSkipOm = 2,quiet = T,
                                availCov = c("Cov1","Cov2")))



})
