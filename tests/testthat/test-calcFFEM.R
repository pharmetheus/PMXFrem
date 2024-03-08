# PMXRenv::activate.unqualified.packages()
test_that("calcFFEM output works", {

extFile <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
phiFile <- system.file("extdata/SimNeb/run31.phi", package = "PMXFrem")

dfExt   <- getExt(extFile = extFile)
dfPhi   <- getPhi(phiFile = phiFile)

calcFFEMtestout <-calcFFEM(dfExt,numNonFREMThetas = 7,numSkipOm = 2,quiet = T)
calcFFEMtestout2 <-calcFFEM(dfExt,numNonFREMThetas = 7,numSkipOm = 2,quiet = T,
                            etaFREM=as.numeric(dfPhi[1,]))


expect_snapshot_value(calcFFEMtestout$Coefficients,style = "deparse")
expect_snapshot_value(calcFFEMtestout$FullVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout$Expr,style = "deparse")
expect_snapshot_value(calcFFEMtestout$Vars,style = "deparse")
expect_snapshot_value(calcFFEMtestout$UpperVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout$Eta_prim,style = "deparse")

expect_snapshot_output(calcFFEM(dfExt,numNonFREMThetas = 7,numSkipOm = 2,quiet = F))

expect_snapshot_value(calcFFEMtestout2$Coefficients,style = "deparse")
expect_snapshot_value(calcFFEMtestout2$FullVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout2$Expr,style = "deparse")
expect_snapshot_value(calcFFEMtestout2$Vars,style = "deparse")
expect_snapshot_value(calcFFEMtestout2$UpperVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout2$Eta_prim,style = "deparse")

expect_snapshot_output(calcFFEM(dfExt,numNonFREMThetas = 7,numSkipOm = 2,quiet = F,
                                etaFREM=as.numeric(dfPhi[1,])))

})
