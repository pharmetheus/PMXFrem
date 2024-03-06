# PMXRenv::activate.unqualified.packages()
test_that("calcFFEM output works", {

extFile <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
dfExt   <- getExt(extFile = extFile)

calcFFEMtestout <-calcFFEM(dfExt,numNonFREMThetas = 7,numSkipOm = 2,quiet = T)

expect_snapshot_value(calcFFEMtestout$Coefficients,style = "deparse")
expect_snapshot_value(calcFFEMtestout$FullVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout$Expr,style = "deparse")
expect_snapshot_value(calcFFEMtestout$Vars,style = "deparse")
expect_snapshot_value(calcFFEMtestout$UpperVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout$Eta_prim,style = "deparse")

expect_snapshot_output(calcFFEM(dfExt,numNonFREMThetas = 7,numSkipOm = 2,quiet = F))


})
