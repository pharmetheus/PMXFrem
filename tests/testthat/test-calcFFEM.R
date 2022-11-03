test_that("calcFFEM output works", {
  
#Load calcffemtestdata object
# load(file="./calcffemtestdata")

extFile <- system.file("extdata/SimVal/run9.ext", package = "PMXFrem")
dfExt   <- getExt(extFile = extFile)

# testout<-calcFFEM(dfExt,numNonFREMThetas = 9,numSkipOm = 2,quiet = T)
calcFFEMtestout <-calcFFEM(dfExt,numNonFREMThetas = 9,numSkipOm = 2,quiet = T)

expect_snapshot_value(calcFFEMtestout$Coefficients,style = "deparse")
expect_snapshot_value(calcFFEMtestout$FullVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout$Expr,style = "deparse")
expect_snapshot_value(calcFFEMtestout$Vars,style = "deparse")
expect_snapshot_value(calcFFEMtestout$UpperVars,style = "deparse")
expect_snapshot_value(calcFFEMtestout$Eta_prim,style = "deparse")

expect_snapshot_output(calcFFEM(dfExt,numNonFREMThetas = 9,numSkipOm = 2,quiet = F))
# #Test coefficients
# expect_equal(all(testout$Coefficients==calcffemtestdata$Coefficients),TRUE)
# #Test variances
# expect_equal(all(testout$FullVars==calcffemtestdata$FullVars),TRUE)
# #Test covariate expressions
# expect_equal(all(testout$Expr==calcffemtestdata$Expr),TRUE)

})