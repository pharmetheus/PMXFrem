test_that("calcFFEM output works", {
  
#Load calcffemtestdata object
load(file="./calcffemtestdata")

extFile <- system.file("extdata/SimVal/run9.ext",package="PMXFrem")
dfExt<-getExt(extFile =  extFile)

testout<-calcFFEM(dfExt,numNonFREMThetas = 9,numSkipOm = 2,quiet = T)

#Test coefficients
expect_equal(all(testout$Coefficients==calcffemtestdata$Coefficients),TRUE)
#Test variances
expect_equal(all(testout$FullVars==calcffemtestdata$FullVars),TRUE)
#Test covariate expressions
expect_equal(all(testout$Expr==calcffemtestdata$Expr),TRUE)

})