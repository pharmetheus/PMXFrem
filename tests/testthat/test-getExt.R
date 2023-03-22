PMXRenv::activate.unqualified.packages()
test_that("Ext files are read properly", {
  extData <- getExt(system.file("extdata","SimNeb/run30.ext",package="PMXFrem"))
  expect_type(extData,"list")
})
