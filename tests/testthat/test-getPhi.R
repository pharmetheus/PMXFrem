test_that("getPhi works properly", {
  phiData <- getExt(system.file("extdata","SimNeb/run30.phi",package="PMXFrem"))
  expect_type(phiData,"list")
})
