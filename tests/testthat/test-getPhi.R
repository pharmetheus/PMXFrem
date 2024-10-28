test_that("getPhi works properly", {
  phiData <- getPhi(system.file("extdata","SimNeb/run30.phi",package="PMXFrem"))
  expect_type(phiData,"list")

  expect_warning(getPhi(system.file("extdata","SimNeb/CopyOfrun30.phi",package="PMXFrem")))

})
