PMXRenv::activate.unqualified.packages()
test_that("getParameters work", {

  expect_error(getParameters())

  # getParameters(9,
  #               modDevDir=system.file("extdata/SimVal/", package = "PMXFrem"),
  #               numNonFREMEtas = 6,
  #               numNonFREMThetas = 9,
  #               numSkipOm = 2
  #               )
})
