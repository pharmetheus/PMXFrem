test_that("createVPC works", {

  ## Check with specified parameter names
  vpcData <- createVPCdata(modName          = "run9",
                           modDevDir        = system.file("extdata/SimVal/", package = "PMXFrem"),
                           parNames         = c("FREL","CL","HLKON","V","MAT"),
                           numNonFREMThetas = 9,
                           numSkipOm        = 2,
                           dataFile         = data,
                           newDataFile      = NULL,
                           quiet            = TRUE)
  
  expect_snapshot_value(vpcData,style = "deparse")
  
  ## Check without specified parameter names
  vpcData2 <- createVPCdata(modName          = "run9",
                            modDevDir        = system.file("extdata/SimVal/", package = "PMXFrem"),
                            numNonFREMThetas = 9,
                            numSkipOm        = 2,
                            dataFile         = data,
                            newDataFile      = NULL,
                            quiet            = TRUE)

  expect_snapshot_value(vpcData2,style = "deparse")  
})
