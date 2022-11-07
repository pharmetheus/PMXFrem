test_that("createVPC works", {

  data <- read_csv(system.file("extdata/SimVal/DAT-1-MI-PMX-2.csv", package = "PMXFrem"),show_col_types = FALSE) %>% 
    filter(BLQ!=1) %>% 
    filter(TYPE!=1) %>% 
    filter(TSLD<100)
  
  
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
