test_that("createVPC works", {

  data <- read_csv(system.file("extdata/SimVal/DAT-1-MI-PMX-2.csv", package = "PMXFrem"),show_col_types = FALSE) %>%
    filter(BLQ!=1) %>%
    filter(TYPE!=1) %>%
    filter(TSLD<100)


  ## Check with specified parameter names
  vpcData <- createVPCdata(modName          = "run9",
                           modDevDir        = system.file("extdata/SimVal/", package = "PMXFrem"),
                           parNames         = c("FREL","CL","V","MAT"),
                           numNonFREMThetas = 9,
                           numSkipOm        = 2,
                           dataFile         = data,
                           newDataFile      = NULL,
                           quiet            = TRUE)

  expect_equal(class(vpcData),"list")
  expect_equal(length(vpcData),4)
  expect_equal(class(vpcData$Omega)[1],"matrix")
  expect_equal(class(vpcData$Coefficients)[1],"matrix")
  expect_equal(class(vpcData$indCovEff),"character")
  expect_equal(length(vpcData$indCovEff),4)
  expect_equal(class(vpcData$newData)[1],"tbl_df")

  expect_snapshot_value(vpcData,style = "deparse")

  ## Check without specified parameter names
  vpcData2 <- createVPCdata(modName          = "run9",
                            modDevDir        = system.file("extdata/SimVal/", package = "PMXFrem"),
                            numNonFREMThetas = 9,
                            numSkipOm        = 2,
                            dataFile         = data,
                            newDataFile      = NULL,
                            quiet            = TRUE)

  expect_equal(class(vpcData2),"list")
  expect_equal(length(vpcData2),4)
  expect_equal(class(vpcData2$Omega)[1],"matrix")
  expect_equal(class(vpcData2$Coefficients)[1],"matrix")
  expect_equal(class(vpcData2$indCovEff),"character")
  expect_equal(length(vpcData2$indCovEff),4)
  expect_snapshot_value(vpcData2,style = "deparse")
})
