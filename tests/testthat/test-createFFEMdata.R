# PMXRenv::activate.unqualified.packages()
test_that("createFFEM works", {

  data <- read_csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"),show_col_types = FALSE) %>%
    filter(BLQ!=1)

  ## Check with specified parameter names
  vpcData <- createFFEMdata(modName          = "run31",
                           modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
                           parNames         = c("CL","V","MAT"),
                           numNonFREMThetas = 7,
                           numSkipOm        = 2,
                           dataFile         = data,
                           newDataFile      = NULL,
                           quiet            = TRUE)

  expect_equal(class(vpcData),"list")
  expect_equal(length(vpcData),6)
  expect_equal(class(vpcData$Omega)[1],"matrix")
  expect_equal(class(vpcData$Coefficients)[1],"matrix")
  expect_equal(class(vpcData$indCovEff),"character")
  expect_equal(length(vpcData$indCovEff),3)
  expect_equal(class(vpcData$newData)[1],"tbl_df")

  expect_snapshot_value(vpcData,style = "deparse")

  ## Check without specified parameter names
  vpcData2 <- createFFEMdata(modName          = "run31",
                            modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
                            numNonFREMThetas = 7,
                            numSkipOm        = 2,
                            dataFile         = data,
                            newDataFile      = NULL,
                            quiet            = TRUE)

  expect_equal(class(vpcData2),"list")
  expect_equal(length(vpcData2),6)
  expect_equal(class(vpcData2$Omega)[1],"matrix")
  expect_equal(class(vpcData2$Coefficients)[1],"matrix")
  expect_equal(class(vpcData2$indCovEff),"character")
  expect_equal(length(vpcData2$indCovEff),3)
   expect_snapshot_value(vpcData2,style = "deparse")

  ## Check when availCov = "all'
  vpcData3 <- createFFEMdata(modName          = "run31",
                            modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
                            numNonFREMThetas = 7,
                            numSkipOm        = 2,
                            dataFile         = data,
                            availCov         = 'all',
                            newDataFile      = NULL,
                            quiet            = TRUE)

   expect_snapshot_value(vpcData3,style = "deparse")
})
