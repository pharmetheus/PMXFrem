test_that("calEtas works", {

  data <- readr::read_csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"),show_col_types = FALSE) %>%
    dplyr::filter(BLQ!=1)

  ## Check with specified parameter names
  vpcData <- createFFEMdata(modName          = "run31",
                            modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
                            parNames         = c("CL","V","MAT"),
                            numNonFREMThetas = 7,
                            numSkipOm        = 2,
                            dataFile         = data,
                            newDataFile      = NULL,
                            quiet            = TRUE)



  ind_params <- calcEtas(modName          = "run31",
                         modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
                         numSkipOm        = 2,
                         numNonFREMThetas = 7,
                         FFEMData         = vpcData)


  expect_equal(class(ind_params),"data.frame")
  expect_snapshot(stabilize(ind_params))
})
