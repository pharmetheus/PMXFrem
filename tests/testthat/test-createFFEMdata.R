test_that("createFFEMdata works", {

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

  expect_equal(class(vpcData),"list")
  expect_equal(length(vpcData),6)
  expect_equal(class(vpcData$Omega)[1],"matrix")
  expect_equal(class(vpcData$Coefficients)[1],"matrix")
  expect_equal(class(vpcData$indCovEff),"character")
  expect_equal(length(vpcData$indCovEff),3)
  expect_equal(class(vpcData$newData)[1],"tbl_df")

  expect_snapshot_value(stabilize(as.data.frame(head(vpcData$newData, 20))), style = "serialize")

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

  expect_snapshot_value(stabilize(as.data.frame(head(vpcData2$newData, 20))), style = "serialize")

  ## Check when availCov = "all"
  vpcData3 <- createFFEMdata(modName          = "run31",
                             modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
                             numNonFREMThetas = 7,
                             numSkipOm        = 2,
                             dataFile         = data,
                             availCov         = 'all',
                             newDataFile      = NULL,
                             quiet            = TRUE)

  expect_snapshot_value(stabilize(as.data.frame(head(vpcData3$newData, 20))), style = "serialize")
})

test_that("createFFEMdata works with parallel processing", {

  data <- readr::read_csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"),show_col_types = FALSE) %>%
    dplyr::filter(BLQ!=1)

  ## Check with specified parameter names and cores > 1
  vpcData <- createFFEMdata(modName          = "run31",
                            modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
                            parNames         = c("CL","V","MAT"),
                            numNonFREMThetas = 7,
                            numSkipOm        = 2,
                            dataFile         = data,
                            newDataFile      = NULL,
                            quiet            = TRUE,
                            cores            = 2) # Trigger the parallel code path

  # The result should be numerically identical to the sequential run,
  # so a snapshot is a good way to verify correctness.
  expect_snapshot_value(stabilize(vpcData), style = "serialize")
})
test_that("createFFEMdata generates V-columns when omegaToData = TRUE and numSkipOm = 0", {
  data <- readr::read_csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"), show_col_types = FALSE) %>%
    dplyr::filter(BLQ != 1)
  
  ## Execute with Cholesky data toggle active and no skipped omegas
  vpcData <- createFFEMdata(
    modName          = "run31",
    modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
    parNames         = c("CL", "V", "MAT"),
    numNonFREMThetas = 7,
    numSkipOm        = 0,
    dataFile         = data,
    newDataFile      = NULL,
    quiet            = TRUE,
    omegaToData      = TRUE
  )
  
  output_data <- vpcData$newData
  
  # Verify the structural variance columns exist
  expect_true("V11" %in% names(output_data))
  expect_true("V21" %in% names(output_data))
  expect_true("V22" %in% names(output_data))
  
  # Verify data population (ensure it hasn't injected NAs due to matrix bounds errors)
  expect_false(any(is.na(output_data$V11)))
  expect_true(is.numeric(output_data$V11))
  
  # Snapshot the structural output to prevent regression
  expect_snapshot_value(stabilize(as.data.frame(head(output_data, 20))), style = "serialize")
})


test_that("createFFEMdata shifts V-column indices correctly when numSkipOm > 0", {
  data <- readr::read_csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"), show_col_types = FALSE) %>%
    dplyr::filter(BLQ != 1)
  
  ## Execute with Cholesky data toggle active and 2 skipped omegas
  vpcData <- createFFEMdata(
    modName          = "run31",
    modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
    parNames         = c("CL", "V", "MAT"),
    numNonFREMThetas = 7,
    numSkipOm        = 2,
    dataFile         = data,
    newDataFile      = NULL,
    quiet            = TRUE,
    omegaToData      = TRUE
  )
  
  output_data <- vpcData$newData
  
  # With numSkipOm = 2, we must NOT see V11 or V22
  expect_false("V11" %in% names(output_data))
  expect_false("V22" %in% names(output_data))
  
  # We MUST see V33, V43, etc., correctly offset
  expect_true("V33" %in% names(output_data))
  expect_true("V43" %in% names(output_data))
  expect_true("V44" %in% names(output_data))
  
  # Verify data population
  expect_false(any(is.na(output_data$V33)))
  
  # Snapshot the structural output to prevent regression
  expect_snapshot_value(stabilize(as.data.frame(head(output_data, 20))), style = "serialize")
})