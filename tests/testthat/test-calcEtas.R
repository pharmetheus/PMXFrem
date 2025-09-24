# --- Setup test data once to avoid repetition ---
test_data <- readr::read_csv(
  system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"),
  show_col_types = FALSE
) %>%
  dplyr::filter(BLQ != 1)

# --- Test Group 1: Original functionality (backward compatibility) ---
test_that("calEtas works with a pre-created FFEMData object", {
  
  # 1. Create the FFEMData object first
  vpcData <- createFFEMdata(
    modName          = "run31",
    modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
    parNames         = c("CL", "V", "MAT"),
    numNonFREMThetas = 7,
    numSkipOm        = 2,
    dataFile         = test_data,
    newDataFile      = NULL,
    quiet            = TRUE
  )
  
  # 2. Pass the object to calcEtas
  ind_params <- calcEtas(
    modName          = "run31",
    modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
    numSkipOm        = 2,
    numNonFREMThetas = 7,
    FFEMData         = vpcData
  )
  
  expect_s3_class(ind_params, "data.frame")
  # Using expect_snapshot is great for complex objects
  expect_snapshot(ind_params)
})


# --- Test Group 2: New functionality (internal object creation) ---
test_that("calEtas works when creating FFEMData internally", {
  
  # Method 1: The original way (our reference result)
  vpcData <- createFFEMdata(modName = "run31", modDevDir = system.file("extdata/SimNeb/", package = "PMXFrem"),
                            parNames = c("CL", "V", "MAT"), numNonFREMThetas = 7, numSkipOm = 2,
                            dataFile = test_data, newDataFile = NULL, quiet = TRUE)
  result_precreated <- calcEtas(modName = "run31", modDevDir = system.file("extdata/SimNeb/", package = "PMXFrem"),
                                numSkipOm = 2, numNonFREMThetas = 7, FFEMData = vpcData)
  
  # Method 2: The new, direct way
  result_internal <- calcEtas(
    modName          = "run31",
    modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
    numSkipOm        = 2,
    numNonFREMThetas = 7,
    FFEMData         = NULL, # Explicitly NULL
    dataFile         = test_data,
    parNames         = c("CL", "V", "MAT")
  )
  
  # The results must be identical
  expect_equal(result_internal, result_precreated)
})


# --- Test Group 3: Error handling ---
test_that("calcEtas throws an error if data is not provided for internal creation", {
  
  expect_error(
    calcEtas(
      modName          = "run31",
      modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
      numSkipOm        = 2,
      numNonFREMThetas = 7,
      FFEMData         = NULL,
      dataFile         = NULL, # Missing dataFile
      parNames         = c("CL", "V", "MAT")
    ),
    regexp = "If `FFEMData` is NULL, you must provide `dataFile` and `parNames`."
  )
})
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
  # Use the robust value snapshot instead of the text-based one
  expect_snapshot_value(stabilize(ind_params), style = "serialize")
})

# --- Test Group 4: Argument-specific tests for createFFEMdata ---
test_that("createFFEMdata respects the availCov argument", {
  
  # 1. Generate FFEMData using ALL available covariates (default behavior)
  full_cov_data <- createFFEMdata(
    modName          = "run31",
    modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
    parNames         = c("CL", "V", "MAT"),
    numNonFREMThetas = 7,
    numSkipOm        = 2,
    dataFile         = test_data,
    newDataFile      = NULL,
    quiet            = TRUE,
    availCov         = "all" # Explicitly state the default
  )
  
  # 2. Get the names of the covariates to test with a subset
  # For the "run31" model, the covariates are WT and SEX
  test_covariate <- "WT"
  
  # 3. Generate FFEMData using only a SUBSET of covariates
  subset_cov_data <- createFFEMdata(
    modName          = "run31",
    modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
    parNames         = c("CL", "V", "MAT"),
    numNonFREMThetas = 7,
    numSkipOm        = 2,
    dataFile         = test_data,
    newDataFile      = NULL,
    quiet            = TRUE,
    availCov         = test_covariate # Use only WT
  )
  
  # 4. Assertions
  # The two FFEMData objects themselves should not be identical
  expect_false(identical(full_cov_data, subset_cov_data))
  
  # More specifically, the calculated individual covariate effects in the newData
  # data frame should be different, as one includes effects from all covariates
  # and the other only from the specified subset.
  # We check one of the generated columns, e.g., CLFREMCOV.
  expect_false(
    identical(
      full_cov_data$newData$CLFREMCOV,
      subset_cov_data$newData$CLFREMCOV
    )
  )
})
