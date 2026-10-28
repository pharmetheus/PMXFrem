test_that("createFFEMdata works", {
  data <- utils::read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")) %>%
    dplyr::filter(BLQ != 1) %>%
    dplyr::as_tibble() # the class assertion below is that createFFEMdata() preserves it

  ## Check with specified parameter names
  vpcData <- createFFEMdata(
    modName = "run31",
    modDevDir = system.file("extdata/SimNeb/", package = "PMXFrem"),
    parNames = c("CL", "V", "MAT"),
    numNonFREMThetas = 7,
    numSkipOm = 2,
    dataFile = data,
    newDataFile = NULL,
    quiet = TRUE
  )

  expect_equal(class(vpcData), "list")
  expect_equal(length(vpcData), 6)
  expect_equal(class(vpcData$Omega)[1], "matrix")
  expect_equal(class(vpcData$Coefficients)[1], "matrix")
  expect_equal(class(vpcData$indCovEff), "character")
  expect_equal(length(vpcData$indCovEff), 3)
  expect_equal(class(vpcData$newData)[1], "tbl_df")

  expect_snapshot_value(stabilize(as.data.frame(head(vpcData$newData, 20))), style = "json2", cran = TRUE)

  ## Check without specified parameter names
  vpcData2 <- createFFEMdata(
    modName = "run31",
    modDevDir = system.file("extdata/SimNeb/", package = "PMXFrem"),
    numNonFREMThetas = 7,
    numSkipOm = 2,
    dataFile = data,
    newDataFile = NULL,
    quiet = TRUE
  )

  expect_equal(class(vpcData2), "list")
  expect_equal(length(vpcData2), 6)
  expect_equal(class(vpcData2$Omega)[1], "matrix")
  expect_equal(class(vpcData2$Coefficients)[1], "matrix")
  expect_equal(class(vpcData2$indCovEff), "character")
  expect_equal(length(vpcData2$indCovEff), 3)

  expect_snapshot_value(stabilize(as.data.frame(head(vpcData2$newData, 20))), style = "json2", cran = TRUE)

  ## Check when availCov = "all"
  vpcData3 <- createFFEMdata(
    modName = "run31",
    modDevDir = system.file("extdata/SimNeb/", package = "PMXFrem"),
    numNonFREMThetas = 7,
    numSkipOm = 2,
    dataFile = data,
    availCov = "all",
    newDataFile = NULL,
    quiet = TRUE
  )

  expect_snapshot_value(stabilize(as.data.frame(head(vpcData3$newData, 20))), style = "json2", cran = TRUE)
})

test_that("createFFEMdata works with parallel processing", {
  skip_if_workers_load_source_tree()
  data <- utils::read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")) %>%
    dplyr::filter(BLQ != 1) %>%
    dplyr::as_tibble() # the class assertion below is that createFFEMdata() preserves it

  ## Check with specified parameter names and cores > 1. No warnings: an
  ## `.export` that repeats what foreach finds itself warns on fresh workers.
  expect_no_warning(vpcData <- createFFEMdata(
    modName = "run31",
    modDevDir = system.file("extdata/SimNeb/", package = "PMXFrem"),
    parNames = c("CL", "V", "MAT"),
    numNonFREMThetas = 7,
    numSkipOm = 2,
    dataFile = data,
    newDataFile = NULL,
    quiet = TRUE,
    cores = 2
  )) # Trigger the parallel code path

  # The result should be numerically identical to the sequential run,
  # so a snapshot is a good way to verify correctness.
  expect_frame_snapshot(vpcData$newData)
})

test_that("createFFEMdata shifts V-column indices correctly when numSkipOm > 0", {
  data <- utils::read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")) %>%
    dplyr::filter(BLQ != 1) %>%
    dplyr::as_tibble() # the class assertion below is that createFFEMdata() preserves it

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
  expect_snapshot_value(stabilize(as.data.frame(head(output_data, 20))), style = "json2", cran = TRUE)
})

test_that("a missing covariate does not also blank one whose name contains it (WT in LBWT)", {
  modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
  modFile <- file.path(modDevDir, "run31.mod")
  data <- utils::read.csv(file.path(modDevDir, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"))
  data <- data[data$BLQ != 1, ]
  ## No subject in the bundled data misses WT and has LBWT, so make one
  id <- data$ID[data$WT != -99 & data$LBWT != -99][1]
  data$WT[data$ID == id] <- -99
  out <- createFFEMdata(
    modName = "run31", modDevDir = modDevDir, parNames = c("CL", "V", "MAT"),
    numNonFREMThetas = 7, numSkipOm = 2, dataFile = data, newDataFile = NULL, quiet = TRUE
  )$newData
  got <- unlist(out[out$ID == id, c("CLFREMCOV", "VFREMCOV", "MATFREMCOV")][1, ])

  ## The FFEM for this subject conditions on every covariate it has, LBWT included
  covNames <- getCovNames(modFile)$covNames
  row <- addFREMcovariates(dfFFEM = data, modFile = modFile, missVal = -99)
  row <- row[row$ID == id, ][1, ]
  has <- covNames[vapply(covNames, function(cv) !is.null(row[[cv]]) && row[[cv]] != -99, logical(1))]
  expect_true("LBWT" %in% has)
  expect_false("WT" %in% has)
  ffem <- calcFFEM(
    dfext = getExt(file.path(modDevDir, "run31.ext")), numNonFREMThetas = 7, covNames = covNames,
    availCov = has, quiet = TRUE, parNames = c("CL", "V", "MAT"), numSkipOm = 2
  )
  expected <- vapply(ffem$Expr, function(e) as.numeric(eval(parse(text = e), envir = list(data = row))), numeric(1))
  expect_equal(unname(got), unname(expected))
})

test_that(".fremCovColumns picks a covariate and its binarized columns, nothing else", {
  cols <- c("ID", "WT", "LBWT", "WT_2", "WTBL", "WT.1", "RACE_2", "BL_WT")
  expect_equal(cols[.fremCovColumns("WT", cols)], c("WT", "WT_2"))
  expect_equal(cols[.fremCovColumns("RACE", cols)], "RACE_2")
  ## a covariate whose own name ends in _<digits> is still its own column
  expect_equal(cols[.fremCovColumns("RACE_2", cols)], "RACE_2")
})
