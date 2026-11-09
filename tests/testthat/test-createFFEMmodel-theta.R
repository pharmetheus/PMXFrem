# The $THETA record of a generated FFEM model: the FREM estimates as initial
# values, with the bounds, FIX and comments of the base model kept.

ffemThetaRecord <- function() {
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  td <- withr::local_tempdir(.local_envir = parent.frame())
  withr::local_dir(td)
  lines <- suppressWarnings(suppressMessages(createFFEMmodel(
    runno = 31, modDevDir = src, numNonFREMThetas = 7, numSkipOm = 2,
    parNames = c("CL", "V", "MAT"),
    dataFile = file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"),
    newDataFile = "ffem.csv", quiet = TRUE,
    baserunno = 30, baseModDevDir = src
  )))
  grep("^\\$THETA", lines, value = TRUE)
}

test_that("createFFEMmodel() keeps the bounds, FIX and comments of the base model's THETAs", {
  th <- ffemThetaRecord()
  ext <- getExt(system.file("extdata", "SimNeb", "run31.ext", package = "PMXFrem"))
  est <- as.numeric(ext[ext$ITERATION == -1000000000, paste0("THETA", 1:7)])

  expect_length(th, 7)

  ## run30: THETA 1 is 1 FIX, THETAs 2-5 have a lower bound of 0, THETAs 6-7
  ## lie in (-1, 3).
  bounds <- PMXFrem:::.fremThetaBounds(th)
  expect_equal(bounds$low, c(NA, 0, 0, 0, 0, -1, -1))
  expect_equal(bounds$up, c(NA, NA, NA, NA, NA, 3, 3))
  expect_match(th[1], "^\\$THETA 1 FIX ")
  expect_false(any(grepl("FIX", th[-1])))

  ## The initial values are the FREM estimates.
  init <- vapply(th, function(l) {
    tok <- PMXFrem:::.fremThetaTokens(l)
    parts <- strsplit(gsub("[()]", "", tok), ",")[[1]]
    as.numeric(if (length(parts) >= 2) parts[2] else parts[1])
  }, numeric(1), USE.NAMES = FALSE)
  expect_equal(init, est)

  ## The comments of the base model.
  expect_equal(sub("^[^;]*;\\s*", "", th), c(
    "1. TVFREL", "2. TVCL", "3. TVV", "4. TVMAT", "5. D1",
    "6. FRELFOOD1", "7. MATFOOD1"
  ))
})

test_that("an estimate on a bound starts just inside it in the FFEM model", {
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  ext <- getExt(file.path(src, "run31.ext"))
  ## Put THETA 6 on its upper bound of 3.
  ext[ext$ITERATION == -1000000000, "THETA6"] <- 3
  td <- withr::local_tempdir()
  withr::local_dir(td)
  ## The message is part of the output quiet = FALSE gives; the FFEM model it
  ## also prints is captured.
  expect_message(
    utils::capture.output(lines <- suppressWarnings(createFFEMmodel(
      runno = 31, modDevDir = src, numNonFREMThetas = 7, numSkipOm = 2,
      parNames = c("CL", "V", "MAT"),
      dataFile = file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"),
      newDataFile = "ffem.csv", quiet = FALSE, dfext = ext,
      baserunno = 30, baseModDevDir = src
    ))),
    "THETA\\(6\\).*not strictly inside"
  )
  th6 <- grep("^\\$THETA", lines, value = TRUE)[6]
  expect_match(th6, "^\\$THETA \\(-1,2\\.996,3\\)")
})
