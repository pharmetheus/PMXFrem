# The base model's $THETA bounds survive into the FREM model (T36), and a
# FREM model starts from the base model's final estimates whatever the number
# of covariates. run30's $THETA records:
#   (0,7.62228) TVCL ... (-1.00,-0.0800943,3.00) FRELFOOD1 / MATFOOD1
# and run30.ext's final TVCL estimate is 6.17206.

fremThetaLines <- function(covariates, modDir = NULL, quiet = TRUE) {
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  if (is.null(modDir)) modDir <- src
  td <- withr::local_tempdir()
  res <- suppressWarnings(createFREMmodel(
    modName = "run30", modDevDir = modDir,
    ffemDataFile = file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"),
    covariates = covariates, numSkipOm = 2,
    cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "RATE", "FOOD", "DV"),
    outputDir = td, fremModName = "frem", quiet = quiet
  ))
  grep("^\\$THETA", readLines(res$model), value = TRUE)
}

thetaLine <- function(lines, label) {
  hit <- grep(paste0("\\b", label, "\\b"), lines, value = TRUE)
  expect_length(hit, 1)
  hit
}

test_that("the base model's bounds are kept, around the final estimates", {
  th <- fremThetaLines(c("AGE", "WT"))
  expect_match(thetaLine(th, "TVCL"), "^\\$THETA\\s*\\(0,\\s*6\\.17206\\)")
  expect_match(thetaLine(th, "FRELFOOD1"), "\\(-1(\\.0+)?,\\s*-0\\.05[0-9]*,\\s*3(\\.0+)?\\)")
  expect_match(thetaLine(th, "TVFREL"), "^\\$THETA\\s*1\\s+FIX\\b")
  ## The covariates' own THETAs have no bounds
  expect_match(thetaLine(th, "TV_AGE"), "^\\$THETA\\s*[0-9.]+\\s*;")
})

test_that("a one-covariate FREM model also starts from the final estimates, with the bounds", {
  th <- fremThetaLines("AGE")
  expect_match(thetaLine(th, "TVCL"), "^\\$THETA\\s*\\(0,\\s*6\\.17206\\)")
  expect_match(thetaLine(th, "MATFOOD1"), "\\(-1(\\.0+)?,.*,\\s*3(\\.0+)?\\)")
})

test_that("a one-covariate FREM model reads the data file written next to it", {
  ## It used to keep the minimal model's $DATA, naming a file removed at the end.
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  td <- withr::local_tempdir()
  res <- suppressWarnings(createFREMmodel(
    modName = "run30", modDevDir = src,
    ffemDataFile = file.path(src, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"),
    covariates = "AGE", numSkipOm = 2,
    cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "RATE", "FOOD", "DV"),
    outputDir = td, fremModName = "frem", quiet = TRUE
  ))
  dataRec <- grep("^\\$DATA", readLines(res$model), value = TRUE)
  expect_match(dataRec, paste0("^\\$DATA\\s+", basename(res$data), "\\b"))
  expect_true(file.exists(file.path(td, basename(res$data))))
})

test_that("an estimate on or outside a bound is moved inside it, with a message", {
  ## Tighten TVCL's lower bound past its final estimate (6.17206).
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  td <- withr::local_tempdir()
  file.copy(file.path(src, c("run30.mod", "run30.ext")), td)
  mod <- readLines(file.path(td, "run30.mod"))
  i <- grep("^\\$THETA.*TVCL", mod)
  mod[i] <- sub("\\(0,7\\.62228\\)", "(6.5,7.62228)", mod[i])
  expect_match(mod[i], "\\(6\\.5,7\\.62228\\)")
  writeLines(mod, file.path(td, "run30.mod"))

  expect_message(th <- fremThetaLines(c("AGE", "WT"), modDir = td, quiet = FALSE), "THETA\\(2\\).*6\\.17206")
  tvcl <- thetaLine(th, "TVCL")
  init <- as.numeric(sub("^\\$THETA\\s*\\(6\\.5,\\s*([^,)]+)\\).*", "\\1", tvcl))
  expect_gt(init, 6.5)
  expect_lt(init, 6.6)
})

test_that("updateFREMmodel() keeps the bounds of an existing FREM model", {
  src <- system.file("extdata", "SimNeb", package = "PMXFrem")
  td <- withr::local_tempdir()
  file.copy(file.path(src, c("run31.mod", "run31.ext")), td)
  out <- suppressWarnings(updateFREMmodel(
    strFREMModel = file.path(td, "run31.mod"),
    numNonFREMThetas = 7, numSkipOm = 2,
    bWriteData = FALSE, bWriteMod = FALSE, quiet = TRUE,
    strUpdateType = "NoData"
  ))
  th <- grep("^\\$THETA", out$model, value = TRUE)
  expect_match(thetaLine(th, "TVCL"), "^\\$THETA\\s*\\(0,")
  expect_match(thetaLine(th, "FRELFOOD1"), "\\(-1(\\.0+)?,.*,\\s*3(\\.0+)?\\)")
})

test_that(".fremThetaBounds() reads the bounds of every THETA form", {
  ## The first line is the NONMEM 7.5 help's own $THETA sample.
  b <- expect_silent(.fremThetaBounds(c(
    "$THETA  (0,3) 2 FIXED (0,.6,1) 10 (-INF,-2.7,0) (37 FIXED)",
    "$THETA (-1.00, -0.08, 3.00) ; spaces after the commas",
    "$THETA (0 2 5) FIX ; no commas, FIX outside",
    "$THETA (0,,10) ; no initial value",
    "; a comment only"
  )))
  expect_equal(b$low, c(0, NA, 0, NA, NA, NA, -1, 0, 0))
  expect_equal(b$up, c(NA, NA, 1, NA, 0, NA, 3, 5, 10))
})
