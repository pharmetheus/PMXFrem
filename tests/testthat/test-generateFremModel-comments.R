# generateFremModel() re-emits the $THETA / $OMEGA labels it finds in the input
# model. The mapping from comment to parameter is positional, so it has to
# survive formatting the input happens to use: standalone notes, uncommented
# records, multi-value records, and block rows wrapped over several lines.

.run31Copy <- function(td, mod = NULL) {
  file.copy(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"), td)
  p <- file.path(td, "run31.mod")
  if (!is.null(mod)) writeLines(mod(readLines(p)), p)
  p
}

## Labels of the regenerated $OMEGA records, in eta order.
.omegaLabels <- function(lines) {
  om <- grep("^\\s*\\$OMEGA", lines)[1]
  en <- grep("^\\s*\\$SIGMA", lines)[1] - 1L
  seg <- lines[om:en]
  seg <- seg[grepl(";", seg)]
  trimws(sub("^[^;]*;\\s*", "", seg))
}

.update <- function(path) {
  updateFREMmodel(strFREMModel = path, numNonFREMThetas = 7, numSkipOm = 2,
                  bWriteData = FALSE, bWriteMod = FALSE, quiet = TRUE,
                  strUpdateType = "NoData")$model
}


test_that("value counting handles the NONMEM record forms", {
  # $OMEGA: BLOCK(k) size is not a value; FIX and friends are stripped
  expect_equal(PMXFrem:::.fremCountOmegaValues("$OMEGA 0.05 ; lbl"), 1L)
  expect_equal(PMXFrem:::.fremCountOmegaValues("$OMEGA 0.0001 FIX ; lbl"), 1L)
  expect_equal(PMXFrem:::.fremCountOmegaValues("$OMEGA BLOCK(21)"), 0L)
  expect_equal(PMXFrem:::.fremCountOmegaValues("$OMEGA BLOCK(1) 0.05 ; lbl"), 1L)
  expect_equal(PMXFrem:::.fremCountOmegaValues(" 0.13 0.18  ; row 2"), 2L)
  expect_equal(PMXFrem:::.fremCountOmegaValues("; just a note"), 0L)
  expect_equal(PMXFrem:::.fremCountOmegaValues(""), 0L)

  # $THETA: a bounded record is one value, FIX is not one
  expect_equal(PMXFrem:::.fremCountThetaValues("$THETA 1 FIX ; 1. TVFREL"), 1L)
  expect_equal(PMXFrem:::.fremCountThetaValues("$THETA (0,6.11) ; 2. TVCL"), 1L)
  expect_equal(PMXFrem:::.fremCountThetaValues("$THETA (-1.00,-0.05,3.00)"), 1L)
  expect_equal(PMXFrem:::.fremCountThetaValues("$THETA 1 2 3"), 3L)
  expect_equal(PMXFrem:::.fremCountThetaValues("; just a note"), 0L)
})

test_that("a well-formed model round-trips its labels unchanged", {
  td  <- withr::local_tempdir()
  out <- .update(.run31Copy(td))
  lab <- .omegaLabels(out)
  expect_equal(lab[1:5], c("1. IIV on RUV", "2. IIV on D1", "3. IIV on CL",
                           "4. IIV on V", "5. IIV on MAT"))
  expect_equal(lab[6], "BSV_WT")
  expect_equal(lab[23], "BSV_SMOK")     # the last, reached through wrapped rows
  expect_length(lab, 23L)
})

test_that("a standalone comment inside $OMEGA does not shift the labels", {
  td <- withr::local_tempdir()
  p  <- .run31Copy(td, function(L) {
    at <- grep("^\\$OMEGA\\s+0\\.0001", L)[1]        # before the 2nd omega
    append(L, "; tweaked this after run 27", after = at - 1L)
  })
  lab <- .omegaLabels(.update(p))
  # without the fix the note consumed a slot and pushed every later label down
  expect_equal(lab[1:5], c("1. IIV on RUV", "2. IIV on D1", "3. IIV on CL",
                           "4. IIV on V", "5. IIV on MAT"))
  expect_equal(lab[23], "BSV_SMOK")
})

test_that("an uncommented $OMEGA record does not shift the labels", {
  td <- withr::local_tempdir()
  p  <- .run31Copy(td, function(L) {
    i <- grep("^\\$OMEGA\\s+0\\.0542558", L)[1]      # drop the 1st omega's comment
    L[i] <- sub(";.*$", "", L[i])
    L
  })
  lab <- .omegaLabels(.update(p))
  # eta 1 falls back to the generated label; everything after keeps its own
  expect_equal(lab[2:5], c("2. IIV on D1", "3. IIV on CL", "4. IIV on V",
                           "5. IIV on MAT"))
  expect_equal(lab[23], "BSV_SMOK")
})

test_that("wrapped block rows keep their labels aligned", {
  # run31's later BLOCK(21) rows already wrap over two physical lines, with the
  # comment only on the second. Counting rows by line would over-count them.
  td  <- withr::local_tempdir()
  lab <- .omegaLabels(.update(.run31Copy(td)))
  expect_equal(lab[17:23], c("BSV_RACEL_3", "BSV_RACEL_2", "BSV_NCIL_2",
                             "BSV_NCIL_1", "BSV_GENO2", "BSV_ETHNIC",
                             "BSV_SMOK"))
})
