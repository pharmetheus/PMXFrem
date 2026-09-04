# createFREMParamFunction() / verifyFREMParamFunction()

.fremMod <- function() system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
.fremExt <- function() system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")

.finals <- function() {                      # all FREM-model THETA finals
  de <- getExt(extFile = .fremExt())
  as.numeric(de[de$ITERATION == -1000000000, grep("^THETA", names(de)), drop = TRUE])
}

# a tiny model with a controllable $PK, for the ETA-placement edge cases.
# numSkipOm + numNonFREMThetas are passed explicitly so fremModelInfo() (which
# needs the ;;;FREM CODE markers) is not called.
.stubMod <- function(pk) {
  f <- withr::local_tempfile(fileext = ".mod", .local_envir = parent.frame())
  writeLines(c(
    "$PROBLEM stub", "$INPUT ID TIME DV WT", "$DATA d.csv IGNORE=@",
    "$PK", pk, "$ERROR", "  Y = F + EPS(1)",
    "$THETA 1 2 3 4", "$OMEGA 0.1 0.1 0.1", "$SIGMA 1"
  ), f)
  f
}

# ---------------------------------------------------------------------------
# result shape / derivation
# ---------------------------------------------------------------------------

test_that("createFREMParamFunction returns the expected list shape", {
  out <- createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                                 extFile = .fremExt(), quiet = TRUE)
  expect_type(out, "list")
  expect_s3_class(out$code, "pmxFREMParamFunction")
  expect_identical(out$functionListName, c("CL", "V", "MAT"))
  expect_identical(out$primaryNames, c("CL", "V", "MAT"))
  expect_identical(out$secondaryNames, character(0))
  expect_identical(out$fremParameters, c("CL", "V", "MAT"))
  expect_equal(out$numSkipOm, 2)                 # derived
  expect_equal(out$numNonFREMThetas, 7)          # derived
  expect_equal(out$noBaseThetas, 7)              # == numNonFREMThetas
  expect_equal(out$numParCov, 3)
  expect_identical(out$fremModel, .fremMod())
  expect_true("FOOD" %in% names(out$covRef))
})

test_that("numSkipOm / numNonFREMThetas are derived, or validated when given", {
  d <- createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                               extFile = .fremExt(), quiet = TRUE)
  expect_equal(c(d$numSkipOm, d$numNonFREMThetas), c(2, 7))

  expect_silent(
    createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                            numSkipOm = 2, numNonFREMThetas = 7,
                            extFile = .fremExt(), quiet = TRUE)
  )
  # a wrong numSkipOm: fremModelInfo() warns, and the eta index no longer
  # matches numSkipOm + k so the emitter warns too
  w <- capture_warnings(
    createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                            numSkipOm = 1, extFile = .fremExt(), quiet = TRUE))
  expect_true(any(grepl("numSkipOm", w)))
  expect_true(any(grepl("was expected for numSkipOm = 1", w)))
})

test_that("createFREMParamFunction validates its arguments", {
  expect_error(
    createFREMParamFunction(.fremMod(), parameters = character(0),
                            extFile = .fremExt()),
    "at least one")
  expect_error(
    createFREMParamFunction(parameters = c("CL", "V")),
    "Supply .fremModel."
  )
  expect_warning(
    createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                            numParCov = 5, extFile = .fremExt(), quiet = TRUE),
    "numParCov \\(5\\) does not match"
  )
  expect_error(
    createFREMParamFunction(.fremMod(), parameters = c("CL", "NOSUCHPAR"),
                            extFile = .fremExt(), quiet = TRUE),
    "NOSUCHPAR"
  )
})

# ---------------------------------------------------------------------------
# the generated source
# ---------------------------------------------------------------------------

test_that("the body is pruned: the FREM covariate block is dropped", {
  out  <- createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                                  extFile = .fremExt(), quiet = TRUE)
  code <- paste(out$code, collapse = "\n")
  expect_match(code, "function\\(basethetas, covthetas, dfrow, etas = rep\\(0, 5\\)")
  expect_match(code, "CL <- exp\\(MU_3 \\+ \\(covthetas\\[1\\] \\+ .eta\\(etas, 3\\)\\)\\)")
  expect_match(code, "MAT <- MATCOVTIME \\* exp\\(MU_5 \\+ \\(covthetas\\[3\\] \\+ .eta\\(etas, 5\\)\\)\\)")
  # the appended MU_j = THETA(8..25) / COVj block is not needed by CL/V/MAT
  expect_no_match(code, "COV[0-9]+ <-")
  expect_no_match(code, "MU_6 <-")
  expect_no_match(code, "basethetas\\[(8|9|1[0-9]|2[0-5])\\]")
})

test_that("ETA() is replaced whatever encloses it (not only inside exp())", {
  bm  <- .stubMod(c(
    "  TVCL = THETA(1)", "  TVV  = THETA(2)",
    "  CL = TVCL * EXP(ETA(1))",     # multiplicative
    "  V  = TVV + ETA(2)"            # additive
  ))
  out  <- createFREMParamFunction(bm, parameters = c("CL", "V"), numSkipOm = 0,
                                  numNonFREMThetas = 4, quiet = TRUE)
  code <- paste(out$code, collapse = "\n")
  expect_match(code, "CL <- TVCL \\* exp\\(\\(covthetas\\[1\\] \\+ .eta\\(etas, 1\\)\\)\\)")
  expect_match(code, "V <- TVV \\+ \\(covthetas\\[2\\] \\+ .eta\\(etas, 2\\)\\)")
})

test_that("fremEtaScale classifies the ETA() enclosure per FREM parameter", {
  bm  <- .stubMod(c(
    "  TVCL = THETA(1)", "  TVV = THETA(2)", "  TVKA = THETA(3)",
    "  CL = TVCL * EXP(ETA(1))",         # log-normal
    "  V  = TVV + ETA(2)",               # additive
    "  KA = EXP(THETA(3) * ETA(3))"      # exp of a non-unit multiple -> not simple
  ))
  out <- createFREMParamFunction(bm, parameters = c("CL", "V", "KA"),
                                 numSkipOm = 0, numNonFREMThetas = 4, quiet = TRUE)
  expect_identical(out$fremEtaScale, c(CL = "exp", V = "other", KA = "other"))
})

test_that("verifyFREMParamFunction skips the splice for a non-log-normal parameter", {
  bm  <- .stubMod(c(
    "  TVCL = THETA(1)", "  TVV = THETA(2)",
    "  CL = TVCL * EXP(ETA(1))",     # log-normal
    "  V  = TVV + ETA(2)"            # additive
  ))
  out <- createFREMParamFunction(bm, parameters = c("CL", "V"), numSkipOm = 0,
                                 numNonFREMThetas = 4, quiet = TRUE)
  v <- verifyFREMParamFunction(out, thetas = c(3, 5, 0, 0), quiet = TRUE)
  d <- attr(v, "checks")

  expect_true(d$PASS[d$PARAMETER == "CL"])                  # fully checked
  expect_true(is.na(d$PASS[d$PARAMETER == "V"]))            # splice skipped
  expect_true(is.na(d$COVSPLICE[d$PARAMETER == "V"]))
  expect_true(is.na(d$ETASPLICE[d$PARAMETER == "V"]))
  expect_false(is.na(d$STRUCTURAL[d$PARAMETER == "V"]))     # structural still ran
  expect_true(d$STRUCTURAL[d$PARAMETER == "V"] <= 1e-6)

  expect_true(as.logical(v))                                # nothing failed
  expect_output(print(v), "non-log-normal")
})

test_that("verifyFREMParamFunction still FALSE if a non-log-normal parameter is structurally wrong", {
  bm  <- .stubMod(c(
    "  TVV = THETA(2)",
    "  V  = TVV + ETA(1)"            # additive
  ))
  out <- createFREMParamFunction(bm, parameters = "V", numSkipOm = 0,
                                 numNonFREMThetas = 4, quiet = TRUE)
  # tamper: the generated V is `basethetas[2] + (...)`; make it basethetas[1]
  bad <- gsub("basethetas\\[2\\]", "basethetas[1]", paste(out$code, collapse = "\n"))
  v   <- verifyFREMParamFunction(out, fun = eval(parse(text = bad)),
                                 thetas = c(9, 5, 0, 0), quiet = TRUE)
  expect_false(as.logical(v))
  expect_false(attr(v, "checks")$PASS[1])   # STRUCTURAL fails -> PASS FALSE, not NA
})

test_that("a parameter with no ETA() is returned as-is, not an error", {
  bm  <- .stubMod(c("  TVCL = THETA(1)", "  CL = TVCL",
                    "  TVV = THETA(2)",  "  V  = TVV * EXP(ETA(1))"))
  out <- createFREMParamFunction(bm, parameters = c("V", "CL"), numSkipOm = 0,
                                 numNonFREMThetas = 4, quiet = TRUE)
  expect_identical(out$fremParameters, "V")
  expect_equal(out$numParCov, 1)
  code <- paste(out$code, collapse = "\n")
  expect_match(code, "V <- TVV \\* exp\\(\\(covthetas\\[1\\] \\+ .eta\\(etas, 1\\)\\)\\)")
  expect_match(code, "CL <- TVCL   # returned as-is")

  fn <- eval(parse(text = out$code))
  r  <- fn(basethetas = c(3, 5), covthetas = 0.2, dfrow = data.frame(), etas = 0)
  expect_equal(r$CL, 3)
  expect_equal(r$V,  5 * exp(0.2))
})

test_that("a $PK assignment referencing ETA() more than once is returned as-is with a warning", {
  bm <- .stubMod(c("  TVCL = THETA(1)", "  CL = TVCL * EXP(ETA(1) + ETA(2))"))
  expect_warning(
    out <- createFREMParamFunction(bm, parameters = "CL", numSkipOm = 0,
                                   numNonFREMThetas = 4, quiet = TRUE),
    "references ETA\\(\\) 2 times"
  )
  expect_length(out$fremParameters, 0)
  expect_match(paste(out$code, collapse = "\n"),
               "returned as-is; ETA\\(\\) -> 0")
})

test_that("an unexpected ETA index warns but still emits by parameter position", {
  bm <- .stubMod(c("  TVCL = THETA(1)", "  CL = TVCL * EXP(ETA(4))"))
  expect_warning(
    out <- createFREMParamFunction(bm, parameters = "CL", numSkipOm = 0,
                                   numNonFREMThetas = 4, quiet = TRUE),
    "ETA\\(4\\).*ETA\\(1\\) was expected"
  )
  expect_match(paste(out$code, collapse = "\n"),
               "CL <- TVCL \\* exp\\(\\(covthetas\\[1\\] \\+ .eta\\(etas, 1\\)\\)\\)")
})

# ---------------------------------------------------------------------------
# the generated function behaves correctly
# ---------------------------------------------------------------------------

test_that("with covthetas = 0 and etas = 0 the FREM function equals the SCM typical values", {
  out <- createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                                 extFile = .fremExt(), quiet = TRUE)
  fn  <- eval(parse(text = out$code))

  scm   <- PMXForest::createParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                                          extFile = .fremExt(), quiet = TRUE)
  scmFn <- eval(parse(text = scm$code))

  full <- .finals()
  bth  <- full[seq_len(out$noBaseThetas)]
  sth  <- full[seq_len(scm$noBaseThetas)]
  for (FOOD in c(0, 1)) {
    dfrow <- data.frame(FOOD = FOOD)
    a <- fn(bth,  covthetas = c(0, 0, 0), dfrow = dfrow, etas = rep(0, 5))
    b <- scmFn(thetas = sth, df = dfrow)
    expect_equal(unlist(a), unlist(b), tolerance = 1e-10)
  }
})

test_that("covthetas[k] / etas[numSkipOm+k] scale only parameter k, by exp()", {
  out <- createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                                 extFile = .fremExt(), quiet = TRUE)
  fn  <- eval(parse(text = out$code))
  bth <- .finals()[seq_len(out$noBaseThetas)]
  dfrow <- data.frame(FOOD = 1)
  base  <- unlist(fn(bth, covthetas = c(0, 0, 0), dfrow = dfrow, etas = rep(0, 5)))

  ct  <- c(0.11, -0.07, 0.2)
  got <- unlist(fn(bth, covthetas = ct, dfrow = dfrow, etas = rep(0, 5)))
  expect_equal(unname(got / base), exp(ct), tolerance = 1e-10)

  for (k in 1:3) {
    e <- rep(0, 5); e[2 + k] <- 0.25
    g <- unlist(fn(bth, covthetas = c(0, 0, 0), dfrow = dfrow, etas = e))
    exp_k <- base; exp_k[k] <- exp_k[k] * exp(0.25)
    expect_equal(unname(g), unname(exp_k), tolerance = 1e-10)
  }
  expect_silent(fn(bth, covthetas = c(0, 0, 0), dfrow = dfrow, etas = numeric(0)))
})

# ---------------------------------------------------------------------------
# verifyFREMParamFunction()
# ---------------------------------------------------------------------------

test_that("verifyFREMParamFunction returns a scalar TRUE for a faithful function", {
  out <- createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                                 extFile = .fremExt(), quiet = TRUE)
  v <- verifyFREMParamFunction(out, extFile = .fremExt(), quiet = TRUE)

  expect_length(as.logical(v), 1L)
  expect_true(as.logical(v))
  expect_true(if (v) TRUE else FALSE)              # usable in an if

  d <- attr(v, "checks")
  expect_s3_class(d, "data.frame")
  expect_identical(d$PARAMETER, c("CL", "V", "MAT"))
  expect_true(all(d$PASS))
  expect_true(all(d$STRUCTURAL < 1e-8))
})

test_that("verifyFREMParamFunction returns FALSE and flags the tampered parameter", {
  out <- createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                                 extFile = .fremExt(), quiet = TRUE)
  bad <- gsub("covthetas\\[1\\]", "2 * covthetas[1]", paste(out$code, collapse = "\n"))
  v   <- verifyFREMParamFunction(out, fun = eval(parse(text = bad)),
                                 extFile = .fremExt(), quiet = TRUE)
  expect_false(as.logical(v))
  d <- attr(v, "checks")
  expect_false(d$PASS[d$PARAMETER == "CL"])
  expect_true(all(d$PASS[d$PARAMETER != "CL"]))
})

test_that("verifyFREMParamFunction ignores secondary parameters", {
  out <- createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                                 extFile = .fremExt(), quiet = TRUE,
                                 secondary = list(AUC = "80 / CL"))
  v <- verifyFREMParamFunction(out, extFile = .fremExt(), quiet = TRUE)
  expect_true(as.logical(v))
  expect_identical(attr(v, "checks")$PARAMETER, c("CL", "V", "MAT"))  # no AUC
})

# ---------------------------------------------------------------------------
# secondary parameters
# ---------------------------------------------------------------------------

test_that("a secondary snippet is spliced in, returned, and listed", {
  out <- createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                                 extFile = .fremExt(), quiet = TRUE,
                                 secondary = list(AUC = "dfrow$DOSE / CL",
                                                  KEL = "CL / V"))
  expect_identical(out$functionListName, c("CL", "V", "MAT", "AUC", "KEL"))
  expect_identical(out$primaryNames,   c("CL", "V", "MAT"))
  expect_identical(out$secondaryNames, c("AUC", "KEL"))

  code <- paste(out$code, collapse = "\n")
  expect_match(code, "df <- dfrow")                       # alias emitted
  expect_match(code, "AUC <- local\\(\\{ dfrow\\$DOSE / CL \\}\\)")

  fn  <- eval(parse(text = out$code))
  bth <- .finals()[seq_len(out$noBaseThetas)]
  v   <- fn(basethetas = bth, covthetas = c(0, 0, 0),
            dfrow = data.frame(DOSE = 100), etas = rep(0, out$numSkipOm + 3))
  expect_named(v, c("CL", "V", "MAT", "AUC", "KEL"))
  expect_equal(v$AUC, 100 / v$CL)
  expect_equal(v$KEL, v$CL / v$V)
})

test_that("a config-list secondary binds its constants ahead of the source", {
  out <- createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                                 extFile = .fremExt(), quiet = TRUE,
                                 secondary = list(
                                   AUC = list(source = "dose / CL", dose = 240)))
  expect_identical(out$functionListName, c("CL", "V", "MAT", "AUC"))
  code <- paste(out$code, collapse = "\n")
  expect_match(code, "AUC <- local\\(\\{\\n\\s*dose <- 240")

  fn  <- eval(parse(text = out$code))
  bth <- .finals()[seq_len(out$noBaseThetas)]
  v   <- fn(basethetas = bth, covthetas = c(0, 0, 0), dfrow = data.frame(),
            etas = rep(0, out$numSkipOm + 3))
  expect_equal(v$AUC, 240 / v$CL)
})

test_that("a secondary from a file is inlined verbatim and survives file removal", {
  rf <- withr::local_tempfile(fileext = ".R")
  writeLines(c("## model string must not be re-indented",
               "code <- \"",
               "$PARAM CL=1",
               "\"",
               "nchar(code)"), rf)
  out <- createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                                 extFile = .fremExt(), quiet = TRUE,
                                 secondary = list(NC = rf))
  code <- paste(out$code, collapse = "\n")
  expect_match(code, "\\n\\$PARAM CL=1\\n")               # column-0, not indented
  file.remove(rf)
  fn  <- eval(parse(text = out$code))
  bth <- .finals()[seq_len(out$noBaseThetas)]
  v <- fn(basethetas = bth, covthetas = c(0, 0, 0), dfrow = data.frame(),
          etas = rep(0, out$numSkipOm + 3))
  expect_true(is.finite(v$NC))
})

test_that("a generated function with a secondary drives getForestDFFREM()", {
  out <- createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                                 extFile = .fremExt(), quiet = TRUE,
                                 secondary = list(AUC = "80 / CL"))
  fn  <- eval(parse(text = out$code))

  covNames <- getCovNames(.fremMod())
  dfCovs   <- data.frame(WT = c(60, 90), AGE = c(-99, -99))
  set.seed(1)
  samples <- PMXForest::getSamples(
    system.file("extdata/SimNeb/bs31.dir/raw_results_run31.csv", package = "PMXFrem"),
    extFile = .fremExt(), n = 10)

  res <- suppressWarnings(getForestDFFREM(
    dfCovs = dfCovs, covNames = covNames$covNames, functionList = list(fn),
    functionListName = out$functionListName, numNonFREMThetas = 7, numSkipOm = 2,
    dfParameters = samples, quiet = TRUE, cstrPackages = c("PMXFrem", "dplyr")))
  expect_setequal(as.character(unique(res$PARAMETER)), c("CL", "V", "MAT", "AUC"))
  expect_true(all(is.finite(res$POINT)))
})

# ---------------------------------------------------------------------------
# Integration: the generated function drives getForestDFFREM / getExplainedVar
# ---------------------------------------------------------------------------

test_that("a generated function works inside getForestDFFREM()", {
  modDevDir <- system.file("extdata/SimNeb/", package = "PMXFrem")

  out <- createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                                 extFile = .fremExt(), quiet = TRUE)
  fn  <- eval(parse(text = out$code))

  covNames <- getCovNames(.fremMod())
  dfCovs   <- data.frame(WT = c(60, 90), AGE = c(-99, -99))
  set.seed(1)
  samples  <- PMXForest::getSamples(
    system.file("extdata/SimNeb/bs31.dir/raw_results_run31.csv", package = "PMXFrem"),
    extFile = .fremExt(), n = 10)

  res <- suppressWarnings(getForestDFFREM(
    dfCovs = dfCovs, covNames = covNames$covNames, functionList = list(fn),
    functionListName = c("CL", "V", "MAT"), numNonFREMThetas = 7, numSkipOm = 2,
    dfParameters = samples, quiet = TRUE, cstrPackages = c("PMXFrem", "dplyr")))
  expect_s3_class(res, "data.frame")
  expect_setequal(as.character(unique(res$PARAMETER)), c("CL", "V", "MAT"))
  expect_true(all(is.finite(res$POINT)))
})

test_that("a generated function works inside getExplainedVar() (etas != 0 path)", {
  modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")

  out <- createFREMParamFunction(.fremMod(), parameters = c("CL", "V", "MAT"),
                                 extFile = .fremExt(), quiet = TRUE)
  fn  <- eval(parse(text = out$code))

  dfCovs <- setupDfCovsEV(.fremMod())
  ev <- getExplainedVar(
    type = 0, data = NULL, dfCovs = dfCovs, functionList = list(fn),
    functionListName = c("CL", "V", "MAT"),
    cstrCovariates = c("All", names(dfCovs)),
    modDevDir = modDevDir, runno = 31, ncores = 1, quiet = TRUE, seed = 123)
  expect_s3_class(ev, "data.frame")
  expect_true(all(is.finite(ev$TOTVAR)))
})
