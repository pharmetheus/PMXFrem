# createFREMParamFunction() / verifyFREMParamFunction()

.baseMod <- function() system.file("extdata/SimNeb/run30.mod", package = "PMXFrem")
.baseExt <- function() system.file("extdata/SimNeb/run30.ext", package = "PMXFrem")

.finals <- function() {
  de <- getExt(extFile = .baseExt())
  as.numeric(de[de$ITERATION == -1000000000, grep("^THETA", names(de)), drop = TRUE])
}

# ---------------------------------------------------------------------------
# createFREMParamFunction(): structure of the result
# ---------------------------------------------------------------------------

test_that("createFREMParamFunction returns the expected list shape", {
  out <- createFREMParamFunction(.baseMod(), parameters = c("CL", "V", "MAT"),
                                 numSkipOm = 2, quiet = TRUE)
  expect_type(out, "list")
  expect_s3_class(out$code, "pmxFREMParamFunction")
  expect_identical(out$functionListName, c("CL", "V", "MAT"))
  expect_equal(out$noBaseThetas, 7)
  expect_equal(out$numSkipOm, 2)
  expect_equal(out$numParCov, 3)
  expect_identical(out$fremParameters, c("CL", "V", "MAT"))
  expect_true("FOOD" %in% names(out$covRef))          # structural covariate kept
})

test_that("createFREMParamFunction validates its arguments", {
  expect_error(createFREMParamFunction(.baseMod(), parameters = character(0)),
               "at least one")
  expect_warning(
    createFREMParamFunction(.baseMod(), parameters = c("CL", "V"), numParCov = 3,
                            numSkipOm = 2, quiet = TRUE),
    "numParCov \\(3\\) does not match"
  )
  expect_error(
    createFREMParamFunction(.baseMod(), parameters = c("CL", "NOSUCHPAR"),
                            quiet = TRUE),
    "NOSUCHPAR"
  )
})

test_that("the generated source replaces ETA() in place at numSkipOm + k", {
  out  <- createFREMParamFunction(.baseMod(), parameters = c("CL", "V", "MAT"),
                                  numSkipOm = 2, quiet = TRUE)
  code <- paste(out$code, collapse = "\n")
  expect_match(code, "function\\(basethetas, covthetas, dfrow, etas = rep\\(0, 5\\)")
  expect_match(code, "CL <- exp\\(MU_3 \\+ \\(covthetas\\[1\\] \\+ .eta\\(etas, 3\\)\\)\\)")
  expect_match(code, "V <- exp\\(MU_4 \\+ \\(covthetas\\[2\\] \\+ .eta\\(etas, 4\\)\\)\\)")
  expect_match(code, "MAT <- MATCOVTIME \\* exp\\(MU_5 \\+ \\(covthetas\\[3\\] \\+ .eta\\(etas, 5\\)\\)\\)")
})

# a tiny base model with a controllable $PK, for the ETA-placement edge cases
.stubBase <- function(pk) {
  f <- withr::local_tempfile(fileext = ".mod", .local_envir = parent.frame())
  writeLines(c(
    "$PROBLEM stub", "$INPUT ID TIME DV WT", "$DATA d.csv IGNORE=@",
    "$PK", pk, "$ERROR", "  Y = F + EPS(1)",
    "$THETA 1 2 3 4", "$OMEGA 0.1 0.1 0.1", "$SIGMA 1"
  ), f)
  f
}

test_that("ETA() is replaced whatever encloses it (not only inside exp())", {
  bm  <- .stubBase(c(
    "  TVCL = THETA(1)", "  TVV  = THETA(2)",
    "  CL = TVCL * EXP(ETA(1))",     # multiplicative
    "  V  = TVV + ETA(2)"            # additive
  ))
  out  <- createFREMParamFunction(bm, parameters = c("CL", "V"), numSkipOm = 0,
                                  quiet = TRUE)
  code <- paste(out$code, collapse = "\n")
  expect_match(code, "CL <- TVCL \\* exp\\(\\(covthetas\\[1\\] \\+ .eta\\(etas, 1\\)\\)\\)")
  expect_match(code, "V <- TVV \\+ \\(covthetas\\[2\\] \\+ .eta\\(etas, 2\\)\\)")
})

test_that("a parameter with no ETA() is returned as-is, not an error", {
  bm  <- .stubBase(c("  TVCL = THETA(1)", "  CL = TVCL",
                     "  TVV = THETA(2)",  "  V  = TVV * EXP(ETA(1))"))
  out <- createFREMParamFunction(bm, parameters = c("V", "CL"), numSkipOm = 0,
                                 quiet = TRUE)
  expect_identical(out$fremParameters, "V")             # only V carries an eta
  expect_equal(out$numParCov, 1)
  code <- paste(out$code, collapse = "\n")
  expect_match(code, "V <- TVV \\* exp\\(\\(covthetas\\[1\\] \\+ .eta\\(etas, 1\\)\\)\\)")
  expect_match(code, "CL <- TVCL   # returned as-is")

  fn <- eval(parse(text = out$code))
  r  <- fn(basethetas = c(3, 5), covthetas = 0.2, dfrow = data.frame(), etas = 0)
  expect_equal(r$CL, 3)                                 # structural, no covariate effect
  expect_equal(r$V,  5 * exp(0.2))
})

test_that("a $PK assignment referencing ETA() more than once is returned as-is with a warning", {
  bm <- .stubBase(c("  TVCL = THETA(1)", "  CL = TVCL * EXP(ETA(1) + ETA(2))"))
  expect_warning(
    out <- createFREMParamFunction(bm, parameters = "CL", numSkipOm = 0,
                                   quiet = TRUE),
    "references ETA\\(\\) 2 times"
  )
  expect_length(out$fremParameters, 0)
  expect_match(paste(out$code, collapse = "\n"),
               "CL <- TVCL \\* exp\\(0 \\+ 0\\).*returned as-is; ETA\\(\\) -> 0")
})

test_that("an unexpected ETA index warns but still emits by parameter position", {
  bm <- .stubBase(c("  TVCL = THETA(1)", "  CL = TVCL * EXP(ETA(4))"))
  expect_warning(
    out <- createFREMParamFunction(bm, parameters = "CL", numSkipOm = 0,
                                   quiet = TRUE),
    "ETA\\(4\\).*ETA\\(1\\) was expected"
  )
  expect_match(paste(out$code, collapse = "\n"),
               "CL <- TVCL \\* exp\\(\\(covthetas\\[1\\] \\+ .eta\\(etas, 1\\)\\)\\)")
})

# ---------------------------------------------------------------------------
# The generated function behaves correctly
# ---------------------------------------------------------------------------

test_that("with covthetas = 0 and etas = 0 the FREM function equals the SCM typical values", {
  out <- createFREMParamFunction(.baseMod(), parameters = c("CL", "V", "MAT"),
                                 numSkipOm = 2, quiet = TRUE)
  fn  <- eval(parse(text = out$code))

  scm   <- PMXForest::createParamFunction(.baseMod(), parameters = c("CL", "V", "MAT"),
                                          quiet = TRUE)
  scmFn <- eval(parse(text = scm$code))

  th <- .finals()
  for (FOOD in c(0, 1)) {
    dfrow <- data.frame(FOOD = FOOD)
    a <- fn(th, covthetas = c(0, 0, 0), dfrow = dfrow, etas = rep(0, 5))
    b <- scmFn(thetas = th, df = dfrow)
    expect_equal(unlist(a), unlist(b), tolerance = 1e-10)
  }
})

test_that("covthetas[k] scales parameter k by exp(covthetas[k]) and nothing else", {
  out <- createFREMParamFunction(.baseMod(), parameters = c("CL", "V", "MAT"),
                                 numSkipOm = 2, quiet = TRUE)
  fn  <- eval(parse(text = out$code))
  th  <- .finals()
  dfrow <- data.frame(FOOD = 1)

  base <- unlist(fn(th, covthetas = c(0, 0, 0), dfrow = dfrow, etas = rep(0, 5)))
  ct   <- c(0.11, -0.07, 0.2)
  got  <- unlist(fn(th, covthetas = ct, dfrow = dfrow, etas = rep(0, 5)))
  expect_equal(unname(got / base), exp(ct), tolerance = 1e-10)
})

test_that("etas[numSkipOm + k] scales parameter k by exp(eta) and nothing else", {
  out <- createFREMParamFunction(.baseMod(), parameters = c("CL", "V", "MAT"),
                                 numSkipOm = 2, quiet = TRUE)
  fn  <- eval(parse(text = out$code))
  th  <- .finals()
  dfrow <- data.frame(FOOD = 1)
  base  <- unlist(fn(th, covthetas = c(0, 0, 0), dfrow = dfrow, etas = rep(0, 5)))

  for (k in 1:3) {
    e <- rep(0, 5); e[2 + k] <- 0.25
    got <- unlist(fn(th, covthetas = c(0, 0, 0), dfrow = dfrow, etas = e))
    expected <- base
    expected[k] <- expected[k] * exp(0.25)
    expect_equal(unname(got), unname(expected), tolerance = 1e-10)
  }
  # short etas -> .eta() returns 0, no error
  expect_silent(fn(th, covthetas = c(0, 0, 0), dfrow = dfrow, etas = numeric(0)))
})

# ---------------------------------------------------------------------------
# verifyFREMParamFunction()
# ---------------------------------------------------------------------------

test_that("verifyFREMParamFunction passes a faithfully generated function", {
  out <- createFREMParamFunction(.baseMod(), parameters = c("CL", "V", "MAT"),
                                 numSkipOm = 2, quiet = TRUE)
  v <- verifyFREMParamFunction(out, quiet = TRUE)
  expect_s3_class(v, "data.frame")
  expect_identical(v$PARAMETER, c("CL", "V", "MAT"))
  expect_true(all(v$PASS))
  expect_true(all(v$STRUCTURAL < 1e-8))
})

test_that("verifyFREMParamFunction flags a tampered function", {
  out <- createFREMParamFunction(.baseMod(), parameters = c("CL", "V", "MAT"),
                                 numSkipOm = 2, quiet = TRUE)
  bad <- gsub("covthetas\\[1\\]", "2 * covthetas[1]", paste(out$code, collapse = "\n"))
  v   <- verifyFREMParamFunction(out, fun = eval(parse(text = bad)), quiet = TRUE)
  expect_false(v$PASS[v$PARAMETER == "CL"])
  expect_true(all(v$PASS[v$PARAMETER != "CL"]))
})

# ---------------------------------------------------------------------------
# Integration: the generated function drives getForestDFFREM / getExplainedVar
# ---------------------------------------------------------------------------

test_that("a generated function works inside getForestDFFREM()", {
  modDevDir <- system.file("extdata/SimNeb/", package = "PMXFrem")
  fremRunno <- 31
  bm <- system.file("extdata/SimNeb/run30.mod", package = "PMXFrem")

  out <- createFREMParamFunction(bm, parameters = c("CL", "V", "MAT"),
                                 numSkipOm = 2, quiet = TRUE)
  fn  <- eval(parse(text = out$code))

  covNames <- getCovNames(file.path(modDevDir, "run31.mod"))
  dfCovs   <- data.frame(WT = c(60, 90), AGE = c(-99, -99))

  set.seed(1)
  samples <- PMXForest::getSamples(
    system.file("extdata/SimNeb/bs31.dir/raw_results_run31.csv", package = "PMXFrem"),
    extFile = file.path(modDevDir, "run31.ext"), n = 10)

  res <- suppressWarnings(getForestDFFREM(
    dfCovs           = dfCovs,
    covNames         = covNames$covNames,
    functionList     = list(fn),
    functionListName = c("CL", "V", "MAT"),
    numNonFREMThetas = 7,
    numSkipOm        = 2,
    dfParameters     = samples,
    quiet            = TRUE,
    cstrPackages     = c("PMXFrem", "dplyr")
  ))
  expect_s3_class(res, "data.frame")
  expect_setequal(as.character(unique(res$PARAMETER)), c("CL", "V", "MAT"))
  expect_true(all(is.finite(res$POINT)))
})

test_that("a generated function works inside getExplainedVar() (etas != 0 path)", {
  modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
  bm        <- file.path(modDevDir, "run30.mod")

  out <- createFREMParamFunction(bm, parameters = c("CL", "V", "MAT"),
                                 numSkipOm = 2, quiet = TRUE)
  fn  <- eval(parse(text = out$code))

  dfCovs <- setupDfCovsEV(file.path(modDevDir, "run31.mod"))
  ev <- getExplainedVar(
    type             = 0,
    data             = NULL,
    dfCovs           = dfCovs,
    functionList     = list(fn),
    functionListName = c("CL", "V", "MAT"),
    cstrCovariates   = c("All", names(dfCovs)),
    modDevDir        = modDevDir,
    runno            = 31,
    ncores           = 1,
    quiet            = TRUE,
    seed             = 123
  )
  expect_s3_class(ev, "data.frame")
  expect_true(all(is.finite(ev$TOTVAR)))
})
