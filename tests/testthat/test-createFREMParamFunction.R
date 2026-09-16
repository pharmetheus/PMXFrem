# createFREMParamFunction() / verifyFREMParamFunction()

.fremMod <- function() system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
.fremExt <- function() system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
## The FFEM version of run31, as createFFEMmodel() writes it. It is the
## independent structural reference: PMXForest refuses the FREM model.
.ffemMod <- function() system.file("extdata/SimNeb/run31max1-2.mod", package = "PMXFrem")

.finals <- function() { # all FREM-model THETA finals
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
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE
  )
  expect_type(out, "list")
  expect_s3_class(out$code, "pmxFREMParamFunction")
  expect_identical(out$functionListName, c("CL", "V", "MAT"))
  expect_identical(out$primaryNames, c("CL", "V", "MAT"))
  expect_identical(out$secondaryNames, character(0))
  expect_identical(out$fremParameters, c("CL", "V", "MAT"))
  expect_equal(out$numSkipOm, 2) # derived
  expect_equal(out$numNonFREMThetas, 7) # derived
  expect_equal(out$noBaseThetas, 7) # == numNonFREMThetas
  expect_equal(out$numParCov, 3)
  expect_identical(out$fremModel, .fremMod())
  expect_true("FOOD" %in% names(out$covRef))
})

test_that("numSkipOm / numNonFREMThetas are derived, or validated when given", {
  d <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE
  )
  expect_equal(c(d$numSkipOm, d$numNonFREMThetas), c(2, 7))

  expect_silent(
    createFREMParamFunction(.fremMod(),
      parameters = c("CL", "V", "MAT"),
      numSkipOm = 2, numNonFREMThetas = 7,
      extFile = .fremExt(), quiet = TRUE
    )
  )
  # A wrong numSkipOm: fremModelInfo() warns, and so does the cross-check
  # against the control stream - run31.mod has two etas before its FREM
  # $OMEGA block, not one. The emitter itself does not depend on numSkipOm
  # being right (it reads each parameter's eta index from $PK), but the
  # caller's covthetas does, so the disagreement must be said out loud.
  w <- capture_warnings(
    createFREMParamFunction(.fremMod(),
      parameters = c("CL", "V", "MAT"),
      numSkipOm = 1, extFile = .fremExt(), quiet = TRUE
    )
  )
  expect_true(any(grepl("numSkipOm", w)))
  expect_true(any(grepl(
    "has 2 eta\\(s\\) before its FREM \\$OMEGA block", w
  )))
})

test_that("createFREMParamFunction warns when a kept $PK statement references a THETA beyond numNonFREMThetas", {
  # MAT depends on MATFOOD, which uses THETA(6); force numNonFREMThetas = 4 so
  # that index sits outside the structural theta block.
  w <- capture_warnings(
    createFREMParamFunction(.fremMod(),
      parameters = "MAT",
      numNonFREMThetas = 4, numSkipOm = 2,
      extFile = .fremExt(), quiet = TRUE
    )
  )
  expect_true(any(grepl("references THETA\\(6\\), beyond .*numNonFREMThetas = 4", w)))
})

test_that("createFREMParamFunction errors when the counts can't be derived (no ext, no dfext)", {
  td <- withr::local_tempdir()
  mod <- file.path(td, "lonely.mod")
  file.copy(.fremMod(), mod) # model copied, .ext deliberately not
  expect_error(
    createFREMParamFunction(mod, parameters = c("CL", "V", "MAT"), quiet = TRUE),
    "Need `numSkipOm` and `numNonFREMThetas`, or an ext"
  )
})

test_that("createFREMParamFunction resolves the model from runno / modDevDir", {
  out <- createFREMParamFunction(
    runno = 31,
    modDevDir = system.file("extdata/SimNeb/", package = "PMXFrem"),
    parameters = c("CL", "V", "MAT"), quiet = TRUE
  )
  ref <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE
  )
  expect_identical(out$code, ref$code)
})

test_that("createFREMParamFunction writes the source to `file` and reports it when quiet = FALSE", {
  f <- withr::local_tempfile(fileext = ".R")
  expect_message(
    out <- createFREMParamFunction(.fremMod(),
      parameters = c("CL", "V", "MAT"),
      extFile = .fremExt(), file = f, quiet = FALSE
    ),
    "Written to "
  )
  expect_true(file.exists(f))
  expect_identical(readLines(f), as.character(out$code))
})

test_that("createFREMParamFunction quiet = FALSE narrates the translation and covariate references", {
  expect_message(
    createFREMParamFunction(.fremMod(),
      parameters = c("CL", "V", "MAT"),
      extFile = .fremExt(), quiet = FALSE
    ),
    "Translated \\$PK of run31.mod for FREM"
  )
})

test_that("print.pmxFREMParamFunction echoes the generated source", {
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE
  )
  expect_s3_class(out$code, "pmxFREMParamFunction")
  expect_output(print(out$code), "function\\(")
})

test_that("createFREMParamFunction validates its arguments", {
  expect_error(
    createFREMParamFunction(.fremMod(),
      parameters = character(0),
      extFile = .fremExt()
    ),
    "at least one"
  )
  expect_error(
    createFREMParamFunction(parameters = c("CL", "V")),
    "Supply .fremModel."
  )
  expect_warning(
    createFREMParamFunction(.fremMod(),
      parameters = c("CL", "V", "MAT"),
      numParCov = 5, extFile = .fremExt(), quiet = TRUE
    ),
    "numParCov \\(5\\) does not match"
  )
  expect_error(
    createFREMParamFunction(.fremMod(),
      parameters = c("CL", "NOSUCHPAR"),
      extFile = .fremExt(), quiet = TRUE
    ),
    "NOSUCHPAR"
  )
})

# ---------------------------------------------------------------------------
# the generated source
# ---------------------------------------------------------------------------

test_that("the body is pruned: the FREM covariate block is dropped", {
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE
  )
  code <- paste(out$code, collapse = "\n")
  # 23 etas: the model's, not the request's. etas is indexed by the model's
  # own numbering, so the default zero vector has to span all of them.
  expect_match(code, "function\\(basethetas, covthetas, dfrow, etas = rep\\(0, 23\\)")
  expect_match(code, "CL <- exp\\(MU_3 \\+ \\(covthetas\\[1\\] \\+ .eta\\(etas, 3\\)\\)\\)")
  expect_match(code, "MAT <- MATCOVTIME \\* exp\\(MU_5 \\+ \\(covthetas\\[3\\] \\+ .eta\\(etas, 5\\)\\)\\)")
  # the appended MU_j = THETA(8..25) / COVj block is not needed by CL/V/MAT
  expect_no_match(code, "COV[0-9]+ <-")
  expect_no_match(code, "MU_6 <-")
  expect_no_match(code, "basethetas\\[(8|9|1[0-9]|2[0-5])\\]")
})

test_that("ETA() is replaced whatever encloses it (not only inside exp())", {
  bm <- .stubMod(c(
    "  TVCL = THETA(1)", "  TVV  = THETA(2)",
    "  CL = TVCL * EXP(ETA(1))", # multiplicative
    "  V  = TVV + ETA(2)" # additive
  ))
  out <- createFREMParamFunction(bm,
    parameters = c("CL", "V"), numSkipOm = 0,
    numNonFREMThetas = 4, quiet = TRUE
  )
  code <- paste(out$code, collapse = "\n")
  expect_match(code, "CL <- TVCL \\* exp\\(\\(covthetas\\[1\\] \\+ .eta\\(etas, 1\\)\\)\\)")
  expect_match(code, "V <- TVV \\+ \\(covthetas\\[2\\] \\+ .eta\\(etas, 2\\)\\)")
})

test_that("fremEtaScale classifies the ETA() enclosure per FREM parameter", {
  bm <- .stubMod(c(
    "  TVCL = THETA(1)", "  TVV = THETA(2)", "  TVKA = THETA(3)",
    "  CL = TVCL * EXP(ETA(1))", # log-normal
    "  V  = TVV + ETA(2)", # additive
    "  KA = EXP(THETA(3) * ETA(3))" # exp of a non-unit multiple -> not simple
  ))
  out <- createFREMParamFunction(bm,
    parameters = c("CL", "V", "KA"),
    numSkipOm = 0, numNonFREMThetas = 4, quiet = TRUE
  )
  expect_identical(out$fremEtaScale, c(CL = "exp", V = "other", KA = "other"))
})

test_that("verifyFREMParamFunction skips the splice for a non-log-normal parameter", {
  bm <- .stubMod(c(
    "  TVCL = THETA(1)", "  TVV = THETA(2)",
    "  CL = TVCL * EXP(ETA(1))", # log-normal
    "  V  = TVV + ETA(2)" # additive
  ))
  out <- createFREMParamFunction(bm,
    parameters = c("CL", "V"), numSkipOm = 0,
    numNonFREMThetas = 4, quiet = TRUE
  )
  v <- verifyFREMParamFunction(out, thetas = c(3, 5, 0, 0), quiet = TRUE)
  d <- attr(v, "checks")

  expect_true(d$PASS[d$PARAMETER == "CL"]) # fully checked
  expect_true(is.na(d$PASS[d$PARAMETER == "V"])) # splice skipped
  expect_true(is.na(d$COVSPLICE[d$PARAMETER == "V"]))
  expect_true(is.na(d$ETASPLICE[d$PARAMETER == "V"]))
  expect_false(is.na(d$STRUCTURAL[d$PARAMETER == "V"])) # structural still ran
  expect_true(d$STRUCTURAL[d$PARAMETER == "V"] <= 1e-6)

  expect_true(as.logical(v)) # nothing failed
  expect_output(print(v), "non-log-normal")
})

test_that("verifyFREMParamFunction still FALSE if a non-log-normal parameter is structurally wrong", {
  bm <- .stubMod(c(
    "  TVV = THETA(2)",
    "  V  = TVV + ETA(1)" # additive
  ))
  out <- createFREMParamFunction(bm,
    parameters = "V", numSkipOm = 0,
    numNonFREMThetas = 4, quiet = TRUE
  )
  # tamper: the generated V is `basethetas[2] + (...)`; make it basethetas[1]
  bad <- gsub("basethetas\\[2\\]", "basethetas[1]", paste(out$code, collapse = "\n"))
  v <- verifyFREMParamFunction(out,
    fun = eval(parse(text = bad)),
    thetas = c(9, 5, 0, 0), quiet = TRUE
  )
  expect_false(as.logical(v))
  expect_false(attr(v, "checks")$PASS[1]) # STRUCTURAL fails -> PASS FALSE, not NA
})

test_that("a parameter with no ETA() is returned as-is, not an error", {
  bm <- .stubMod(c(
    "  TVCL = THETA(1)", "  CL = TVCL",
    "  TVV = THETA(2)", "  V  = TVV * EXP(ETA(1))"
  ))
  out <- createFREMParamFunction(bm,
    parameters = c("V", "CL"), numSkipOm = 0,
    numNonFREMThetas = 4, quiet = TRUE
  )
  expect_identical(out$fremParameters, "V")
  expect_equal(out$numParCov, 1)
  code <- paste(out$code, collapse = "\n")
  expect_match(code, "V <- TVV \\* exp\\(\\(covthetas\\[1\\] \\+ .eta\\(etas, 1\\)\\)\\)")
  expect_match(code, "CL <- TVCL   # returned as-is")

  fn <- eval(parse(text = out$code))
  r <- fn(basethetas = c(3, 5), covthetas = 0.2, dfrow = data.frame(), etas = 0)
  expect_equal(r$CL, 3)
  expect_equal(r$V, 5 * exp(0.2))
})

test_that("a $PK assignment referencing ETA() more than once is returned as-is with a warning", {
  bm <- .stubMod(c("  TVCL = THETA(1)", "  CL = TVCL * EXP(ETA(1) + ETA(2))"))
  expect_warning(
    out <- createFREMParamFunction(bm,
      parameters = "CL", numSkipOm = 0,
      numNonFREMThetas = 4, quiet = TRUE
    ),
    "references ETA\\(\\) 2 times"
  )
  expect_length(out$fremParameters, 0)
  expect_match(
    paste(out$code, collapse = "\n"),
    "returned as-is; ETA\\(\\) -> 0"
  )
})

test_that("the ETA index in $PK decides the FREM index, not the request order", {
  # CL is the only requested parameter, but it carries ETA(4). With
  # numSkipOm = 0 that makes it the model's 4th FREM parameter, so it takes
  # covthetas[4] and etas[4] - not covthetas[1] and etas[1], which is what
  # indexing by position in `parameters` used to produce.
  bm <- .stubMod(c("  TVCL = THETA(1)", "  CL = TVCL * EXP(ETA(4))"))
  expect_no_warning(
    out <- createFREMParamFunction(bm,
      parameters = "CL", numSkipOm = 0,
      numNonFREMThetas = 4, quiet = TRUE
    )
  )
  expect_match(
    paste(out$code, collapse = "\n"),
    "CL <- TVCL \\* exp\\(\\(covthetas\\[4\\] \\+ .eta\\(etas, 4\\)\\)\\)"
  )
})

test_that("a parameter whose eta is inside the skipped omegas is not a FREM parameter", {
  bm <- .stubMod(c("  TVCL = THETA(1)", "  CL = TVCL * EXP(ETA(1))"))
  out <- createFREMParamFunction(bm,
    parameters = "CL", numSkipOm = 2,
    numNonFREMThetas = 4, quiet = TRUE
  )
  expect_length(out$fremParameters, 0)
  expect_match(
    paste(out$code, collapse = "\n"),
    "CL <- TVCL \\* exp\\(.eta\\(etas, 1\\)\\)"
  )
})

# ---------------------------------------------------------------------------
# the generated function behaves correctly
# ---------------------------------------------------------------------------

test_that("with covthetas = 0 and etas = 0 the FREM function equals the SCM typical values", {
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE
  )
  fn <- eval(parse(text = out$code))

  ## The reference is the FFEM model, not the FREM one: PMXForest refuses a
  ## FREM model, and the FFEM $PK is the same algebra with the covariate effect
  ## as an additive term inside the ETA's EXP(). At 0 they coincide.
  scm <- PMXForest::createParamFunction(.ffemMod(),
    parameters = c("CL", "V", "MAT"),
    covRef = list(CLFREMCOV = 0, VFREMCOV = 0, MATFREMCOV = 0),
    quiet = TRUE
  )
  scmFn <- eval(parse(text = scm$code))

  full <- .finals()
  bth <- full[seq_len(out$noBaseThetas)]
  sth <- full[seq_len(scm$noBaseThetas)]
  for (FOOD in c(0, 1)) {
    dfrow <- data.frame(FOOD = FOOD)
    a <- fn(bth, covthetas = c(0, 0, 0), dfrow = dfrow, etas = rep(0, 5))
    b <- scmFn(thetas = sth, df = dfrow)
    expect_equal(unlist(a), unlist(b), tolerance = 1e-10)
  }
})

test_that("covthetas[k] / etas[numSkipOm+k] scale only parameter k, by exp()", {
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE
  )
  fn <- eval(parse(text = out$code))
  bth <- .finals()[seq_len(out$noBaseThetas)]
  dfrow <- data.frame(FOOD = 1)
  base <- unlist(fn(bth, covthetas = c(0, 0, 0), dfrow = dfrow, etas = rep(0, 5)))

  ct <- c(0.11, -0.07, 0.2)
  got <- unlist(fn(bth, covthetas = ct, dfrow = dfrow, etas = rep(0, 5)))
  expect_equal(unname(got / base), exp(ct), tolerance = 1e-10)

  for (k in 1:3) {
    e <- rep(0, 5)
    e[2 + k] <- 0.25
    g <- unlist(fn(bth, covthetas = c(0, 0, 0), dfrow = dfrow, etas = e))
    exp_k <- base
    exp_k[k] <- exp_k[k] * exp(0.25)
    expect_equal(unname(g), unname(exp_k), tolerance = 1e-10)
  }
  # An etas vector too short for the ETA() the model references used to become
  # a silent zero, which reads as "this subject has no random effect" - a wrong
  # number rather than a missing one. It is an error now.
  expect_error(
    fn(bth, covthetas = c(0, 0, 0), dfrow = dfrow, etas = numeric(0)),
    "etas has 0 element\\(s\\), but ETA\\(3\\) is referenced"
  )
})

# ---------------------------------------------------------------------------
# verifyFREMParamFunction()
# ---------------------------------------------------------------------------

test_that("a FREM model is refused as its own structural reference", {
  ## Without `ffemModel` the reference defaults to x$fremModel, which for a FREM
  ## model PMXForest::createParamFunction() refuses - correctly, since a FREM
  ## model's covariate effects are in $OMEGA. The error must name the way out
  ## rather than let PMXForest's refusal surface from two levels down.
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE
  )
  e <- tryCatch(
    verifyFREMParamFunction(out, extFile = .fremExt(), quiet = TRUE),
    error = conditionMessage
  )
  expect_match(e, "FREM model")
  expect_match(e, "ffemModel")
  expect_match(e, "createFFEMmodel")

  ## and a path that does not exist is caught before PMXForest sees it
  expect_error(
    verifyFREMParamFunction(out, ffemModel = "no/such/model.mod", quiet = TRUE),
    "does not exist"
  )
})

test_that("verifyFREMParamFunction returns a scalar TRUE for a faithful function", {
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE
  )
  v <- verifyFREMParamFunction(out, ffemModel = .ffemMod(), extFile = .fremExt(), quiet = TRUE)

  expect_length(as.logical(v), 1L)
  expect_true(as.logical(v))
  expect_true(if (v) TRUE else FALSE) # usable in an if

  d <- attr(v, "checks")
  expect_s3_class(d, "data.frame")
  expect_identical(d$PARAMETER, c("CL", "V", "MAT"))
  expect_true(all(d$PASS))
  expect_true(all(d$STRUCTURAL < 1e-8))
})

test_that("verifyFREMParamFunction returns FALSE and flags the tampered parameter", {
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE
  )
  bad <- gsub("covthetas\\[1\\]", "2 * covthetas[1]", paste(out$code, collapse = "\n"))
  v <- verifyFREMParamFunction(out,
    ffemModel = .ffemMod(),
    fun = eval(parse(text = bad)),
    extFile = .fremExt(), quiet = TRUE
  )
  expect_false(as.logical(v))
  d <- attr(v, "checks")
  expect_false(d$PASS[d$PARAMETER == "CL"])
  expect_true(all(d$PASS[d$PARAMETER != "CL"]))
})

test_that("verifyFREMParamFunction ignores secondary parameters", {
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE,
    secondary = list(AUC = "80 / CL")
  )
  v <- verifyFREMParamFunction(out, ffemModel = .ffemMod(), extFile = .fremExt(), quiet = TRUE)
  expect_true(as.logical(v))
  expect_identical(attr(v, "checks")$PARAMETER, c("CL", "V", "MAT")) # no AUC
})

test_that("verifyFREMParamFunction rejects an object it did not produce", {
  expect_error(
    verifyFREMParamFunction(list(code = "1")),
    "must be the list returned by createFREMParamFunction"
  )
  expect_error(
    verifyFREMParamFunction(42),
    "must be the list returned by createFREMParamFunction"
  )
})

test_that("verifyFREMParamFunction derives the extFile from the model when thetas are absent", {
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE
  )
  # neither `thetas` nor `extFile` supplied: it should find run31.ext beside the model
  v <- verifyFREMParamFunction(out, ffemModel = .ffemMod(), quiet = TRUE)
  expect_true(as.logical(v))
})

test_that("verifyFREMParamFunction errors when neither thetas nor a usable extFile exist", {
  td <- withr::local_tempdir()
  mod <- file.path(td, "lonely.mod")
  file.copy(.fremMod(), mod) # model copied, no .ext beside it
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE
  )
  out$fremModel <- mod # point at the ext-less copy
  expect_error(
    verifyFREMParamFunction(out, ffemModel = .ffemMod(), quiet = TRUE),
    "Supply `thetas`, or an `extFile`"
  )
})

test_that("verifyFREMParamFunction reports each parameter when quiet = FALSE", {
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE
  )
  expect_message(
    verifyFREMParamFunction(out, ffemModel = .ffemMod(), extFile = .fremExt(), quiet = FALSE),
    "parameter\\(s\\) pass"
  )
  expect_message(
    verifyFREMParamFunction(out, ffemModel = .ffemMod(), extFile = .fremExt(), quiet = FALSE),
    "CL: pass"
  )
})

# ---------------------------------------------------------------------------
# secondary parameters
# ---------------------------------------------------------------------------

test_that("a secondary snippet is spliced in, returned, and listed", {
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE,
    secondary = list(
      AUC = "dfrow$DOSE / CL",
      KEL = "CL / V"
    )
  )
  expect_identical(out$functionListName, c("CL", "V", "MAT", "AUC", "KEL"))
  expect_identical(out$primaryNames, c("CL", "V", "MAT"))
  expect_identical(out$secondaryNames, c("AUC", "KEL"))

  code <- paste(out$code, collapse = "\n")
  expect_match(code, "df <- dfrow") # alias emitted
  expect_match(code, "AUC <- local\\(\\{ dfrow\\$DOSE / CL \\}\\)")

  fn <- eval(parse(text = out$code))
  bth <- .finals()[seq_len(out$noBaseThetas)]
  v <- fn(
    basethetas = bth, covthetas = c(0, 0, 0),
    dfrow = data.frame(DOSE = 100), etas = rep(0, out$numSkipOm + 3)
  )
  expect_named(v, c("CL", "V", "MAT", "AUC", "KEL"))
  expect_equal(v$AUC, 100 / v$CL)
  expect_equal(v$KEL, v$CL / v$V)
})

test_that("a config-list secondary binds its constants ahead of the source", {
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE,
    secondary = list(
      AUC = list(source = "dose / CL", dose = 240)
    )
  )
  expect_identical(out$functionListName, c("CL", "V", "MAT", "AUC"))
  code <- paste(out$code, collapse = "\n")
  expect_match(code, "AUC <- local\\(\\{\\n\\s*dose <- 240")

  fn <- eval(parse(text = out$code))
  bth <- .finals()[seq_len(out$noBaseThetas)]
  v <- fn(
    basethetas = bth, covthetas = c(0, 0, 0), dfrow = data.frame(),
    etas = rep(0, out$numSkipOm + 3)
  )
  expect_equal(v$AUC, 240 / v$CL)
})

test_that("a secondary from a file is inlined verbatim and survives file removal", {
  rf <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "## model string must not be re-indented",
    "code <- \"",
    "$PARAM CL=1",
    "\"",
    "nchar(code)"
  ), rf)
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE,
    secondary = list(NC = rf)
  )
  code <- paste(out$code, collapse = "\n")
  expect_match(code, "\\n\\$PARAM CL=1\\n") # column-0, not indented
  file.remove(rf)
  fn <- eval(parse(text = out$code))
  bth <- .finals()[seq_len(out$noBaseThetas)]
  v <- fn(
    basethetas = bth, covthetas = c(0, 0, 0), dfrow = data.frame(),
    etas = rep(0, out$numSkipOm + 3)
  )
  expect_true(is.finite(v$NC))
})

test_that("a generated function with a secondary drives getForestDFFREM()", {
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE,
    secondary = list(AUC = "80 / CL")
  )
  fn <- eval(parse(text = out$code))

  covNames <- getCovNames(.fremMod())
  dfCovs <- data.frame(WT = c(60, 90), AGE = c(-99, -99))
  set.seed(1)
  samples <- PMXForest::getSamples(
    system.file("extdata/SimNeb/bs31.dir/raw_results_run31.csv", package = "PMXFrem"),
    extFile = .fremExt(), n = 10
  )

  res <- suppressWarnings(getForestDFFREM(
    dfCovs = dfCovs, covNames = covNames$covNames, functionList = list(fn),
    functionListName = out$functionListName, numNonFREMThetas = 7, numSkipOm = 2,
    dfParameters = samples, quiet = TRUE, cstrPackages = c("PMXFrem", "dplyr")
  ))
  expect_setequal(as.character(unique(res$PARAMETER)), c("CL", "V", "MAT", "AUC"))
  expect_true(all(is.finite(res$POINT)))
})

# ---------------------------------------------------------------------------
# Integration: the generated function drives getForestDFFREM / getExplainedVar
# ---------------------------------------------------------------------------

test_that("a generated function works inside getForestDFFREM()", {
  modDevDir <- system.file("extdata/SimNeb/", package = "PMXFrem")

  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE
  )
  fn <- eval(parse(text = out$code))

  covNames <- getCovNames(.fremMod())
  dfCovs <- data.frame(WT = c(60, 90), AGE = c(-99, -99))
  set.seed(1)
  samples <- PMXForest::getSamples(
    system.file("extdata/SimNeb/bs31.dir/raw_results_run31.csv", package = "PMXFrem"),
    extFile = .fremExt(), n = 10
  )

  res <- suppressWarnings(getForestDFFREM(
    dfCovs = dfCovs, covNames = covNames$covNames, functionList = list(fn),
    functionListName = c("CL", "V", "MAT"), numNonFREMThetas = 7, numSkipOm = 2,
    dfParameters = samples, quiet = TRUE, cstrPackages = c("PMXFrem", "dplyr")
  ))
  expect_s3_class(res, "data.frame")
  expect_setequal(as.character(unique(res$PARAMETER)), c("CL", "V", "MAT"))
  expect_true(all(is.finite(res$POINT)))
})

test_that("a generated function works inside getExplainedVar() (etas != 0 path)", {
  modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")

  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "V", "MAT"),
    extFile = .fremExt(), quiet = TRUE
  )
  fn <- eval(parse(text = out$code))

  dfCovs <- setupDfCovsEV(.fremMod())
  ev <- getExplainedVar(
    type = 0, data = NULL, dfCovs = dfCovs, functionList = list(fn),
    functionListName = c("CL", "V", "MAT"),
    cstrCovariates = c("All", names(dfCovs)),
    modDevDir = modDevDir, runno = 31, ncores = 1, quiet = TRUE, seed = 123
  )
  expect_s3_class(ev, "data.frame")
  expect_true(all(is.finite(ev$TOTVAR)))
})

# ---------------------------------------------------------------------------
# covthetas / etas are indexed by the MODEL, not by the request
#
# `covthetas` is the FFEM-projected coefficient vector for the model's FREM
# parameters, and `etas` carries the model's random effects - both of them
# model-length, whatever subset of parameters was asked for. See the
# hand-written functions in getExplainedVar()'s own documentation:
# basethetas[2] * exp(covthetas[1] + etas[3]) with numSkipOm = 2.
# ---------------------------------------------------------------------------

.paramLine <- function(x, p) {
  grep(paste0("^\\s*", p, " <- "), x$code, value = TRUE)
}

test_that("a subset or reordered request emits the same code as the full request", {
  m <- .fremMod()
  e <- .fremExt()

  full <- createFREMParamFunction(m,
    parameters = c("CL", "V", "MAT"), extFile = e, quiet = TRUE
  )
  sub <- createFREMParamFunction(m,
    parameters = c("V", "MAT"), extFile = e, quiet = TRUE
  )
  rev <- createFREMParamFunction(m,
    parameters = c("MAT", "V"), extFile = e, quiet = TRUE
  )

  for (p in c("V", "MAT")) {
    expect_identical(.paramLine(sub, p), .paramLine(full, p))
    expect_identical(.paramLine(rev, p), .paramLine(full, p))
  }

  # V carries ETA(4) in run31.mod, so it is the model's 2nd FREM parameter
  # (numSkipOm = 2) however few parameters were requested.
  expect_match(.paramLine(sub, "V"), "covthetas\\[2\\]")
  expect_match(.paramLine(sub, "V"), "\\.eta\\(etas, 4\\)")
  expect_match(.paramLine(rev, "MAT"), "covthetas\\[3\\]")
  expect_match(.paramLine(rev, "MAT"), "\\.eta\\(etas, 5\\)")

  # and the recorded shape describes the model, not the request
  expect_equal(sub$numParCov, full$numParCov)
  expect_equal(rev$numParCov, full$numParCov)
})

test_that("a subset request evaluates to the same numbers as the full request", {
  m <- .fremMod()
  e <- .fremExt()
  th <- as.numeric(getExt(extFile = e)[1, grep(
    "^THETA", names(getExt(extFile = e))
  ), drop = TRUE])

  full <- createFREMParamFunction(m,
    parameters = c("CL", "V", "MAT"), extFile = e, quiet = TRUE
  )
  sub <- createFREMParamFunction(m,
    parameters = c("V", "MAT"), extFile = e, quiet = TRUE
  )
  fFull <- eval(parse(text = full$code))
  fSub <- eval(parse(text = sub$code))

  base <- th[seq_len(full$noBaseThetas)]
  ct <- c(0.11, 0.22, 0.33) # model FREM order: CL, V, MAT
  et <- rep(0, 23)
  et[4] <- 0.3 # V's eta in the model
  dfrow <- as.data.frame(
    stats::setNames(
      as.list(rep(full$missVal, length(full$covRef))),
      names(full$covRef)
    )
  )

  a <- fFull(base, covthetas = ct, dfrow = dfrow, etas = et)
  b <- fSub(base, covthetas = ct, dfrow = dfrow, etas = et)
  expect_equal(b$V, a$V)
  expect_equal(b$MAT, a$MAT)
  # and the eta actually moved V, not something else
  expect_equal(b$V / fSub(base,
    covthetas = ct, dfrow = dfrow,
    etas = rep(0, 23)
  )$V, exp(0.3))
})

test_that("verifyFREMParamFunction passes on a subset request, and probes the model's indices", {
  m <- .fremMod()
  e <- .fremExt()
  ffem <- system.file("extdata/SimNeb/run31max1-2.mod", package = "PMXFrem")

  sub <- createFREMParamFunction(m,
    parameters = c("V", "MAT"), extFile = e, quiet = TRUE
  )
  v <- verifyFREMParamFunction(sub, ffemModel = ffem, extFile = e, quiet = TRUE)
  expect_true(isTRUE(unclass(v)[1]))
  d <- attr(v, "checks")
  expect_true(all(d$STRUCTURAL < 1e-8))
  expect_true(all(d$COVSPLICE < 1e-8))
  expect_true(all(d$ETASPLICE < 1e-8))
})

test_that("verifyFREMParamFunction catches a function that indexes by request position", {
  # The defect this check exists for: emit covthetas/etas by rank within
  # `parameters` instead of by the model's own FREM numbering. Built here by
  # hand so the check is exercised even if the emitter never regresses.
  m <- .fremMod()
  e <- .fremExt()
  ffem <- system.file("extdata/SimNeb/run31max1-2.mod", package = "PMXFrem")

  good <- createFREMParamFunction(m,
    parameters = c("V", "MAT"), extFile = e, quiet = TRUE
  )
  bad <- good
  # V's splice first, then MAT's, so the second does not re-hit the first
  bad$code <- sub("covthetas[2] + .eta(etas, 4)",
    "covthetas[1] + .eta(etas, 3)", bad$code,
    fixed = TRUE
  )
  bad$code <- sub("covthetas[3] + .eta(etas, 5)",
    "covthetas[2] + .eta(etas, 4)", bad$code,
    fixed = TRUE
  )
  # the mutation has to have applied, or a PASS below would mean nothing
  expect_false(identical(bad$code, good$code))
  expect_match(paste(bad$code, collapse = "\n"), "V <- exp\\(MU_4 \\+ \\(covthetas\\[1\\]")

  v <- verifyFREMParamFunction(bad, ffemModel = ffem, extFile = e, quiet = TRUE)
  expect_false(isTRUE(unclass(v)[1]))
})

# ---------------------------------------------------------------------------
# Which ETA() in which statement gets the covariate splice
# ---------------------------------------------------------------------------

.fremModWith <- function(td, after, newline) {
  l <- readLines(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"),
    warn = FALSE
  )
  i <- grep(after, l, fixed = TRUE)[1]
  l <- append(l, newline, after = i)
  f <- file.path(td, "run31x.mod")
  writeLines(l, f)
  file.copy(.fremExt(), file.path(td, "run31x.ext"))
  f
}

test_that("a second assignment to a FREM parameter is not given the covariate splice", {
  # IF(OCC.EQ.2) CL = CL * EXP(ETA(2)) is an IOV term, not CL's FREM eta.
  # Splicing it emitted covthetas[0], which is numeric(0), so CL vanished
  # from the returned list for every OCC == 2 row - no error, no warning.
  td <- withr::local_tempdir()
  m <- .fremModWith(
    td, "CL    = EXP(MU_3               + ETA(3))",
    "IF(OCC.EQ.2) CL = CL * EXP(ETA(2))"
  )
  out <- suppressWarnings(createFREMParamFunction(m,
    parameters = c("CL", "V", "MAT"), covRef = list(OCC = 1), quiet = TRUE
  ))
  code <- paste(out$code, collapse = "\n")
  expect_no_match(code, "covthetas[0]", fixed = TRUE)

  fn <- eval(parse(text = out$code))
  th <- .finals()[seq_len(out$noBaseThetas)]
  r <- fn(th,
    covthetas = c(0, 0, 0), dfrow = data.frame(FOOD = 1, OCC = 2),
    etas = rep(0, 23)
  )
  expect_named(r, c("CL", "V", "MAT"))
  expect_true(is.finite(r$CL))
})

test_that("a FREM parameter's own eta mixed with another eta is refused", {
  # EXP(MU_3 + ETA(3) + ETA(2)) cannot have the covariate coefficient spliced
  # in place: nmDeparse() replaces every ETA in the expression, so the
  # coefficient would be counted twice and the IOV eta would become the IIV.
  td <- withr::local_tempdir()
  m <- .fremModWith(
    td, "CL    = EXP(MU_3               + ETA(3))",
    "IF(FOOD.EQ.0) CL = EXP(MU_3 + ETA(3) + ETA(2))"
  )
  expect_error(
    createFREMParamFunction(m,
      parameters = c("CL", "V", "MAT"), quiet = TRUE
    ),
    "ETA\\(3\\), ETA\\(2\\)|together with"
  )
})

test_that("a parameter whose eta is in the skip region keeps its own eta", {
  # D1FR = MU_2 + ETA(2) with numSkipOm = 2. It is not a FREM covariate
  # parameter - no covthetas index applies - but its eta is real, and
  # explained-variability work needs it. It used to be spliced with the wrong
  # covthetas index, and then (briefly) refused outright.
  out <- createFREMParamFunction(.fremMod(),
    parameters = c("CL", "D1FR"), extFile = .fremExt(), quiet = TRUE
  )
  code <- paste(out$code, collapse = "\n")
  expect_match(code, "D1FR <- MU_2 \\+ .eta\\(etas, 2\\)")
  expect_no_match(code, "D1FR <- MU_2 \\+ \\(covthetas")
  expect_false("D1FR" %in% out$fremParameters)

  fn <- eval(parse(text = out$code))
  th <- .finals()[seq_len(out$noBaseThetas)]
  e <- rep(0, 23)
  e[2] <- 0.4
  a <- fn(th, covthetas = c(0, 0, 0), dfrow = data.frame(FOOD = 1), etas = rep(0, 23))
  b <- fn(th, covthetas = c(0, 0, 0), dfrow = data.frame(FOOD = 1), etas = e)
  expect_equal(b$D1FR - a$D1FR, 0.4, tolerance = 1e-12)
})

test_that("verifyFREMParamFunction derives numSkipOm from the reference and fails on a mismatch", {
  # The probe index is etaIdx - numSkipOm. Taking numSkipOm from `x` means
  # inheriting the generator's own belief about it, so a wrong numSkipOm
  # produced a function every real caller would index wrongly - and verify
  # said PASS. The FFEM reference's own $OMEGA records have the answer.
  m <- .fremMod()
  e <- .fremExt()
  ffem <- system.file("extdata/SimNeb/run31max1-2.mod", package = "PMXFrem")

  out <- suppressWarnings(createFREMParamFunction(m,
    parameters = c("CL", "V", "MAT"), numSkipOm = 1, numNonFREMThetas = 7,
    extFile = e, quiet = TRUE
  ))
  v <- verifyFREMParamFunction(out, ffemModel = ffem, extFile = e, quiet = TRUE)
  expect_false(isTRUE(unclass(v)[1]))
  expect_equal(attr(v, "numSkipOm")$reference, 2L)
  expect_equal(attr(v, "numSkipOm")$object, 1)
})

test_that("verifyFREMParamFunction pins every FREMCOV column the reference needs", {
  # KA depends on MAT, so run31max1-2.mod's $PK reads MATFREMCOV even though
  # MAT was not requested. Pinning only <requested>FREMCOV left it unresolved
  # and the whole check errored out.
  m <- .fremMod()
  e <- .fremExt()
  ffem <- system.file("extdata/SimNeb/run31max1-2.mod", package = "PMXFrem")

  # KA carries no ETA() of its own; asking for it without MAT means its
  # covariate effect is dropped, and that is now said out loud.
  expect_warning(
    out <- createFREMParamFunction(m,
      parameters = c("FREL", "KA", "CL"), extFile = e, quiet = TRUE
    ),
    "'KA' has no ETA\\(\\) of its own"
  )
  expect_error(
    verifyFREMParamFunction(out, ffemModel = ffem, extFile = e, quiet = TRUE),
    NA
  )
})

test_that("a parameter whose ETA sits behind an intermediate variable is flagged", {
  # ETACL = ETA(3) / CL = EXP(MU_3 + ETACL) is legal $PK. CL's own assignment
  # carries no ETA(), so it was emitted "returned as-is (no IIV / no FREM
  # covariate effect)" - silently dropping CL's covariate effect - and
  # verifyFREMParamFunction() then reported it as "not checked
  # (non-log-normal)" and still returned TRUE.
  td <- withr::local_tempdir()
  l <- readLines(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"),
    warn = FALSE
  )
  i <- grep("CL    = EXP(MU_3               + ETA(3))", l, fixed = TRUE)
  expect_length(i, 1)
  l[i] <- "ETACL = ETA(3)\nCL    = EXP(MU_3 + ETACL)"
  l <- unlist(strsplit(l, "\n"))
  f <- file.path(td, "indirect.mod")
  writeLines(l, f)
  file.copy(.fremExt(), file.path(td, "indirect.ext"))

  expect_warning(
    createFREMParamFunction(f,
      parameters = c("CL", "V", "MAT"), quiet = TRUE
    ),
    "CL.*ETACL|ETACL.*ETA"
  )
})

test_that("a repeated name in `parameters` is reduced to one", {
  expect_warning(
    createFREMParamFunction(.fremMod(),
      parameters = c("CL", "CL", "V"), extFile = .fremExt(), quiet = TRUE
    ),
    "repeated"
  )
  out <- suppressWarnings(createFREMParamFunction(.fremMod(),
    parameters = c("CL", "CL", "V"), extFile = .fremExt(), quiet = TRUE
  ))
  expect_identical(out$primaryNames, c("CL", "V"))
  expect_equal(sum(grepl("^\\s+CL = CL,?$", out$code)), 1)
})
