# addFremIIV() and addFremStructuralTheta() - insert-in-place extension of an
# established FREM model, with a full ETA / MU_ / COV / THETA renumber pass.

.run31 <- function(td) {
  file.copy(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"), td)
  file.path(td, "run31.mod")
}

# largest ETA()/THETA() index actually referenced (guarded against THETA( -> ETA()
.maxIdx <- function(lines, token) {
  rx <- sprintf("(?<![A-Za-z])%s\\([0-9]+\\)", token)
  hit <- unlist(regmatches(lines, gregexpr(rx, lines, perl = TRUE)))
  if (!length(hit)) {
    return(0L)
  }
  max(as.integer(gsub("[^0-9]", "", hit)))
}
.pkErr <- function(lines) {
  s <- grep("^\\s*\\$PK", lines)
  e <- grep("^\\s*\\$THETA", lines)[1] - 1L
  lines[s:e]
}


# ---------------------------------------------------------------------------
# addFremIIV()
# ---------------------------------------------------------------------------

test_that("addFremIIV inserts ETA(numSkipOm+1) and shifts everything at/after it", {
  td <- withr::local_tempdir()
  res <- addFremIIV(.run31(td), parameter = "FREL", omegaInit = 0.04, quiet = TRUE)

  expect_equal(res$etaIndex, 3L) # numSkipOm (2) + 1
  expect_equal(res$numSkipOm, 3L)
  expect_equal(res$numNonFREMThetas, 7L) # untouched

  pk <- .pkErr(res$model)
  # skip-region etas ETA(1), ETA(2) untouched; old ETA(3..23) -> ETA(4..24)
  expect_match(paste(pk, collapse = "\n"), "D1FR\\s*=\\s*MU_2\\s*\\+\\s*ETA\\(2\\)")
  expect_match(paste(pk, collapse = "\n"), "CL\\s*=\\s*EXP\\(MU_4\\s*\\+\\s*ETA\\(4\\)\\)")
  # the new IIV on FREL, log-normal by default
  expect_match(
    paste(pk, collapse = "\n"),
    "FREL\\s*=\\s*\\(TVFREL\\*FRELCOVTIME\\) \\* EXP\\(ETA\\(3\\)\\)"
  )
  # ETA indices are 1..24 with no gap
  etaIdx <- sort(unique(as.integer(gsub("[^0-9]", "", unlist(regmatches(
    res$model, gregexpr("(?<![A-Za-z])ETA\\([0-9]+\\)", res$model, perl = TRUE)
  ))))))
  expect_equal(etaIdx, 1:24)
})

test_that("addFremIIV leaves THETA() references and the FREM BLOCK(N) untouched", {
  td <- withr::local_tempdir()
  base <- readLines(.run31(td))
  res <- addFremIIV(file.path(td, "run31.mod"),
    parameter = "FREL",
    omegaInit = 0.04, quiet = TRUE
  )

  expect_equal(.maxIdx(res$model, "THETA"), .maxIdx(base, "THETA")) # 25, unchanged
  expect_match(paste(res$model, collapse = "\n"), "\\$OMEGA\\s+BLOCK\\(21\\)")
  # exactly one new simple $OMEGA, just before the block
  om <- grep("^\\s*\\$OMEGA", res$model, value = TRUE)
  expect_match(om[3], "^\\$OMEGA\\s+0\\.04\\b.*IIV on FREL")
  expect_match(om[4], "BLOCK\\(21\\)")
})

test_that("addFremIIV link = 'add' and link = 'none'", {
  td <- withr::local_tempdir()
  m <- .run31(td)
  add <- addFremIIV(m, parameter = "FREL", omegaInit = 0.01, link = "add", quiet = TRUE)
  expect_match(
    paste(.pkErr(add$model), collapse = "\n"),
    "FREL\\s*=\\s*TVFREL\\*FRELCOVTIME \\+ ETA\\(3\\)"
  )

  none <- addFremIIV(m, omegaInit = 0.02, link = "none", quiet = TRUE)
  baseFrel <- grep("^FREL\\b", readLines(m), value = TRUE)
  expect_identical(grep("^FREL\\b", none$model, value = TRUE), baseFrel) # $PK line untouched
  expect_true(any(grepl("\\$OMEGA\\s+0\\.02", none$model))) # $OMEGA still added
  # ETA / MU_ / COV renumber still happened
  expect_match(paste(none$model, collapse = "\n"), "CL\\s*=\\s*EXP\\(MU_4\\s*\\+\\s*ETA\\(4\\)\\)")
})

test_that("addFremIIV validates its inputs", {
  td <- withr::local_tempdir()
  m <- .run31(td)
  expect_error(addFremIIV(m, parameter = "FREL"), "`omegaInit` must be a single positive number")
  expect_error(addFremIIV(m, parameter = "FREL", omegaInit = -1), "positive number")
  expect_error(addFremIIV(m, omegaInit = 0.01), "`parameter`.*is required unless link")
  expect_error(
    addFremIIV(m, parameter = "NOSUCH", omegaInit = 0.01),
    "no \\$PK assignment of 'NOSUCH'"
  )
})

test_that("addFremIIV writes <mod>_iiv.mod and the result still parses as a FREM model", {
  td <- withr::local_tempdir()
  m <- .run31(td)
  res <- addFremIIV(m, parameter = "FREL", omegaInit = 0.04, quiet = TRUE)
  expect_true(file.exists(res$file))
  expect_match(res$file, "run31_iiv\\.mod$")
  # getCovNames still sees all 18 FREM covariates
  expect_length(getCovNames(res$file)$covNames, 18L)
})


# ---------------------------------------------------------------------------
# addFremStructuralTheta()
# ---------------------------------------------------------------------------

test_that("addFremStructuralTheta (THETA only) inserts at numNonFREMThetas+1 and renumbers refs", {
  td <- withr::local_tempdir()
  res <- addFremStructuralTheta(.run31(td),
    thetaInit = c(0, 0.5, 10),
    label = "TV_EXTRA", quiet = TRUE
  )

  expect_equal(res$thetaIndex, 8L)
  expect_true(is.na(res$etaIndex))
  expect_equal(res$numNonFREMThetas, 8L)

  txt <- paste(res$model, collapse = "\n")
  # the new record sits between the last structural theta and the first FREM one
  li <- res$model
  newRec <- grep("\\(0,0.5,10\\) ; 8\\. TV_EXTRA", li)
  expect_length(newRec, 1L)
  expect_match(li[newRec - 1L], "MATFOOD1")
  expect_match(li[newRec + 1L], "TV_WT")
  # structural refs THETA(1..7) untouched; FREM MU block THETA(8..25) -> THETA(9..26)
  expect_match(txt, "IF\\(FOOD\\.EQ\\.0\\) MATFOOD = \\( 1 \\+ THETA\\(6\\)\\)")
  expect_match(txt, "MU_6 = THETA\\(9\\)") # was MU_6 = THETA(8)
  expect_match(txt, "MU_23 = THETA\\(26\\)") # was MU_23 = THETA(25)
  expect_no_match(txt, "THETA\\(27\\)")
})

test_that("addFremStructuralTheta with addEta + muReference wires MU_k = THETA(j); param = EXP(MU_k + ETA(k))", {
  td <- withr::local_tempdir()
  res <- addFremStructuralTheta(.run31(td),
    thetaInit = c(0, 0.5, 10),
    parameter = "KANEW", addEta = TRUE,
    omegaInit = 0.09, quiet = TRUE
  )

  expect_equal(res$thetaIndex, 8L)
  expect_equal(res$etaIndex, 3L)
  expect_equal(res$numNonFREMThetas, 8L)
  expect_equal(res$numSkipOm, 3L)

  pk <- paste(.pkErr(res$model), collapse = "\n")
  # house style, as run31 writes CL/V/MAT: TV<par> / MU_k = LOG(TV<par>) / EXP()
  expect_match(pk, "TVKANEW = THETA\\(8\\)")
  expect_match(pk, "MU_3 = LOG\\(TVKANEW\\)")
  expect_match(pk, "KANEW = EXP\\(MU_3 \\+ ETA\\(3\\)\\)")
  # the added block is delimited and sits before the FREM MU block
  li <- res$model
  expect_lt(grep(";; Begin added THETA", li), min(grep("MU_7 = THETA\\(9\\)", li)))
  expect_length(grep(";; Begin added THETA", li), 1L)
  expect_length(grep(";; End added THETA", li), 1L)
  # new $OMEGA for the IIV, before BLOCK(21)
  om <- grep("^\\s*\\$OMEGA", li, value = TRUE)
  expect_match(om[3], "0\\.09\\b.*IIV on KANEW")
  expect_match(om[4], "BLOCK\\(21\\)")
  # ETA + THETA index sets are both gap-free
  expect_equal(.maxIdx(li, "ETA"), 24L)
  expect_equal(.maxIdx(li, "THETA"), 26L)
})

test_that("the added definition survives generateFremModel()'s FREM-block splice", {
  # generateFremModel() locates the FREM block as
  #   min(grep("MU_\\d+ = THETA")) .. max(grep("COV\\d+ = MU_"))
  # and replaces that whole range. Emitting `MU_k = THETA(j)` for the added
  # parameter would match that grep, and - sitting before the FREM block - would
  # make min() point at it, so a later updateFREMmodel() would splice the new
  # parameter away. The LOG(TV) house-style form must not collide.
  td <- withr::local_tempdir()
  for (mu in c(TRUE, FALSE)) {
    res <- addFremStructuralTheta(.run31(td),
      thetaInit = c(0, 0.5, 10),
      parameter = "KANEW", addEta = TRUE,
      muReference = mu, omegaInit = 0.09,
      quiet = TRUE, bWriteMod = FALSE
    )
    li <- res$model
    spliceFrom <- min(grep("MU_\\d+ = THETA", li))
    spliceTo <- max(grep("COV\\d+ = MU_", li))
    expect_match(li[spliceFrom], "MU_7 = THETA\\(9\\)") # the FREM block
    expect_false(any(grepl("KANEW", li[spliceFrom:spliceTo]))) # not inside it
  }
})

test_that("addFremStructuralTheta with addEta + muReference = FALSE emits a single-line def", {
  td <- withr::local_tempdir()
  res <- addFremStructuralTheta(.run31(td),
    thetaInit = 0.5, parameter = "KANEW",
    addEta = TRUE, muReference = FALSE,
    omegaInit = 0.09, quiet = TRUE
  )
  pk <- paste(.pkErr(res$model), collapse = "\n")
  expect_match(pk, "KANEW = THETA\\(8\\) \\* EXP\\(ETA\\(3\\)\\)")
  expect_no_match(pk, "MU_3 = ")
})

test_that("addEta = FALSE defines a new parameter rather than erroring", {
  td <- withr::local_tempdir()
  res <- addFremStructuralTheta(.run31(td),
    thetaInit = c(0, 0.5, 10),
    parameter = "KANEW", addEta = FALSE, quiet = TRUE
  )
  pk <- paste(.pkErr(res$model), collapse = "\n")
  expect_match(pk, ";; Begin added THETA")
  expect_match(pk, "KANEW = THETA\\(8\\)")
  expect_true(is.na(res$etaIndex))
  expect_equal(res$numSkipOm, 2L) # no eta added
  expect_equal(res$numNonFREMThetas, 8L)
})

test_that("an existing $PK parameter is modified in place, not redefined", {
  td <- withr::local_tempdir()
  # FREL already exists in run31's $PK
  res <- addFremStructuralTheta(.run31(td),
    thetaInit = 0.5, parameter = "FREL",
    addEta = FALSE, quiet = TRUE
  )
  li <- res$model
  expect_length(grep("^\\s*FREL\\s*=", li), 1L) # still one assignment
  expect_match(grep("^\\s*FREL\\s*=", li, value = TRUE), "THETA\\(8\\)")
  expect_length(grep(";; Begin added THETA", li), 0L) # nothing inserted
})

test_that("addFremStructuralTheta validates its inputs", {
  td <- withr::local_tempdir()
  m <- .run31(td)
  expect_error(addFremStructuralTheta(m), "`thetaInit` is required")
  expect_error(
    addFremStructuralTheta(m, thetaInit = 0.5, addEta = TRUE),
    "`parameter` is required when addEta"
  )
  expect_error(
    addFremStructuralTheta(m, thetaInit = 0.5, parameter = "X", addEta = TRUE),
    "`omegaInit` is required when addEta"
  )
})

test_that("addFremStructuralTheta accepts a verbatim string thetaInit and writes <mod>_theta.mod", {
  td <- withr::local_tempdir()
  res <- addFremStructuralTheta(.run31(td),
    thetaInit = "(0, 1.2) FIX",
    label = "TV_X", quiet = TRUE
  )
  expect_match(
    paste(res$model, collapse = "\n"),
    "\\$THETA\\s+\\(0, 1\\.2\\) FIX ; 8\\. TV_X"
  )
  expect_match(res$file, "run31_theta\\.mod$")
  expect_true(file.exists(res$file))
})

# ---------------------------------------------------------------------------
# $OMEGA placement
#
# The new record must define ETA(numSkipOm + 1), so it belongs *after* every
# skip record. Anchoring on the first "$OMEGA BLOCK(" is only equivalent when
# the skip omegas are bare $OMEGA records, as in run31.mod. This package's own
# updateFREMmodel() writes them as $OMEGA BLOCK(1) - run31_new.mod - and there
# the anchor lands at index 1, permuting every existing IIV and giving the new
# one whatever the record it displaced was initialised to (1e-04 FIX: zero).
# ---------------------------------------------------------------------------

.run31new <- function(td) {
  file.copy(system.file("extdata/SimNeb/run31_new.mod", package = "PMXFrem"), td)
  file.copy(system.file("extdata/SimNeb/run31_new.ext", package = "PMXFrem"), td)
  file.path(td, "run31_new.mod")
}

.omegaRecs <- function(lines) grep("^\\s*\\$OMEGA", lines, value = TRUE)

test_that("addFremIIV inserts the new $OMEGA after the skip records, not before the first BLOCK(", {
  td <- withr::local_tempdir()
  res <- addFremIIV(.run31new(td), parameter = "FREL", omegaInit = 0.04, quiet = TRUE)

  expect_equal(res$etaIndex, 3L) # numSkipOm (2) + 1
  expect_equal(res$numSkipOm, 3L)

  recs <- .omegaRecs(res$model)

  # the two skip omegas keep their own initial values, FIX flags and comments
  expect_match(recs[1], "0\\.0541999")
  expect_match(recs[1], "IIV on RUV")
  expect_match(recs[2], "1e-04")
  expect_match(recs[2], "FIX")
  expect_match(recs[2], "IIV on D1")

  # the new record is third - ETA(3) - carries omegaInit, and is not FIX
  expect_match(recs[3], "0\\.04")
  expect_match(recs[3], "IIV on FREL")
  expect_false(grepl("FIX", recs[3]))

  # and the FREM block still follows it
  expect_match(recs[4], "BLOCK\\(20\\)")
})

test_that(".fremOmegaRecords counts the record shapes the bundled corpus does not contain", {
  # BLOCK(n), bare diagonals and BLOCK(n) SAME are all present in inst/extdata
  # and are covered by the models above. These three are not, so they are
  # asserted directly rather than left to chance.
  bareSame <- c(
    "$OMEGA BLOCK(2) 0.1 0.01 0.2",
    "$OMEGA BLOCK SAME",
    "$SIGMA 1"
  )
  expect_equal(PMXFrem:::.fremOmegaRecords(bareSame)$n, c(2L, 2L))

  expect_equal(
    PMXFrem:::.fremOmegaRecords(c("$OMEGA DIAGONAL(3) 0.1 0.2 0.3"))$n, 3L
  )

  # (value)xN repetition, and a continuation line
  expect_equal(
    PMXFrem:::.fremOmegaRecords(c("$OMEGA (0.1)x3", "$SIGMA 1"))$n, 3L
  )
  expect_equal(
    PMXFrem:::.fremOmegaRecords(c("$OMEGA 0.1 0.2", "  0.3 ; a comment", "$SIGMA 1"))$n,
    3L
  )

  # options and comments must not be counted as values
  expect_equal(
    PMXFrem:::.fremOmegaRecords(c("$OMEGA 1e-04 FIX ; 2. IIV on D1"))$n, 1L
  )
})

test_that("addFremIIV stops when numSkipOm does not land on an $OMEGA record boundary", {
  m <- c(
    "$PROBLEM x", "$PK", "CL = THETA(1) * EXP(ETA(1) + ETA(2) + ETA(3))",
    "$THETA 1", "$OMEGA BLOCK(3) 0.1 0.01 0.2 0.01 0.01 0.3", "$SIGMA 1"
  )
  # numSkipOm = 1 would put the new eta inside the BLOCK(3): there is no record
  # boundary there, so guessing a position would silently corrupt the model.
  expect_error(
    addFremIIV(m,
      omegaInit = 0.01, link = "none",
      numNonFREMThetas = 1, numSkipOm = 1, quiet = TRUE
    ),
    "no \\$OMEGA record starts at ETA\\(2\\)"
  )
})

test_that("the eta/omega counters read code, not comments or prior records", {
  # A commented-out ETA() is not a reference. Counting it inflates numTotEta,
  # and with it numParCov and the generated function's default etas length.
  m <- c("$PK", "; old: CL = EXP(MU_3 + ETA(99))", "CL = EXP(MU_3 + ETA(3))")
  expect_equal(PMXFrem:::.fremCountTotEta(m), 3L)

  # $OMEGAP / $OMEGAPD are prior records; they define no etas of the model.
  pri <- c(
    "$OMEGA BLOCK(1) 0.1", "$OMEGA BLOCK(2) 0.1 0.01 0.1",
    "$OMEGAP BLOCK(2) VALUES(0.1,0.01) FIX", "$OMEGAPD 2 FIX", "$SIGMA 1"
  )
  expect_equal(PMXFrem:::.fremOmegaRecords(pri)$n, c(1L, 2L))

  # NONMEM 7.3's SAME(m) stands for m repeats of the preceding block.
  expect_equal(
    PMXFrem:::.fremOmegaRecords(c(
      "$OMEGA BLOCK(2) 0.1 0.01 0.1", "$OMEGA BLOCK SAME(3)", "$SIGMA 1"
    ))$n,
    c(2L, 6L)
  )
  expect_equal(
    PMXFrem:::.fremOmegaRecords(c(
      "$OMEGA BLOCK(2) 0.1 0.01 0.1", "$OMEGA BLOCK(2) SAME(3)", "$SIGMA 1"
    ))$n,
    c(2L, 6L)
  )
})

test_that("addFremStructuralTheta refuses a conditionally assigned $PK parameter", {
  # run31.mod's own house style is
  #   IF(FOOD.EQ.1) MATFOOD = 1
  #   IF(FOOD.EQ.0) MATFOOD = ( 1 + THETA(6))
  # The line-anchored detector saw no assignment, so the parameter was treated
  # as new and "MATFOOD = THETA(8)" was appended at the end of $PK - after
  # MATCOVTIME = MATFOOD had already consumed the real value. NM-TRAN accepts
  # that and NONMEM estimates a THETA that cannot move the objective function.
  td <- withr::local_tempdir()
  m <- .run31(td)
  expect_error(
    addFremStructuralTheta(m,
      thetaInit = 0.1, parameter = "MATFOOD",
      addEta = FALSE, bWriteMod = FALSE, quiet = TRUE
    ),
    "MATFOOD.*conditionally|conditionally.*MATFOOD"
  )
})

test_that("addFremStructuralTheta still attaches to a plainly assigned parameter", {
  td <- withr::local_tempdir()
  res <- addFremStructuralTheta(.run31(td),
    thetaInit = 0.1, parameter = "MATCOVTIME", addEta = FALSE,
    bWriteMod = FALSE, quiet = TRUE
  )
  expect_match(
    paste(res$model, collapse = "\n"),
    "MATCOVTIME = \\(MATFOOD\\) \\* THETA\\(8\\)"
  )
})

test_that("the eta renumber pass covers every abbreviated-code record, not just $PK/$ERROR", {
  # An ETA() left behind in $DES still has an in-range index, so a record/count
  # check sees nothing wrong - but it now names a different random effect than
  # the one it named before the insertion.
  td <- withr::local_tempdir()
  l <- readLines(.run31(td), warn = FALSE)
  ie <- grep("^\\$ERROR", l)
  l <- append(l, c("$DES", "DADT(1) = -ETA(6)*A(1)"), after = ie - 1L)
  f <- file.path(td, "des.mod")
  writeLines(l, f)
  file.copy(file.path(td, "run31.ext"), file.path(td, "des.ext"))

  res <- suppressWarnings(addFremIIV(f,
    parameter = "CL", omegaInit = 0.05, quiet = TRUE, bWriteMod = FALSE
  ))
  expect_match(
    grep("DADT", res$model, value = TRUE), "ETA\\(7\\)"
  )
})

test_that("the eta renumber pass is case-insensitive, as NM-TRAN is", {
  # NM-TRAN does not care about case in abbreviated code. Renumbering only the
  # upper-case spelling leaves the lower-case one behind, and the reference
  # that *was* renumbered then points at a variable nothing assigns - a model
  # NM-TRAN rejects.
  td <- withr::local_tempdir()
  l <- readLines(.run31(td), warn = FALSE)
  i <- grep("COV6\\s*=\\s*MU_6\\s*\\+\\s*ETA\\(6\\)", l)
  expect_length(i, 1)
  l[i] <- "     cov6 = mu_6 + eta(6)"
  f <- file.path(td, "lc.mod")
  writeLines(l, f)
  file.copy(file.path(td, "run31.ext"), file.path(td, "lc.ext"))

  res <- suppressWarnings(addFremIIV(f,
    parameter = "CL", omegaInit = 0.05, quiet = TRUE, bWriteMod = FALSE
  ))
  txt <- paste(res$model, collapse = "\n")
  # the lower-case line moved to index 7 like every other reference
  expect_match(txt, "(?i)cov7\\s*=\\s*mu_7\\s*\\+\\s*eta\\(7\\)", perl = TRUE)
  expect_no_match(txt, "(?i)cov6\\s*=", perl = TRUE)
})

test_that("addFremIIV warns when the target parameter already carries an ETA", {
  # CL = EXP(MU_3 + ETA(3)) already has IIV. Attaching another gives
  # CL = (EXP(MU_4 + ETA(4))) * EXP(ETA(3)) - two independent random effects
  # stacked on one parameter. That is sometimes intended and sometimes a
  # mistake, and nothing said which.
  td <- withr::local_tempdir()
  expect_warning(
    addFremIIV(.run31(td),
      parameter = "CL", omegaInit = 0.05, quiet = TRUE,
      bWriteMod = FALSE
    ),
    "already references ETA"
  )
})

test_that("addFremStructuralTheta says muReference does not apply to an existing parameter", {
  td <- withr::local_tempdir()
  w <- capture_warnings(
    addFremStructuralTheta(.run31(td),
      thetaInit = 0.1, parameter = "CL", addEta = TRUE, muReference = FALSE,
      omegaInit = 0.05, bWriteMod = FALSE, quiet = TRUE
    )
  )
  expect_true(any(grepl("muReference", w)))
  # CL already carries ETA(3), so the stacking warning comes too
  expect_true(any(grepl("already references ETA", w)))
})

test_that("fremModelInfo warns when the model and the ext describe different structures", {
  # The .ext is deliberately not migrated by the mutators, so a caller can
  # easily pair a mutated .mod with the old .ext. numSkipOm and numTotEta then
  # come back describing the model before the mutation, with no sign of it.
  td <- withr::local_tempdir()
  m <- .run31(td)
  res <- addFremIIV(m,
    parameter = "V", omegaInit = 0.05, quiet = TRUE,
    bWriteMod = FALSE
  ) |> suppressWarnings()
  newMod <- file.path(td, "mutated.mod")
  writeLines(res$model, newMod)
  expect_warning(
    fremModelInfo(modFile = newMod, dfext = file.path(td, "run31.ext")),
    "24 eta|does not match|disagree"
  )
})

test_that("fremModelInfo warns when the model gained a theta the ext does not have", {
  # addFremStructuralTheta(addEta = FALSE) adds a $THETA and no eta. The
  # eta-count comparison alone passes that pairing, and numNonFREMThetas then
  # comes back one short with nothing said.
  td <- withr::local_tempdir()
  m <- .run31(td)
  res <- addFremStructuralTheta(m,
    thetaInit = 0.5, parameter = "KA2", addEta = FALSE,
    bWriteMod = FALSE, quiet = TRUE
  )
  newMod <- file.path(td, "theta_only.mod")
  writeLines(res$model, newMod)
  expect_warning(
    fremModelInfo(modFile = newMod, dfext = file.path(td, "run31.ext")),
    "references THETA\\(26\\) but the ext has only 25"
  )
})

test_that("fremModelInfo does not warn on a matched model and ext", {
  # the guard must not fire on the package's own reference run
  expect_no_warning(fremModelInfo(
    modFile = system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"),
    dfext = system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
  ))
})

test_that("addFremIIV names a conditionally assigned parameter instead of saying it is missing", {
  # MATFOOD is assigned on two guarded lines. Reporting "no $PK assignment of
  # 'MATFOOD' was found" would send the user looking for a typo; the real
  # problem is that an eta cannot be attached to one branch.
  td <- withr::local_tempdir()
  expect_error(
    addFremIIV(.run31(td),
      parameter = "MATFOOD", omegaInit = 0.05, bWriteMod = FALSE, quiet = TRUE
    ),
    "'MATFOOD' is assigned conditionally in \\$PK"
  )
})

test_that("fremModelInfo keeps numParCov consistent when numSkipOm is overridden", {
  # numSkipOm + numParCov + numFREMThetas must add up to the model's etas, so
  # forcing a different numSkipOm moves numParCov with it.
  m <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
  e <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
  info <- suppressWarnings(fremModelInfo(modFile = m, dfext = e, numSkipOm = 1))
  expect_equal(info$numSkipOm, 1)
  expect_equal(
    info$numSkipOm + info$numParCov + info$numFREMThetas, info$numTotEta
  )
})

test_that("fremModelInfo says so when the derived structure is impossible", {
  # run31.mod is a FREM model with 18 FREM covariate thetas; run30.ext is its
  # base model's, with 7 thetas in total. Derived numNonFREMThetas is then
  # 7 - 18 = -11, which no model can have.
  w <- capture_warnings(fremModelInfo(
    modFile = system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"),
    dfext = system.file("extdata/SimNeb/run30.ext", package = "PMXFrem")
  ))
  expect_true(any(grepl("Derived FREM structure looks inconsistent", w)))
})

test_that("the theta renumber pass covers every abbreviated-code record, and ignores case", {
  # .fremRenumberTheta() covered only $PK/$ERROR/$PRED/$THETA and matched only
  # upper-case THETA(). A THETA() in $DES, or written theta(), was left behind:
  # still an in-range index, so nothing complains, but after the insertion it
  # names the theta before the one it used to. Same defect as the eta pass had.
  td <- withr::local_tempdir()
  l <- readLines(.run31(td), warn = FALSE)
  ie <- grep("^\\$ERROR", l)
  l <- append(l, c("$DES", "DADT(1) = -THETA(8)*A(1)"), after = ie - 1L)
  i <- grep("TVCL\\s*=\\s*THETA\\(2\\)", l)
  expect_length(i, 1)
  l[i] <- "TVCL    = theta(2)"
  f <- file.path(td, "thetacase.mod")
  writeLines(l, f)
  file.copy(file.path(td, "run31.ext"), file.path(td, "thetacase.ext"))

  res <- addFremStructuralTheta(f,
    thetaInit = 0.5, parameter = "KA2", addEta = FALSE,
    bWriteMod = FALSE, quiet = TRUE
  )
  # THETA(8) was the first FREM covariate mean; a structural theta is inserted
  # at 8, so every reference from 8 up moves by one - in $DES too.
  expect_match(grep("DADT", res$model, value = TRUE), "THETA\\(9\\)")
  # theta(2) is below the insertion point and must not move, in either case
  expect_match(paste(res$model, collapse = "\n"), "(?i)TVCL\\s*=\\s*theta\\(2\\)", perl = TRUE)
})

test_that("a lower-case theta() at or after the insertion point is renumbered", {
  td <- withr::local_tempdir()
  l <- readLines(.run31(td), warn = FALSE)
  i <- grep("MU_6\\s*=\\s*THETA\\(8\\)", l)
  expect_length(i, 1)
  l[i] <- sub("THETA\\(8\\)", "theta(8)", l[i])
  f <- file.path(td, "lc.mod")
  writeLines(l, f)
  file.copy(file.path(td, "run31.ext"), file.path(td, "lc.ext"))

  res <- addFremStructuralTheta(f,
    thetaInit = 0.5, parameter = "KA2", addEta = FALSE,
    bWriteMod = FALSE, quiet = TRUE
  )
  txt <- paste(res$model, collapse = "\n")
  expect_match(txt, "(?i)MU_6\\s*=\\s*theta\\(9\\)", perl = TRUE)
  expect_no_match(txt, "(?i)MU_6\\s*=\\s*theta\\(8\\)", perl = TRUE)
})
