#' Check a generated FREM parameter function
#'
#' @description Runs four consistency checks on the function from
#'   [createFREMParamFunction()], for a set of test covariate rows:
#'   \enumerate{
#'     \item **Structural match.** With `covthetas = 0` and `etas = 0` a FREM
#'       parameter reduces to its structural typical value. The result is
#'       compared with the SCM-style typical-value function that
#'       [PMXForest::createParamFunction()] generates from the same FREM model.
#'     \item **Covariate splice.** Setting `covthetas[k]` must change every
#'       returned parameter exactly as setting parameter `k`'s `<P>FREMCOV`
#'       column changes the FFEM reference - parameter `k` itself, anything
#'       computed from it, and nothing else.
#'     \item **Random-effect splice.** The same for `etas[numSkipOm + k]`. The
#'       FFEM reference writes each parameter as `EXP(MU + (ETA(k) + <P>FREMCOV))`,
#'       so the covariate column is also the expected effect of the eta.
#'     \item **`numSkipOm`.** The reference model's own `$OMEGA` records say how
#'       many etas precede its parameter block. If that disagrees with the
#'       `numSkipOm` the function was generated with, every `covthetas` index
#'       in it is offset and the result is `FALSE` - see `attr(v, "numSkipOm")`.
#'   }
#'   **Where the probe indices come from.** `k` and `numSkipOm + k` are taken
#'   from `PMXForest`'s own parse of the FFEM reference model (`etaMap`), keyed
#'   by parameter name - not from `x`, and not from a parameter's position in
#'   the request. A check that shares the generator's indexing convention
#'   cannot detect the generator using the wrong one, which is how this
#'   function once reported `PASS` on a subset request whose parameters had
#'   taken another parameter's covariate coefficient and eta.
#'   Checks 2 and 3 assume the parameter is **log-normal** (`P = C * exp(<linear
#'   in ETA>)`). For a parameter whose `$PK` encloses its `ETA()` differently -
#'   additive, logit, `exp(THETA * ETA)`, ... - the splice is not `exp()` scaling,
#'   so `COVSPLICE` / `ETASPLICE` and `PASS` are reported as `NA` for that
#'   parameter (the structural check still runs). Anything that actually fails is
#'   reported loudly. Only the FREM `$PK` parameters are checked - `secondary`
#'   parameters (AUC, Cmax, ...) are skipped, as they have no structural
#'   counterpart or splice to probe.
#'
#' @param x The list returned by [createFREMParamFunction()].
#' @param fun The function to check. Defaults to `eval(parse(text = x$code))`.
#' @param thetas A numeric vector of the FREM model's THETA final estimates
#'   (structural **and** covariate means). Defaults to the values read from
#'   `extFile`.
#' @param extFile Path to the FREM model's `.ext`. Required if `thetas` is not
#'   supplied. Defaults to the `.ext` next to `x$fremModel`.
#' @param dfrows A data frame of covariate rows to test over. Each row is passed
#'   as `dfrow`. Defaults to a single all-reference row (every structural
#'   covariate at `x$missVal`).
#' @param tol Relative tolerance for a check to pass. Default `1e-6`.
#' @param quiet If `FALSE` (default), prints a per-parameter pass/fail summary.
#'
#' @return A single logical - `TRUE` unless a parameter's check actually failed,
#'   so the result can be used directly in an `if`. A non-log-normal parameter
#'   (splice not checked, `PASS = NA`) does not make the result `FALSE`. The
#'   per-parameter detail is attached as `attr(x, "checks")`: a data frame with
#'   `PARAMETER`, `STRUCTURAL` / `COVSPLICE` / `ETASPLICE` (max relative
#'   difference for each check, `NA` when skipped) and `PASS` (`TRUE` / `FALSE` /
#'   `NA`). Printing shows the table.
#'
#' @seealso [createFREMParamFunction()].
#'
#' @export
#'
#' @examples
#' fremModel <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
#' out <- createFREMParamFunction(fremModel,
#'   parameters = c("CL", "V", "MAT"),
#'   quiet = TRUE
#' )
#'
#' ## The structural check needs an independent transliteration of the same
#' ## $PK, and a FREM model cannot supply one - PMXForest refuses it. The FFEM
#' ## version of the model, as written by createFFEMmodel(), is the reference.
#' ffemModel <- system.file("extdata/SimNeb/run31max1-2.mod", package = "PMXFrem")
#'
#' if (verifyFREMParamFunction(out, ffemModel = ffemModel, quiet = TRUE)) {
#'   message("generated function checks out")
#' }
#'
#' v <- verifyFREMParamFunction(out, ffemModel = ffemModel, quiet = TRUE)
#' attr(v, "checks") # per-parameter detail
#'
#' @family Diagnostics & Plotting
#' @concept diagnostics
#' @param ffemModel Path to the FFEM version of the model - what
#'   [createFFEMmodel()] writes. Required when `x` was built from a FREM model:
#'   the structural check compares the generated function against an
#'   *independent* transliteration of the same `$PK`, and
#'   `PMXForest::createParamFunction()` refuses a FREM model outright. The FFEM
#'   model's `$PK` is the FREM model's with the covariate effect as an additive
#'   term inside the same `EXP()` as the ETA, so with those columns at 0 the two
#'   reduce to the same typical values. Defaults to `x$fremModel`, which is
#'   correct only when that model is not itself a FREM model.
#' @param covSuffix The suffix [createFFEMmodel()] gives the FREM covariate
#'   columns, so their reference value - 0, by construction - can be supplied
#'   without the caller naming them. Default `"FREMCOV"`.
verifyFREMParamFunction <- function(x,
                                    fun = NULL,
                                    thetas = NULL,
                                    extFile = NULL,
                                    ffemModel = NULL,
                                    covSuffix = "FREMCOV",
                                    dfrows = NULL,
                                    tol = 1e-6,
                                    quiet = FALSE) {
  if (!is.list(x) || is.null(x$code) || is.null(x$fremModel)) {
    stop("`x` must be the list returned by createFREMParamFunction().",
      call. = FALSE
    )
  }
  ## Check the $PK parameters only; `secondary` quantities (AUC, Cmax, ...) are
  ## not comparable to PMXForest::createParamFunction() and have no eta/cov
  ## splice to probe.
  params <- if (!is.null(x$primaryNames)) {
    x$primaryNames
  } else {
    setdiff(
      x$functionListName,
      if (is.null(x$secondaryNames)) character(0) else x$secondaryNames
    )
  }
  numSkipOm <- if (is.null(x$numSkipOm)) 0 else x$numSkipOm
  nNonFREM <- x$noBaseThetas # length of `basethetas`
  if (is.null(fun)) fun <- eval(parse(text = x$code))

  ## ---- FREM model THETA final estimates (all of them) ----
  if (is.null(thetas)) {
    if (is.null(extFile)) {
      extFile <- paste0(tools::file_path_sans_ext(x$fremModel), ".ext")
    }
    if (!file.exists(extFile)) {
      stop("Supply `thetas`, or an `extFile` that exists (looked for ",
        extFile, ").",
        call. = FALSE
      )
    }
    dfe <- getExt(extFile = extFile)
    dfe <- dfe[dfe$ITERATION == -1000000000, , drop = FALSE]
    thetas <- as.numeric(dfe[1, grep("^THETA", names(dfe)), drop = TRUE])
  }
  basethetas <- thetas[seq_len(nNonFREM)] # what the FREM fn gets

  ## ---- the independent structural reference ----
  ## The point of the structural check is that the right-hand side is a
  ## *different* transliteration of the same $PK - PMXForest's emitter, not
  ## .fremEmit(). Comparing .fremEmit() against itself would prove nothing.
  ##
  ## A FREM model cannot supply it: PMXForest::createParamFunction() refuses
  ## one outright, and rightly - a FREM model's covariate effects live in
  ## $OMEGA, so a parameter function built from its $PK would describe none of
  ## the covariates the model was built for. The FFEM version of the same model
  ## is the parseable one. Its $PK is the FREM model's with the covariate
  ## effect as an additive term inside the same EXP() as the ETA,
  ##
  ##   FREM  CL = EXP(MU_3 + ETA(3))
  ##   FFEM  CL = EXP(MU_3 + (ETA(3) + CLFREMCOV))
  ##
  ## so with the FREM covariate columns at 0 the two reduce to the same typical
  ## values, and its $THETA records are the structural ones alone, which is
  ## exactly the `basethetas` slice above.
  refModel <- if (is.null(ffemModel)) x$fremModel else ffemModel
  if (!file.exists(refModel)) {
    stop("The structural reference model does not exist: ", refModel,
      call. = FALSE
    )
  }
  if (.isFremModel(refModel)) {
    stop(basename(refModel), " is a FREM model, which cannot be the ",
      "structural reference: PMXForest::createParamFunction() refuses one.",
      "\nSupply the FFEM version through `ffemModel` - the model ",
      "createFFEMmodel() writes - e.g. ffemModel = \"run31max1-2.mod\".",
      call. = FALSE
    )
  }

  ## The FREM covariate columns are named <parameter><covSuffix> by
  ## createFFEMmodel(), and their reference is 0 by construction: the typical
  ## subject carries no covariate effect. The caller should not have to know
  ## that, or type the names.
  ## Only pin the columns this model actually has: the reference defaults to
  ## `x$fremModel`, which for a non-FREM model carries no FREM covariate
  ## columns at all, and PMXForest rejects a covRef naming a covariate the $PK
  ## does not use.
  refLines <- sub(";.*$", "", readLines(refModel, warn = FALSE))
  refText <- paste(refLines, collapse = " ")
  ## Every <name><covSuffix> the reference actually mentions, not only the
  ## ones for the requested parameters: a requested parameter can depend on
  ## another one (KA on MAT), and $PK then reads that one's FREMCOV column
  ## too. Pinning only the requested names left it unresolved and the whole
  ## check errored out.
  fremCols <- unique(unlist(regmatches(refText, gregexpr(
    paste0("\\b[A-Za-z][A-Za-z0-9_]*", covSuffix, "\\b"), refText
  ))))
  ## Reference values the caller gave the generator apply to the reference
  ## model too - it is the same $PK, less the FREM machinery.
  userRef <- list()
  if (length(x$covRef)) {
    vals <- lapply(x$covRef, function(z) if (is.list(z)) z$value else z)
    keep <- vapply(names(vals), function(n) {
      grepl(paste0("\\b", n, "\\b"), refText)
    }, logical(1))
    userRef <- vals[keep]
  }
  covRefRef <- c(
    stats::setNames(as.list(rep(0, length(fremCols))), fremCols),
    userRef[setdiff(names(userRef), fremCols)]
  )
  scm <- PMXForest::createParamFunction(refModel,
    parameters = params,
    covRef = if (length(covRefRef)) covRefRef else NULL,
    quiet = TRUE
  )
  scmFn <- eval(parse(text = scm$code))
  scmTh <- thetas[seq_len(scm$noBaseThetas)]

  ## ---- test rows ----
  if (is.null(dfrows)) {
    covs <- names(x$covRef)
    dfrows <- if (length(covs)) {
      as.data.frame(stats::setNames(as.list(rep(x$missVal, length(covs))), covs))
    } else {
      data.frame(row.names = 1L)
    }
  }
  np <- length(params)

  ## ---- probe indices, derived independently of the generated function ----
  ## PMXForest's own parse of the FFEM reference reports which ETA() each
  ## parameter carries in $PK (scm$etaMap), keyed by name. Taking the indices
  ## from `x`, or from position in `params`, would mean probing with whatever
  ## convention .fremEmit() used - and a check that shares the emitter's
  ## convention cannot detect the emitter using the wrong one. That is exactly
  ## how this function once reported PASS on a generated function that took
  ## another parameter's covariate coefficient and eta.
  etaIdx <- stats::setNames(rep(NA_integer_, np), params)
  known <- intersect(params, names(scm$etaMap))
  etaIdx[known] <- as.integer(scm$etaMap[known])
  covIdx <- etaIdx - numSkipOm
  spliceable <- !is.na(covIdx) & covIdx >= 1L

  ## The probe vector has to be long enough for every ETA() the function
  ## references, not only the ones being probed - a parameter whose eta the
  ## reference could not place is still spliced in the generated code. Its own
  ## declared default says how long that is.
  funEtas <- tryCatch(length(eval(formals(fun)$etas)),
    error = function(e) 0L
  )
  nEtas <- max(c(numSkipOm + 1L, etaIdx, funEtas), na.rm = TRUE)

  ## numSkipOm, derived from the reference rather than believed from `x`.
  ## covIdx = etaIdx - numSkipOm, so a wrong numSkipOm makes the generated
  ## function index covthetas wrongly for every real caller - and a check
  ## that inherits the same wrong value cannot see it. The FFEM reference's
  ## own $OMEGA records say how many etas precede its parameter block.
  ## Only when the reference's last $OMEGA record is a BLOCK - not a SAME
  ## repeat - large enough to hold every placed parameter eta: that is the
  ## layout createFFEMmodel() writes. A reference with one diagonal record per
  ## eta, or an IOV block after the parameter block, has no single record that
  ## marks where the parameters start, and reading "everything before the last
  ## record" as numSkipOm would fail a correct function.
  refOm <- .fremOmegaRecords(refLines)
  lastRec <- if (nrow(refOm)) {
    toupper(paste(refLines[refOm$start[nrow(refOm)]:refOm$end[nrow(refOm)]], collapse = " "))
  } else {
    ""
  }
  placed <- sum(!is.na(etaIdx))
  refSkip <- if (nrow(refOm) > 1L &&
    grepl("\\bBLOCK\\s*\\(", lastRec) && !grepl("\\bSAME\\b", lastRec) &&
    refOm$n[nrow(refOm)] >= placed) {
    as.integer(sum(refOm$n) - refOm$n[nrow(refOm)])
  } else {
    NA_integer_
  }
  skipOk <- is.na(refSkip) || refSkip == numSkipOm
  nCov <- max(c(1L, covIdx, x$numParCov), na.rm = TRUE)

  ## Which parameters are log-normal (P = C * exp(<linear in ETA>))? Only those
  ## can be checked with the exp() splice; the rest get NA. `fremEtaScale` is
  ## absent on objects made before this was recorded - assume "exp" then.
  scale <- if (is.null(x$fremEtaScale)) {
    stats::setNames(rep("exp", np), params)
  } else {
    s <- x$fremEtaScale[params]
    s[is.na(s)] <- "exp" # not a FREM covariate parameter
    stats::setNames(s, params)
  }
  ## A parameter whose eta the reference could not place is not spliceable
  ## either: there is no index to probe at.
  isExp <- scale == "exp" & spliceable

  structD <- covD <- etaD <- rep(0, np)
  covD[!isExp] <- etaD[!isExp] <- NA_real_
  names(structD) <- names(covD) <- names(etaD) <- params

  ## Relative difference, with anything that cannot be computed - NA from a
  ## missing value, NaN from sqrt(-1) - reported as Inf. NA in the output is
  ## reserved for "this check was not applicable"; a comparison that produced
  ## no number is a failed check, not a skipped one.
  rel <- function(a, b) {
    d <- abs((a - b) / ifelse(b == 0, 1, b))
    if (length(d) != 1L || !is.finite(d)) Inf else d
  }
  getp <- function(res, p) if (is.null(res[[p]])) NA_real_ else res[[p]]

  ## Expected values come from the FFEM reference, not from arithmetic on the
  ## generated function's own output. There each FREM parameter is written
  ##   P = EXP(MU + (ETA(k) + <P>FREMCOV))
  ## so setting <P>FREMCOV to x in the reference is exactly what covthetas = x,
  ## or etas[k] = x, must do to the generated function - for P itself, and for
  ## every parameter computed from it (KA = 1 / (MAT - D1)). That lets every
  ## returned parameter be checked in every probe, by name, independently of
  ## the emitter.
  covCol <- stats::setNames(paste0(params, covSuffix), params)
  hasCol <- covCol %in% fremCols & isExp
  refWith <- function(dfrow, values) {
    d <- dfrow
    for (p in names(values)) d[[covCol[[p]]]] <- values[[p]]
    scmFn(thetas = scmTh, df = d)
  }
  ## A parameter whose own splice is not probed keeps NA in that column unless
  ## it moves when it should not - then the size of the movement is reported,
  ## which fails it.
  record <- function(D, p, d) {
    if (isExp[[p]]) max(D[[p]], d) else if (d > tol) max(D[[p]], d, na.rm = TRUE) else D[[p]]
  }

  for (i in seq_len(nrow(dfrows))) {
    dfrow <- dfrows[i, , drop = FALSE]

    base0 <- fun(basethetas,
      covthetas = rep(0, nCov), dfrow = dfrow,
      etas = rep(0, nEtas)
    )
    scm0 <- scmFn(thetas = scmTh, df = dfrow)
    for (p in params) {
      structD[p] <- max(structD[p], rel(getp(base0, p), getp(scm0, p)))
    }

    ## A distinct coefficient per parameter, placed at its own FREM index, so a
    ## function reading a neighbour's coefficient scales by the wrong one.
    probe <- params[hasCol]
    ct <- rep(0, nCov)
    vals <- stats::setNames(seq_along(probe) / 7, probe)
    ct[covIdx[probe]] <- vals
    covV <- fun(basethetas, covthetas = ct, dfrow = dfrow, etas = rep(0, nEtas))
    refC <- refWith(dfrow, vals)
    for (p in params) covD[p] <- record(covD, p, rel(getp(covV, p), getp(refC, p)))

    for (k in probe) {
      e <- rep(0, nEtas)
      e[etaIdx[[k]]] <- 0.3
      etaV <- fun(basethetas, covthetas = rep(0, nCov), dfrow = dfrow, etas = e)
      refE <- refWith(dfrow, stats::setNames(0.3, k))
      for (p in params) etaD[p] <- record(etaD, p, rel(getp(etaV, p), getp(refE, p)))
    }
  }

  out <- data.frame(
    PARAMETER = params,
    STRUCTURAL = structD,
    COVSPLICE = covD,
    ETASPLICE = etaD,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  # NA COVSPLICE / ETASPLICE (non-log-normal parameter) -> PASS is NA when the
  # structural check passes, FALSE when it does not.
  ## FALSE on any failed or uncomputable comparison; NA only when the structural
  ## check passed and a splice check did not apply.
  out$PASS <- with(out, ifelse(
    STRUCTURAL > tol | (!is.na(COVSPLICE) & COVSPLICE > tol) |
      (!is.na(ETASPLICE) & ETASPLICE > tol),
    FALSE,
    ifelse(is.na(COVSPLICE) | is.na(ETASPLICE), NA, TRUE)
  ))

  nFail <- sum(out$PASS %in% FALSE)
  nNA <- sum(is.na(out$PASS))
  if (!skipOk && !quiet) {
    message(
      "verifyFREMParamFunction(): numSkipOm is ", numSkipOm,
      ", but ", basename(refModel), " has ", refSkip,
      " eta(s) before its parameter $OMEGA block. Every covthetas index in ",
      "the generated function is offset by ", numSkipOm - refSkip, "."
    )
  }

  if (!quiet) {
    message(
      "verifyFREMParamFunction(): ", sum(out$PASS %in% TRUE), "/",
      nrow(out), " parameter(s) pass (tol ", tol, ")",
      if (nNA) paste0(", ", nNA, " not checked (non-log-normal)") else "",
      "."
    )
    for (i in seq_len(nrow(out))) {
      status <- if (isTRUE(out$PASS[i])) {
        "pass"
      } else if (is.na(out$PASS[i])) {
        "not checked (non-log-normal; structural OK)"
      } else {
        "FAIL"
      }
      message(
        "  ", out$PARAMETER[i], ": ", status,
        "  (structural ", signif(out$STRUCTURAL[i], 3),
        ", cov ", signif(out$COVSPLICE[i], 3),
        ", eta ", signif(out$ETASPLICE[i], 3), ")"
      )
    }
  }

  ## A single logical for use in `if`: FALSE only if a check actually failed;
  ## a non-log-normal parameter (PASS = NA) does not make it FALSE.
  invisible(structure(nFail == 0L && skipOk,
    class = "pmxFREMVerify", checks = out,
    numSkipOm = list(object = numSkipOm, reference = refSkip, ok = skipOk)
  ))
}

#' @export
print.pmxFREMVerify <- function(x, ...) {
  d <- attr(x, "checks")
  nNA <- sum(is.na(d$PASS))
  sk <- attr(x, "numSkipOm")
  cat(if (isTRUE(unclass(x)[1])) "PASS" else "FAIL",
    " - verifyFREMParamFunction: ", sum(d$PASS %in% TRUE), "/", nrow(d),
    " parameter(s)",
    if (nNA) paste0(" (", nNA, " not checked - non-log-normal)") else "",
    "\n",
    sep = ""
  )
  if (!is.null(sk) && isFALSE(sk$ok)) {
    cat("  numSkipOm ", sk$object, " disagrees with the reference model's ",
      sk$reference, "\n",
      sep = ""
    )
  }
  print(d, row.names = FALSE)
  invisible(x)
}

## Does this control stream declare FREMTYPE in $INPUT? That is what makes it a
## FREM model rather than its FFEM counterpart, and what
## PMXForest::createParamFunction() refuses on.
##
## @noRd
.isFremModel <- function(modFile) {
  L <- sub("\r$", "", readLines(modFile, warn = FALSE))
  i <- grep("^\\s*\\$INP", L)
  if (!length(i)) {
    return(FALSE)
  }
  j <- grep("^\\s*\\$", L)
  j <- j[j > i[1]]
  block <- L[i[1]:(if (length(j)) j[1] - 1L else length(L))]
  any(grepl("\\bFREMTYPE\\b", toupper(sub(";.*$", "", block))))
}
