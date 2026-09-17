#' Generate a FREM parameter function from the FREM model's `$PK`
#'
#' @description Translates the `$PK` block of the **FREM** model into R source for
#'   a parameter function of the shape `getForestDFFREM()` and `getExplainedVar()`
#'   expect, `function(basethetas, covthetas, dfrow, etas, ...)`.
#'
#'   The structural `$PK` is transliterated (structural, non-FREM covariates such
#'   as allometric weight kept). A parameter in `parameters` that carries IIV in
#'   `$PK` (one eta in the FREM block's range, across all of its assignments) is a **FREM covariate
#'   parameter**: its `ETA()` reference is replaced **in place** - whatever
#'   encloses it, `exp(mu + ETA)`, `TV * exp(ETA)`, `TV + ETA`, ... - by
#'   `covthetas[k] + etas[i]`, where `ETA(i)` is the reference `$PK` makes and
#'   `k = i - numSkipOm`, because the FREM covariate coefficient is additive on
#'   the same (eta) scale. A parameter with no `ETA()` is returned
#'   as `$PK` computes it (no covariate effect - not an error). A parameter
#'   whose single `ETA()` falls inside the skipped omegas keeps that eta as
#'   `.eta(etas, n)` but takes no covariate effect, since no `covthetas` index
#'   applies to it. Every other `ETA(n)` is set to 0.
#'
#'   The emitted body is **pruned to the statements the requested `parameters`
#'   actually depend on**, so the FREM covariate block (`MU_j = THETA(...)` /
#'   `COVj = MU_j + ETA(j)`) is dropped and `basethetas` is only the structural
#'   thetas - the first `numNonFREMThetas`, exactly what `getForestDFFREM()` /
#'   `getExplainedVar()` pass in.
#'
#'   The result is **source text for you to read, check and edit** - nothing is
#'   evaluated. One generated function serves both uses: call it with `etas = 0`
#'   for a `getForestDFFREM()` forest plot, and with non-zero `etas` for a
#'   `getExplainedVar()` explained-variability plot.
#'
#' @details
#'   **Why the FREM model, not the base model.** `numSkipOm` and the structural
#'   `$PK` / theta numbering are read from the model you are actually analysing.
#'   If the FREM model was edited after it was built (extra `$THETA`s or `$OMEGA`s
#'   added), the base model no longer describes it; the FREM model always does.
#'
#'   **What `covthetas` and `etas` are, and how they are indexed.**
#'   `covthetas[k]` is the FFEM-projected covariate coefficient for the **model's**
#'   k-th FREM covariate parameter, as computed by [calcFFEM()] and passed in by
#'   `getForestDFFREM()` / `getExplainedVar()`. `etas` carries the model's random
#'   effects, with the `numSkipOm` leading non-FREM omegas in front, so the k-th
#'   FREM parameter's structural eta is `etas[numSkipOm + k]`.
#'
#'   Both are indexed by the **model's** numbering, never by position in
#'   `parameters`: `k` is read from the `ETA(i)` the parameter's `$PK`
#'   assignment makes, as `i - numSkipOm`. Asking for two parameters instead of
#'   three therefore emits exactly the code the three-parameter request emitted
#'   for those two, and the same `covthetas` / `etas` vectors serve either call.
#'   (Before PMXFrem 2.1.1 the index was the parameter's position in
#'   `parameters`, so a subset or reordered request silently took another
#'   parameter's covariate coefficient and eta.)
#'
#'   The generated function reads `etas` through a small `.eta()` helper that
#'   **stops** when the vector is too short for the `ETA()` the model
#'   references - that used to be a silent zero, which reads as "this subject
#'   has no random effect" rather than as a missing argument.
#'
#'   **Structural covariate references.** A covariate named in `$INPUT` and used
#'   but not assigned in `$PK` needs a value for the rows where it is inactive
#'   (`missVal`). The reference is taken from the control stream by the same
#'   rules as [PMXForest::createParamFunction()] and hoisted into a preamble;
#'   override or supply through `covRef`.
#'
#'   **Which parameters get the covariate splice.** A parameter's etas are
#'   collected from every assignment of it, at any depth - `CL = TVCL` followed
#'   by `CL = CL * EXP(ETA(3))`, or `CL` assigned only inside `IF` blocks, are
#'   both found. One eta in the FREM block's range -> the parameter is spliced,
#'   in every statement that carries that eta; its other statements take their
#'   `ETA()` as 0. None -> returned as-is (no covariate effect). Two different
#'   FREM-range etas -> returned as-is with a warning (which one carries the
#'   covariate effect is ambiguous). An eta beyond the FREM parameters - a FREM
#'   covariate's own, such as `COV6` - is not a parameter's, and is returned
#'   with that `ETA()` as 0 and a warning. A parameter whose single `ETA()` falls inside the skipped
#'   omegas (an IOV or residual-error eta, say) is not a FREM covariate
#'   parameter: it keeps its own eta and gets no covariate effect. A statement
#'   carrying a parameter's FREM eta together with another eta is an error,
#'   because the coefficient cannot be spliced in place without counting it
#'   twice.
#'
#' @param fremModel Path to the FREM NONMEM control stream (`.mod` / `.ctl`).
#'   Optional if `runno` / `modName` (+ `modDevDir`) are given.
#' @inheritParams getFileNames
#' @param parameters Character vector of `$PK` variables the generated function
#'   should return. List the FREM covariate parameters **in the order of the FREM
#'   parameter block** (that order is what `covthetas` / `etas` index); other
#'   `$PK` quantities may be included and are returned as-is.
#' @param numSkipOm,numNonFREMThetas The number of skipped omegas and of
#'   structural thetas. `NULL` (default) derives them from the FREM model and the
#'   ext via [fremModelInfo()] when either is `NULL`; a supplied value that
#'   disagrees with the derived one then warns and is kept. When both are
#'   supplied nothing is derived, so nothing is checked against the `.ext` -
#'   though `numSkipOm` is still compared with the model's own `$OMEGA`
#'   records.
#' @param dfext,extFile A `getExt()` data frame / an `.ext` path, used only to
#'   derive `numSkipOm` / `numNonFREMThetas`. Defaults to the ext located from
#'   `runno` / `modName` / `modDevDir`, else the `.ext` beside `fremModel`.
#' @param numParCov Optional cross-check: the number of FREM covariate
#'   parameters in the MODEL - its FREM `$OMEGA` block less the FREM covariates -
#'   whatever subset `parameters` asks for. A mismatch warns and the derived
#'   count is used.
#' @param covRef Optional named list of structural-covariate reference values,
#'   e.g. `list(WT = 75)`, forwarded to [PMXForest::nmParsePK()].
#' @param functionName Name for the generated function. Default `"paramFunction"`.
#' @param file Optional path to write the generated source to. It is returned
#'   either way.
#' @param missVal The value marking an inactive covariate. Default -99.
#' @param quiet If `FALSE` (default), reports what was found.
#' @param secondary Optional named list of secondary parameters to append to the
#'   generated function's return list. Each entry's value is a single string (R
#'   code whose last value is the result, `list(AUC = "dfrow$DOSE / CL")`, or
#'   the path to an `.R` file of arbitrary code such as an `mrgsolve`
#'   simulation), or a list `list(source = <string>, dose = 100, tau = 12, ...)`
#'   carrying that `source` plus named atomic constants bound ahead of it. The
#'   code is spliced in inside a `local()` block and sees `basethetas`,
#'   `covthetas`, `dfrow` (also as `df`), `etas`, `...`, any constants passed
#'   alongside `source`, and every structural parameter by name; covariate
#'   columns are `dfrow$NAME`. Handled by [PMXForest::nmResolveSecondary()]; see
#'   [PMXForest::createParamFunction()].
#'
#' @return A list:
#'   \itemize{
#'     \item `code` - the generated R source, class `"pmxFREMParamFunction"`.
#'     \item `functionListName` - `parameters`, with any `secondary` names
#'       appended (so `getForestDFFREM()` / `getExplainedVar()` pick them up).
#'     \item `primaryNames` - `parameters` alone.
#'     \item `secondaryNames` - the `secondary` names (`character(0)` when none).
#'       [verifyFREMParamFunction()] skips these.
#'     \item `fremParameters` - the subset of `parameters` that got the covariate
#'       splice, in order.
#'     \item `fremEtaScale` - named character over `fremParameters`: `"exp"` when
#'       the `$PK` line is `C * exp(<linear in ETA>)` (log-normal), `"other"`
#'       otherwise. [verifyFREMParamFunction()] uses this to decide whether the
#'       `exp()` splice checks apply.
#'     \item `noBaseThetas` - `numNonFREMThetas` (the length of `basethetas`).
#'     \item `covRef` - the structural-covariate reference used, with its source.
#'     \item `numParCov` - the number of FREM covariate parameters in the model,
#'       which is the length `covthetas` must have; not the length of
#'       `fremParameters` when a subset was requested.
#'     \item `numSkipOm`, `numNonFREMThetas`, `fremModel`, `missVal` - as
#'       supplied or derived.
#'   }
#'
#' @seealso [verifyFREMParamFunction()], [PMXForest::nmParsePK()],
#'   [fremModelInfo()], [getForestDFFREM()], [getExplainedVar()].
#'
#' @export
#'
#' @examples
#' fremModel <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
#'
#' # numSkipOm / numNonFREMThetas are derived from the FREM model and its ext.
#' # The ext is found next to the model automatically, so `extFile` is not
#' # needed here - pass `extFile` (or `dfext`) only when the .ext is elsewhere
#' # and cannot be reached through runno / modName / modDevDir.
#' out <- createFREMParamFunction(fremModel, parameters = c("CL", "V", "MAT"))
#' cat(out$code, sep = "\n")
#'
#' # Same result, located by run number instead of a path:
#' out2 <- createFREMParamFunction(
#'   runno      = 31,
#'   modDevDir  = system.file("extdata/SimNeb/", package = "PMXFrem"),
#'   parameters = c("CL", "V", "MAT")
#' )
#' stopifnot(identical(out$code, out2$code))
#'
#' @family Diagnostics & Plotting
#' @concept diagnostics
createFREMParamFunction <- function(fremModel = NULL,
                                    parameters,
                                    numSkipOm = NULL,
                                    numNonFREMThetas = NULL,
                                    runno = NULL,
                                    modName = NULL,
                                    modDevDir = NULL,
                                    dfext = NULL,
                                    extFile = NULL,
                                    numParCov = NULL,
                                    covRef = NULL,
                                    functionName = "paramFunction",
                                    file = NULL,
                                    missVal = -99,
                                    quiet = FALSE,
                                    secondary = NULL) {
  if (!requireNamespace("PMXForest", quietly = TRUE) ||
    !exists("nmParsePK", where = asNamespace("PMXForest"), inherits = FALSE) ||
    !exists("nmResolveSecondary",
      where = asNamespace("PMXForest"),
      inherits = FALSE
    )) {
    stop("createFREMParamFunction() needs PMXForest (>= 1.3.0), which ",
      "exports nmParsePK() and nmResolveSecondary(); please update PMXForest.",
      call. = FALSE
    )
  }
  if (missing(parameters) || length(parameters) < 1) {
    stop("`parameters` must name at least one $PK variable.", call. = FALSE)
  }

  ## ---- locate the FREM model / ext ----
  if (is.null(fremModel)) {
    if (is.null(runno) && is.null(modName)) {
      stop("Supply `fremModel`, or `runno` / `modName` (+ `modDevDir`).",
        call. = FALSE
      )
    }
    fn <- getFileNames(
      runno = runno, modName = modName,
      modDevDir = if (is.null(modDevDir)) "." else modDevDir
    )
    fremModel <- fn$mod
    if (is.null(extFile)) extFile <- fn$ext
  }

  ## ---- numSkipOm / numNonFREMThetas: derive when not supplied ----
  if (is.null(numSkipOm) || is.null(numNonFREMThetas)) {
    if (is.null(dfext)) {
      if (is.null(extFile)) {
        extFile <- paste0(tools::file_path_sans_ext(fremModel), ".ext")
      }
      if (!file.exists(extFile)) {
        stop("Need `numSkipOm` and `numNonFREMThetas`, or an ext ",
          "(`dfext` / `extFile` / `runno`) to derive them.",
          call. = FALSE
        )
      }
      dfext <- getExt(extFile = extFile)
    }
    .info <- fremModelInfo(
      modFile = fremModel, dfext = dfext,
      numNonFREMThetas = numNonFREMThetas,
      numSkipOm = numSkipOm
    )
    numSkipOm <- .info$numSkipOm
    numNonFREMThetas <- .info$numNonFREMThetas
  }

  if (anyDuplicated(parameters)) {
    warning("`parameters` has a repeated name (",
      paste(unique(parameters[duplicated(parameters)]), collapse = ", "),
      "); using each once.",
      call. = FALSE
    )
    parameters <- unique(parameters)
  }

  ## ---- parse the FREM model's $PK ----
  ## Secondaries first: their code can read $PK quantities the caller did not
  ## ask for - `100 / (CL * FREL)` with only CL requested. Parsing and pruning
  ## have to keep those, or the generated function fails at call time with
  ## "object 'FREL' not found". Same as PMXForest::createParamFunction().
  sec <- PMXForest::nmResolveSecondary(secondary, quiet = quiet)
  secNames <- unname(vapply(sec, `[[`, "", "name"))
  secReads <- unique(unlist(lapply(sec, function(e) {
    tryCatch(all.vars(parse(text = e$lines)), error = function(err) character(0))
  })))

  p <- PMXForest::nmParsePK(fremModel,
    parameters = parameters, covRef = covRef,
    missVal = missVal, keep = secReads
  )

  ## ---- the model's FREM block: where FREM parameters' etas can be ---------
  numTotEta <- .fremCountTotEta(readLines(fremModel, warn = FALSE))
  ## numParCov = the FREM block's size less the FREM covariates, which is what
  ## fremModelInfo() derives. A non-FREM model (getCovNames() stops on one) has
  ## no such block, so the upper bound is open.
  nFremCov <- tryCatch(length(getCovNames(modFile = fremModel)$covNames),
    error = function(e) NA_integer_
  )
  modelParCov <- numTotEta - numSkipOm - nFremCov
  fremEtaMax <- if (is.na(modelParCov)) Inf else numSkipOm + modelParCov

  ## ---- classify the requested parameters by their $PK ETA() references ----
  ## A parameter's eta is found in EVERY assignment of it, at any depth - not
  ## only its first top-level one. `CL = TVCL` followed by
  ## `CL = CL * EXP(ETA(3))`, or CL assigned only inside IF blocks, are
  ## ordinary $PK; looking at the first top-level assignment alone classified
  ## both as having no eta and silently dropped CL's eta and covariate effect.
  ##
  ##   one distinct eta in the FREM range  -> FREM covariate parameter
  ##   one distinct eta in the skip region -> keeps its eta, no covariate effect
  ##   an eta beyond the FREM parameters   -> not a FREM parameter's (a FREM
  ##                                          covariate's own eta, say)
  ##   several distinct FREM-range etas    -> ambiguous, returned as-is
  paramEta <- lapply(parameters, function(nm) {
    as <- .fremAssignsOf(p$statements, nm)
    sort(unique(unlist(lapply(as, function(a) .fremEtaIndices(a$rhs)))))
  })
  names(paramEta) <- parameters
  inFrem <- lapply(paramEta, function(e) e[e > numSkipOm & e <= fremEtaMax])
  inSkip <- lapply(paramEta, function(e) e[e <= numSkipOm])
  beyond <- lapply(paramEta, function(e) e[e > fremEtaMax])

  isFremParam <- lengths(inFrem) == 1L
  fremParams <- parameters[isFremParam]
  keepParams <- parameters[!isFremParam & lengths(inFrem) == 0L & lengths(inSkip) == 1L]
  keepEta <- vapply(inSkip[keepParams], function(e) as.integer(e[1]), integer(1))
  etaCounts <- lengths(paramEta)

  for (nm in parameters[lengths(inFrem) > 1L]) {
    warning("Parameter '", nm, "' is assigned with more than one FREM-range ",
      "eta (ETA(", paste(inFrem[[nm]], collapse = "), ETA("), ")), so which ",
      "one carries its covariate effect is ambiguous. It is returned as ",
      "written with every ETA() set to 0; write its function by hand.",
      call. = FALSE
    )
  }
  for (nm in parameters[lengths(inFrem) == 0L & lengths(beyond) > 0L]) {
    warning("'", nm, "' carries ETA(", paste(beyond[[nm]], collapse = "), ETA("),
      "), beyond the model's ", modelParCov, " FREM parameter eta(s) - a FREM ",
      "covariate's own eta, not a parameter's. It is returned with that ETA() ",
      "set to 0 and no covariate effect.",
      call. = FALSE
    )
  }

  ## For each FREM parameter, how its FREM ETA() is enclosed in $PK, taken from
  ## the assignment that carries it:
  ## "exp"   -> P = C * exp(<linear-in-ETA>)      (log-normal; splice is exp())
  ## "other" -> additive, logit, exp(theta*ETA), ... (verify skips the splice)
  fremEtaIdx <- vapply(inFrem[fremParams], function(e) as.integer(e[1]), integer(1))
  names(fremEtaIdx) <- fremParams
  fremEtaScale <- vapply(fremParams, function(nm) {
    as <- .fremAssignsOf(p$statements, nm)
    carrier <- Find(function(a) fremEtaIdx[[nm]] %in% .fremEtaIndices(a$rhs), as)
    .fremEtaScale(carrier$rhs)
  }, character(1))
  names(fremEtaScale) <- fremParams

  if (is.na(modelParCov)) modelParCov <- max(0L, fremEtaIdx - numSkipOm)

  ## Cross-check numSkipOm against the control stream. The FREM block is the
  ## last $OMEGA record, so everything before it is skipped; that is
  ## fremModelInfo()'s numTotEta - blockN, arrived at without the .ext. The
  ## emitter itself no longer needs numSkipOm to be right - it reads each
  ## parameter's eta index from $PK - but numParCov and the caller's own
  ## covthetas do, so a disagreement is worth saying out loud.
  omRecs <- .fremOmegaRecords(readLines(fremModel, warn = FALSE))
  if (!is.na(nFremCov) && nrow(omRecs) > 1L) {
    modelSkip <- sum(omRecs$n) - omRecs$n[nrow(omRecs)]
    if (modelSkip != numSkipOm) {
      warning("numSkipOm is ", numSkipOm, ", but ", basename(fremModel),
        " has ", modelSkip, " eta(s) before its FREM $OMEGA block. ",
        "covthetas is indexed from the FREM block, so check numSkipOm.",
        call. = FALSE
      )
    }
  }

  if (!is.null(numParCov) && numParCov != modelParCov) {
    warning("numParCov (", numParCov, ") does not match the ", modelParCov,
      " FREM covariate parameter(s) the model itself has. Using the ",
      "derived count.",
      call. = FALSE
    )
  }
  numParCov <- modelParCov

  ## A parameter can carry IIV through an intermediate variable:
  ##   ETACL = ETA(3)
  ##   CL    = EXP(MU_3 + ETACL)
  ## CL's own assignment has no ETA(), so it looks like a parameter with no
  ## random effect and is emitted as written - silently dropping its FREM
  ## covariate effect. Only an intermediate carrying an eta in the FREM range
  ## matters: one carrying a skip-region eta is documented to go to 0, and one
  ## that is itself a requested FREM parameter has already been spliced, so
  ## the dependent parameter inherits the effect correctly.
  fremRange <- function(e) any(e > numSkipOm & e <= numSkipOm + numParCov)
  etaCarriers <- unique(unlist(lapply(p$statements, function(s) {
    if (fremRange(.fremStmtEtas(s))) .fremStmtAssigns(s) else character(0)
  })))
  etaCarriers <- setdiff(etaCarriers, c(fremParams, names(keepEta)))
  for (nm in parameters[etaCounts == 0L]) {
    via <- intersect(.fremDependsOn(nm, p$statements), setdiff(etaCarriers, nm))
    if (length(via)) {
      warning("'", nm, "' has no ETA() of its own but depends on ",
        paste(via, collapse = ", "), ", which carry a FREM ETA(). It is ",
        "emitted as written, with those ETA() set to 0, so it gets no FREM ",
        "covariate effect. Reference the ETA() directly in its $PK ",
        "assignment, or write this parameter's function by hand.",
        call. = FALSE
      )
    }
  }


  ## ---- prune to the transitive dependencies of `parameters` ----
  need <- union(parameters, secReads)
  repeat {
    before <- length(need)
    for (s in p$statements) {
      if (any(.fremStmtAssigns(s) %in% need)) {
        need <- union(need, .fremStmtUses(s))
      }
    }
    if (length(need) == before) break
  }
  kept <- Filter(function(s) any(.fremStmtAssigns(s) %in% need), p$statements)
  covs <- intersect(p$covariates, need)

  maxTheta <- max(0L, .fremMaxTheta(kept))
  if (maxTheta > numNonFREMThetas) {
    warning("A retained $PK statement references THETA(", maxTheta, "), beyond ",
      "numNonFREMThetas = ", numNonFREMThetas, ". `basethetas` only holds ",
      "the structural thetas, so that index will be out of range - check ",
      "`parameters` and `numNonFREMThetas`.",
      call. = FALSE
    )
  }

  code <- .fremEmit(kept, covs, p$covRef, parameters, fremParams, numSkipOm,
    functionName, fremModel, missVal, quiet,
    secondary = sec, numTotEta = numTotEta,
    fremEtaIdx = fremEtaIdx, keepEta = keepEta
  )
  class(code) <- c("pmxFREMParamFunction", "character")

  if (!is.null(file)) writeLines(code, file)

  if (!quiet) {
    message(
      "Translated $PK of ", basename(fremModel), " for FREM: ",
      length(fremParams), " FREM covariate parameter(s) (",
      paste(fremParams, collapse = ", "), "), ",
      length(parameters) - length(fremParams), " returned as-is; ",
      "numSkipOm = ", numSkipOm, ", numNonFREMThetas = ", numNonFREMThetas,
      ", ", length(covs), " structural covariate(s)",
      if (length(secNames)) {
        paste0(", ", length(secNames), " secondary parameter(s)")
      } else {
        ""
      },
      "."
    )
    for (cov in covs) {
      message(
        "  ", cov, " reference ",
        PMXForest::nmFormatNum(p$covRef[[cov]]$value), " - ",
        p$covRef[[cov]]$source
      )
    }
    if (!is.null(file)) message("Written to ", file)
  }

  list(
    code = code,
    functionListName = c(parameters, secNames),
    primaryNames = parameters,
    secondaryNames = secNames,
    fremParameters = fremParams,
    fremEtaScale = fremEtaScale,
    noBaseThetas = numNonFREMThetas, covRef = p$covRef[covs],
    numParCov = numParCov, numSkipOm = numSkipOm,
    numNonFREMThetas = numNonFREMThetas, fremModel = fremModel,
    missVal = missVal
  )
}

## ---------------------------------------------------------------------------
## Internal: walkers over the nmParsePK() statement / expression trees
## ---------------------------------------------------------------------------

#' Every assignment of `nm`, at any depth, in statement order
#' @keywords internal
#' @noRd
.fremAssignsOf <- function(stmts, nm) {
  out <- list()
  walk <- function(sl) {
    for (s in sl) {
      if (identical(s$type, "assign")) {
        if (identical(s$lhs, nm)) out[[length(out) + 1L]] <<- s
      } else {
        walk(s$then)
        for (e in s$elifs) walk(e$stmts)
        if (!is.null(s$else_)) walk(s$else_)
      }
    }
  }
  walk(stmts)
  out
}


#' ETA() indices referenced anywhere in a statement, if blocks included
#' @keywords internal
#' @noRd
.fremStmtEtas <- function(s) {
  if (identical(s$type, "assign")) {
    return(.fremEtaIndices(s$rhs))
  }
  c(
    .fremEtaIndices(s$cond),
    unlist(lapply(s$then, .fremStmtEtas)),
    unlist(lapply(s$elifs, function(e) {
      c(.fremEtaIndices(e$cond), unlist(lapply(e$stmts, .fremStmtEtas)))
    })),
    if (!is.null(s$else_)) unlist(lapply(s$else_, .fremStmtEtas))
  )
}


#' Every symbol `nm` transitively depends on
#' @keywords internal
#' @noRd
.fremDependsOn <- function(nm, stmts) {
  need <- nm
  repeat {
    before <- length(need)
    for (s in stmts) {
      if (any(.fremStmtAssigns(s) %in% need)) {
        need <- union(need, .fremStmtUses(s))
      }
    }
    if (length(need) == before) break
  }
  setdiff(need, nm)
}


#' ETA() indices referenced anywhere in an expression node
#' @keywords internal
#' @noRd
.fremEtaIndices <- function(node) {
  if (is.null(node)) {
    return(integer(0))
  }
  switch(node$type,
    eta   = as.integer(node$index),
    call  = unlist(lapply(node$args, .fremEtaIndices)),
    unop  = .fremEtaIndices(node$arg),
    binop = c(.fremEtaIndices(node$lhs), .fremEtaIndices(node$rhs)),
    integer(0)
  )
}

#' How the single ETA() of a FREM parameter is enclosed in its $PK line
#'
#' Returns `"exp"` when the parameter is `C * exp(<sum in which ETA appears with
#' coefficient +1>)` - the log-normal form for which
#' [verifyFREMParamFunction()]'s covariate / random-effect splice checks (scale
#' by `exp(.)`) are meaningful - and `"other"` for anything else (additive ETA,
#' logit, `exp(THETA * ETA)`, ETA inside a further transform, ...).
#'
#' @keywords internal
#' @noRd
.fremEtaScale <- function(node) {
  rec <- function(nd, inExp) {
    if (is.null(nd)) {
      return(NA_character_)
    }
    switch(nd$type,
      eta = if (inExp) "exp" else "other",
      num = ,
      sym = ,
      theta = NA_character_,
      call = {
        kids <- vapply(nd$args, function(a) {
          v <- rec(a, inExp || identical(nd$fn, "exp"))
          if (is.na(v)) "" else v
        }, character(1))
        kids <- kids[nzchar(kids)]
        if (length(kids) == 0L) {
          NA_character_
        } else if (!inExp && identical(nd$fn, "exp") && all(kids == "exp")) {
          "exp"
        } else {
          "other"
        }
      },
      unop = {
        v <- rec(nd$arg, inExp)
        if (is.na(v)) {
          NA_character_
        } else if (identical(nd$op, "+")) {
          v
        } else {
          "other"
        } # negation (either side of exp) -> other
      },
      binop = {
        lv <- rec(nd$lhs, inExp)
        rv <- rec(nd$rhs, inExp)
        v <- if (!is.na(lv)) lv else rv
        if (is.na(v)) {
          NA_character_
        } else if (!inExp && (nd$op == "*" || (nd$op == "/" && !is.na(lv)))) {
          v
        } # C * exp(...) or exp(...) / C : multiplicative. C / exp(...) scales
        # by exp(-eta), so an eta in the denominator falls through to "other".
        else if (inExp && nd$op == "+") {
          v
        } # exp(mu + ETA) : additive in ETA
        else {
          "other"
        }
      },
      "other"
    )
  }
  v <- rec(node, FALSE)
  if (is.na(v)) "other" else v
}

#' Symbol names referenced in an expression node
#' @keywords internal
#' @noRd
.fremSyms <- function(node) {
  if (is.null(node)) {
    return(character(0))
  }
  switch(node$type,
    sym   = node$name,
    call  = unlist(lapply(node$args, .fremSyms)),
    unop  = .fremSyms(node$arg),
    binop = c(.fremSyms(node$lhs), .fremSyms(node$rhs)),
    character(0)
  )
}

#' Highest THETA() index in an expression node
#' @keywords internal
#' @noRd
.fremMaxThetaNode <- function(node) {
  if (is.null(node)) {
    return(0L)
  }
  switch(node$type,
    theta = as.integer(node$index),
    call  = max(0L, vapply(node$args, .fremMaxThetaNode, integer(1))),
    unop  = .fremMaxThetaNode(node$arg),
    binop = max(.fremMaxThetaNode(node$lhs), .fremMaxThetaNode(node$rhs)),
    0L
  )
}

#' Variables assigned anywhere inside a statement (an assign, or an if block)
#' @keywords internal
#' @noRd
.fremStmtAssigns <- function(s) {
  if (identical(s$type, "assign")) {
    return(s$lhs)
  }
  c(
    unlist(lapply(s$then, .fremStmtAssigns)),
    unlist(lapply(s$elifs, function(e) unlist(lapply(e$stmts, .fremStmtAssigns)))),
    if (!is.null(s$else_)) unlist(lapply(s$else_, .fremStmtAssigns))
  )
}

#' Symbols used anywhere inside a statement (conditions + right-hand sides)
#' @keywords internal
#' @noRd
.fremStmtUses <- function(s) {
  if (identical(s$type, "assign")) {
    return(.fremSyms(s$rhs))
  }
  c(
    .fremSyms(s$cond),
    unlist(lapply(s$then, .fremStmtUses)),
    unlist(lapply(s$elifs, function(e) {
      c(
        .fremSyms(e$cond),
        unlist(lapply(e$stmts, .fremStmtUses))
      )
    })),
    if (!is.null(s$else_)) unlist(lapply(s$else_, .fremStmtUses))
  )
}

#' Highest THETA() index anywhere in a list of statements
#' @keywords internal
#' @noRd
.fremMaxTheta <- function(stmts) {
  m <- 0L
  walk <- function(s) {
    if (identical(s$type, "assign")) {
      m <<- max(m, .fremMaxThetaNode(s$rhs))
    } else {
      m <<- max(m, .fremMaxThetaNode(s$cond))
      lapply(s$then, walk)
      lapply(s$elifs, function(e) {
        m <<- max(m, .fremMaxThetaNode(e$cond))
        lapply(e$stmts, walk)
      })
      if (!is.null(s$else_)) lapply(s$else_, walk)
    }
  }
  lapply(stmts, walk)
  m
}

## ---------------------------------------------------------------------------
## Internal: emit the source
## ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.fremEmit <- function(stmts, covs, covRef, parameters, fremParams, numSkipOm,
                      functionName, fremModel, missVal, quiet,
                      secondary = list(), numTotEta = NULL,
                      fremEtaIdx = NULL, keepEta = integer(0)) {
  nEtas <- if (is.null(numTotEta)) numSkipOm + length(fremParams) else numTotEta
  dep <- function(node, etaVal = "0") {
    PMXForest::nmDeparse(node, thetaVar = "basethetas", etaValue = etaVal)
  }

  emit <- function(sl, indent) {
    pad <- strrep("  ", indent)
    out <- character(0)
    for (s in sl) {
      if (identical(s$type, "assign")) {
        eIdx <- .fremEtaIndices(s$rhs)
        ## The FREM eta belongs to the parameter, not to the statement: a
        ## parameter can be assigned more than once, and a later assignment
        ## may carry a different eta (an IOV term, say). Only the statement
        ## that carries the parameter's own FREM eta gets the splice.
        myEta <- if (s$lhs %in% names(fremEtaIdx)) {
          fremEtaIdx[[s$lhs]]
        } else {
          NA_integer_
        }
        if (!is.na(myEta) && length(eIdx) == 1L && eIdx[1] == myEta) {
          ## The FREM index comes from the model: the parameter whose $PK line
          ## carries ETA(numSkipOm + k) is the model's k-th FREM parameter.
          ## Never from match(s$lhs, fremParams), which is a rank within the
          ## *request* and silently hands a subset another parameter's
          ## covariate coefficient and eta.
          k <- myEta - numSkipOm
          fremEta <- sprintf("(covthetas[%d] + .eta(etas, %d))", k, myEta)
          out <- c(out, paste0(
            pad, s$lhs, " <- ", dep(s$rhs, fremEta),
            "   # FREM parameter ", k, ": ETA(", myEta,
            ") -> covthetas[", k, "] + etas[", myEta, "]"
          ))
        } else if (!is.na(myEta) && myEta %in% eIdx) {
          ## nmDeparse() substitutes every ETA in the expression, so a
          ## statement carrying the FREM eta *and* another one would count
          ## the covariate coefficient twice and turn the other eta into the
          ## FREM one. There is no correct in-place splice here.
          stop("The assignment of '", s$lhs, "' references ETA(",
            paste(eIdx, collapse = "), ETA("), ") - its FREM eta ETA(", myEta,
            ") together with another. The covariate effect cannot be spliced ",
            "in place; write this parameter's function by hand.",
            call. = FALSE
          )
        } else if (s$lhs %in% names(keepEta)) {
          ## Its eta is inside the skipped omegas, so no covariate coefficient
          ## applies - but the eta itself is real and is kept.
          ki <- keepEta[[s$lhs]]
          out <- c(out, paste0(
            pad, s$lhs, " <- ", dep(s$rhs, sprintf(".eta(etas, %d)", ki)),
            "   # ETA(", ki, ") is inside numSkipOm: kept, no covariate effect"
          ))
        } else if (!is.na(myEta) && length(eIdx) > 0L) {
          ## A further assignment to a FREM parameter that does not carry its
          ## FREM eta - an IOV term, typically. Typical values take it at 0.
          out <- c(out, paste0(
            pad, s$lhs, " <- ", dep(s$rhs),
            "   # ETA() -> 0 (not this parameter's FREM eta)"
          ))
        } else if (s$lhs %in% parameters) {
          note <- if (length(eIdx) == 0L) {
            "   # returned as-is (no IIV / no FREM covariate effect)"
          } else {
            "   # returned as-is; ETA() -> 0 (not a FREM covariate parameter)"
          }
          out <- c(out, paste0(pad, s$lhs, " <- ", dep(s$rhs), note))
        } else {
          txt <- dep(s$rhs)
          note <- if (length(eIdx) > 0L) "   # ETA() -> 0" else ""
          out <- c(out, paste0(pad, s$lhs, " <- ", txt, note))
        }
      } else { # if block
        cond <- dep(s$cond)
        simpleOne <- isTRUE(s$oneline) && length(s$then) == 1L &&
          identical(s$then[[1]]$type, "assign") &&
          length(s$elifs) == 0L && is.null(s$else_)
        if (simpleOne) {
          out <- c(out, paste0(pad, "if (", cond, ") ", trimws(emit(s$then, 0L))))
        } else {
          out <- c(out, paste0(pad, "if (", cond, ") {"), emit(s$then, indent + 1L))
          for (e in s$elifs) {
            out <- c(
              out, paste0(pad, "} else if (", dep(e$cond), ") {"),
              emit(e$stmts, indent + 1L)
            )
          }
          if (!is.null(s$else_)) {
            out <- c(out, paste0(pad, "} else {"), emit(s$else_, indent + 1L))
          }
          out <- c(out, paste0(pad, "}"))
        }
      }
    }
    out
  }

  preamble <- character(0)
  for (cov in covs) {
    r <- covRef[[cov]]
    preamble <- c(preamble, sprintf(
      "  %s <- if (!is.null(dfrow$%s) && dfrow$%s != %s) dfrow$%s else %s   # %s",
      cov, cov, cov, PMXForest::nmFormatNum(missVal), cov,
      PMXForest::nmFormatNum(r$value), r$source
    ))
  }

  body <- emit(stmts, 1L)

  ## secondary parameters: any call-site constants first, then the source
  ## inlined verbatim inside local({ }) so a multi-line string literal (e.g. an
  ## mrgsolve model block) is not re-indented.
  secblock <- character(0)
  for (s in secondary) {
    loc <- if (is.na(s$src)) {
      "inline snippet"
    } else {
      paste0("inlined from ", basename(s$src))
    }
    secblock <- c(secblock, paste0("  ## ", s$name, "  (", loc, ")"))
    consts <- if (is.null(s$consts)) character(0) else s$consts
    ## A "#" would comment out the closing "})" of the one-line form, so a
    ## snippet carrying one takes the block form - as PMXForest's emitter does.
    if (length(s$lines) == 1L && nzchar(trimws(s$lines)) &&
      length(consts) == 0L && !grepl("#", s$lines, fixed = TRUE)) {
      secblock <- c(
        secblock,
        paste0("  ", s$name, " <- local({ ", trimws(s$lines), " })")
      )
    } else {
      secblock <- c(
        secblock, paste0("  ", s$name, " <- local({"),
        if (length(consts) > 0L) paste0("    ", consts),
        s$lines, "  })"
      )
    }
  }

  retNames <- c(parameters, unname(vapply(secondary, `[[`, "", "name")))
  retval <- c(
    "  list(",
    paste0(
      "    ", retNames, " = ", retNames,
      c(rep(",", length(retNames) - 1L), "")
    ),
    "  )"
  )

  c(
    paste0(
      "## Generated by PMXFrem::createFREMParamFunction() from ",
      basename(fremModel), "."
    ),
    "## $PK pruned to what the returned parameters depend on. For the FREM",
    "## covariate parameters the single ETA() reference is replaced in place",
    "## (whatever encloses it) by  covthetas[k] + etas[i], where ETA(i) is the",
    "## reference $PK makes and k = i - numSkipOm is the parameter's FREM index",
    "## in the model. Both are the model's own numbering, so covthetas and etas",
    "## are model-length however few parameters were requested. Every other",
    "## ETA() -> 0. Review against the control stream before use.",
    "",
    paste0(
      functionName,
      " <- function(basethetas, covthetas, dfrow, etas = rep(0, ", nEtas,
      "), ...) {"
    ),
    "",
    "  .eta <- function(e, i) {",
    "    if (length(e) < i) {",
    "      stop(\"etas has \", length(e), \" element(s), but ETA(\", i,",
    "        \") is referenced. Pass the model's full eta vector.\",",
    "        call. = FALSE",
    "      )",
    "    }",
    "    e[i]",
    "  }",
    if (length(secondary)) c("  df <- dfrow   # alias for secondary code") else NULL,
    "",
    if (length(preamble)) {
      c("  ## ---- structural covariate references ----", preamble, "")
    },
    "  ## ---- $PK (pruned) ----",
    body,
    if (length(secblock)) c("", "  ## ---- secondary parameters ----", secblock),
    "",
    retval,
    "}"
  )
}

#' @export
print.pmxFREMParamFunction <- function(x, ...) {
  cat(unclass(x), sep = "\n")
  invisible(x)
}
