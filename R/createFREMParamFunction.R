#' Generate a FREM parameter function from the FREM model's `$PK`
#'
#' @description Translates the `$PK` block of the **FREM** model into R source for
#'   a parameter function of the shape `getForestDFFREM()` and `getExplainedVar()`
#'   expect, `function(basethetas, covthetas, dfrow, etas, ...)`.
#'
#'   The structural `$PK` is transliterated (structural, non-FREM covariates such
#'   as allometric weight kept). A parameter in `parameters` that carries IIV in
#'   `$PK` (exactly one `ETA()` in its assignment) is a **FREM covariate
#'   parameter**: its `ETA()` reference is replaced **in place** - whatever
#'   encloses it, `exp(mu + ETA)`, `TV * exp(ETA)`, `TV + ETA`, ... - by
#'   `covthetas[k] + etas[numSkipOm + k]`, because the FREM covariate coefficient
#'   is additive on the same (eta) scale. A parameter with no `ETA()` is returned
#'   as `$PK` computes it (no covariate effect - not an error). Every other
#'   `ETA(n)` is set to 0.
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
#'   **What `covthetas` and `etas` are.** `covthetas[k]` is the FFEM-projected
#'   covariate coefficient for the k-th FREM covariate parameter (the k-th entry
#'   of `parameters` that carries an `ETA()`), as computed by [calcFFEM()] and
#'   passed in by `getForestDFFREM()` / `getExplainedVar()`. `etas` carries the
#'   random effects with the `numSkipOm` leading non-FREM omegas in front, so the
#'   k-th parameter's structural eta is `etas[numSkipOm + k]`; the generated
#'   function indexes it with a small `.eta()` helper that returns 0 when `etas`
#'   is shorter (the explained-variability reference calls pass a long zero
#'   vector).
#'
#'   **Structural covariate references.** A covariate named in `$INPUT` and used
#'   but not assigned in `$PK` needs a value for the rows where it is inactive
#'   (`missVal`). The reference is taken from the control stream by the same
#'   rules as [PMXForest::createParamFunction()] and hoisted into a preamble;
#'   override or supply through `covRef`.
#'
#'   **Which parameters get the covariate splice.** Exactly one `ETA()` -> the
#'   parameter is spliced. None -> returned as-is (no covariate effect). More
#'   than one -> returned as-is with a warning (which eta is "the" structural one
#'   is ambiguous). `covthetas` and `etas[numSkipOm + ...]` are indexed by
#'   position **among the spliced parameters**, in `parameters` order. If the eta
#'   index found for a spliced parameter is not `numSkipOm + k`, its position is
#'   used anyway and a warning is issued.
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
#'   ext via [fremModelInfo()]; a supplied value that disagrees with the derived
#'   one warns and is kept.
#' @param dfext,extFile A `getExt()` data frame / an `.ext` path, used only to
#'   derive `numSkipOm` / `numNonFREMThetas`. Defaults to the ext located from
#'   `runno` / `modName` / `modDevDir`, else the `.ext` beside `fremModel`.
#' @param numParCov Optional cross-check: should equal the number of
#'   `parameters` that carry an `ETA()` in `$PK`. A mismatch warns.
#' @param covRef Optional named list of structural-covariate reference values,
#'   e.g. `list(WT = 75)`, forwarded to [PMXForest::nmParsePK()].
#' @param functionName Name for the generated function. Default `"paramFunction"`.
#' @param file Optional path to write the generated source to. It is returned
#'   either way.
#' @param missVal The value marking an inactive covariate. Default -99.
#' @param quiet If `FALSE` (default), reports what was found.
#' @param secondary Optional named list of secondary parameters to append to the
#'   generated function's return list. Each entry is a single string: either R
#'   code whose last value is the result (`list(AUC = "dfrow$DOSE / CL")`) or the
#'   path to an `.R` file of arbitrary code, e.g. an `mrgsolve` simulation. The
#'   code is spliced in inside `local({ ... })` and sees `basethetas`,
#'   `covthetas`, `dfrow` (also as `df`), `etas`, `...` and every structural
#'   parameter by name; covariate columns are `dfrow$NAME`. Handled by
#'   [PMXForest::nmResolveSecondary()]; see [PMXForest::createParamFunction()].
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
#'     \item `noBaseThetas` - `numNonFREMThetas` (the length of `basethetas`).
#'     \item `covRef` - the structural-covariate reference used, with its source.
#'     \item `numParCov` - the number of `fremParameters`.
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
#'   parameters = c("CL", "V", "MAT"))
#' stopifnot(identical(out$code, out2$code))
#'
#' @family Diagnostics & Plotting
#' @concept diagnostics
createFREMParamFunction <- function(fremModel        = NULL,
                                    parameters,
                                    numSkipOm        = NULL,
                                    numNonFREMThetas = NULL,
                                    runno            = NULL,
                                    modName          = NULL,
                                    modDevDir        = NULL,
                                    dfext            = NULL,
                                    extFile          = NULL,
                                    numParCov        = NULL,
                                    covRef           = NULL,
                                    functionName     = "paramFunction",
                                    file             = NULL,
                                    missVal          = -99,
                                    quiet            = FALSE,
                                    secondary        = NULL) {

  if (!requireNamespace("PMXForest", quietly = TRUE) ||
      !exists("nmParsePK", where = asNamespace("PMXForest"), inherits = FALSE) ||
      !exists("nmResolveSecondary", where = asNamespace("PMXForest"),
              inherits = FALSE)) {
    stop("createFREMParamFunction() needs PMXForest (>= 1.2.15.9006), which ",
         "exports nmParsePK() and nmResolveSecondary(); please update PMXForest.",
         call. = FALSE)
  }
  if (missing(parameters) || length(parameters) < 1) {
    stop("`parameters` must name at least one $PK variable.", call. = FALSE)
  }

  ## ---- locate the FREM model / ext ----
  if (is.null(fremModel)) {
    if (is.null(runno) && is.null(modName)) {
      stop("Supply `fremModel`, or `runno` / `modName` (+ `modDevDir`).",
           call. = FALSE)
    }
    fn        <- getFileNames(runno = runno, modName = modName,
                              modDevDir = if (is.null(modDevDir)) "." else modDevDir)
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
             "(`dfext` / `extFile` / `runno`) to derive them.", call. = FALSE)
      }
      dfext <- getExt(extFile = extFile)
    }
    .info            <- fremModelInfo(modFile = fremModel, dfext = dfext,
                                      numNonFREMThetas = numNonFREMThetas,
                                      numSkipOm = numSkipOm)
    numSkipOm        <- .info$numSkipOm
    numNonFREMThetas <- .info$numNonFREMThetas
  }

  ## ---- parse the FREM model's $PK ----
  p <- PMXForest::nmParsePK(fremModel, parameters = parameters, covRef = covRef,
                            missVal = missVal)

  ## ---- classify the requested parameters by ETA count in their $PK line ----
  etaCounts <- vapply(parameters, function(nm) {
    a <- Find(function(s) identical(s$type, "assign") && identical(s$lhs, nm),
              p$statements)
    length(.fremEtaIndices(a$rhs))
  }, integer(1))
  fremParams <- parameters[etaCounts == 1L]

  if (!is.null(numParCov) && numParCov != length(fremParams)) {
    warning("numParCov (", numParCov, ") does not match the ",
            length(fremParams), " parameter(s) in `parameters` that carry a ",
            "single ETA() in $PK (", paste(fremParams, collapse = ", "),
            "). Using the derived count.", call. = FALSE)
  }
  numParCov <- length(fremParams)

  ## ---- prune to the transitive dependencies of `parameters` ----
  need <- parameters
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
            "`parameters` and `numNonFREMThetas`.", call. = FALSE)
  }

  sec      <- PMXForest::nmResolveSecondary(secondary, quiet = quiet)
  secNames <- unname(vapply(sec, `[[`, "", "name"))

  code <- .fremEmit(kept, covs, p$covRef, parameters, fremParams, numSkipOm,
                    functionName, fremModel, missVal, quiet, secondary = sec)
  class(code) <- c("pmxFREMParamFunction", "character")

  if (!is.null(file)) writeLines(code, file)

  if (!quiet) {
    message("Translated $PK of ", basename(fremModel), " for FREM: ",
            length(fremParams), " FREM covariate parameter(s) (",
            paste(fremParams, collapse = ", "), "), ",
            length(parameters) - length(fremParams), " returned as-is; ",
            "numSkipOm = ", numSkipOm, ", numNonFREMThetas = ", numNonFREMThetas,
            ", ", length(covs), " structural covariate(s)",
            if (length(secNames))
              paste0(", ", length(secNames), " secondary parameter(s)") else "",
            ".")
    for (cov in covs) {
      message("  ", cov, " reference ",
              PMXForest::nmFormatNum(p$covRef[[cov]]$value), " - ",
              p$covRef[[cov]]$source)
    }
    if (!is.null(file)) message("Written to ", file)
  }

  list(code = code,
       functionListName = c(parameters, secNames),
       primaryNames     = parameters,
       secondaryNames   = secNames,
       fremParameters = fremParams,
       noBaseThetas = numNonFREMThetas, covRef = p$covRef[covs],
       numParCov = numParCov, numSkipOm = numSkipOm,
       numNonFREMThetas = numNonFREMThetas, fremModel = fremModel,
       missVal = missVal)
}

## ---------------------------------------------------------------------------
## Internal: walkers over the nmParsePK() statement / expression trees
## ---------------------------------------------------------------------------

#' ETA() indices referenced anywhere in an expression node
#' @keywords internal
#' @noRd
.fremEtaIndices <- function(node) {
  if (is.null(node)) return(integer(0))
  switch(node$type,
    eta   = as.integer(node$index),
    call  = unlist(lapply(node$args, .fremEtaIndices)),
    unop  = .fremEtaIndices(node$arg),
    binop = c(.fremEtaIndices(node$lhs), .fremEtaIndices(node$rhs)),
    integer(0))
}

#' Symbol names referenced in an expression node
#' @keywords internal
#' @noRd
.fremSyms <- function(node) {
  if (is.null(node)) return(character(0))
  switch(node$type,
    sym   = node$name,
    call  = unlist(lapply(node$args, .fremSyms)),
    unop  = .fremSyms(node$arg),
    binop = c(.fremSyms(node$lhs), .fremSyms(node$rhs)),
    character(0))
}

#' Highest THETA() index in an expression node
#' @keywords internal
#' @noRd
.fremMaxThetaNode <- function(node) {
  if (is.null(node)) return(0L)
  switch(node$type,
    theta = as.integer(node$index),
    call  = max(0L, vapply(node$args, .fremMaxThetaNode, integer(1))),
    unop  = .fremMaxThetaNode(node$arg),
    binop = max(.fremMaxThetaNode(node$lhs), .fremMaxThetaNode(node$rhs)),
    0L)
}

#' Variables assigned anywhere inside a statement (an assign, or an if block)
#' @keywords internal
#' @noRd
.fremStmtAssigns <- function(s) {
  if (identical(s$type, "assign")) return(s$lhs)
  c(unlist(lapply(s$then, .fremStmtAssigns)),
    unlist(lapply(s$elifs, function(e) unlist(lapply(e$stmts, .fremStmtAssigns)))),
    if (!is.null(s$else_)) unlist(lapply(s$else_, .fremStmtAssigns)))
}

#' Symbols used anywhere inside a statement (conditions + right-hand sides)
#' @keywords internal
#' @noRd
.fremStmtUses <- function(s) {
  if (identical(s$type, "assign")) return(.fremSyms(s$rhs))
  c(.fremSyms(s$cond),
    unlist(lapply(s$then, .fremStmtUses)),
    unlist(lapply(s$elifs, function(e) c(.fremSyms(e$cond),
                                         unlist(lapply(e$stmts, .fremStmtUses))))),
    if (!is.null(s$else_)) unlist(lapply(s$else_, .fremStmtUses)))
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
      lapply(s$elifs, function(e) { m <<- max(m, .fremMaxThetaNode(e$cond)); lapply(e$stmts, walk) })
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
                      secondary = list()) {

  nEtas <- numSkipOm + length(fremParams)
  dep   <- function(node, etaVal = "0") {
    PMXForest::nmDeparse(node, thetaVar = "basethetas", etaValue = etaVal)
  }

  emit <- function(sl, indent) {
    pad <- strrep("  ", indent)
    out <- character(0)
    for (s in sl) {
      if (identical(s$type, "assign")) {
        k    <- match(s$lhs, fremParams)
        eIdx <- .fremEtaIndices(s$rhs)
        if (!is.na(k)) {
          fremEta <- sprintf("(covthetas[%d] + .eta(etas, %d))", k, numSkipOm + k)
          if (eIdx != numSkipOm + k) {
            warning("Parameter '", s$lhs, "' (FREM parameter ", k, ") uses ETA(",
                    eIdx, ") in $PK, but ETA(", numSkipOm + k, ") was expected ",
                    "for numSkipOm = ", numSkipOm, ". Proceeding by parameter ",
                    "position; check numSkipOm and the order of `parameters`.",
                    call. = FALSE)
          }
          out <- c(out, paste0(pad, s$lhs, " <- ", dep(s$rhs, fremEta),
                               "   # FREM parameter ", k, ": ETA(", eIdx,
                               ") -> covthetas[", k, "] + etas[", numSkipOm + k, "]"))
        } else if (s$lhs %in% parameters) {
          note <- if (length(eIdx) == 0L) {
            "   # returned as-is (no IIV / no FREM covariate effect)"
          } else {
            warning("Parameter '", s$lhs, "' references ETA() ", length(eIdx),
                    " times in its $PK assignment (ETA ",
                    paste(eIdx, collapse = ", "), "); it is not treated as a ",
                    "FREM covariate parameter. If it should carry a covariate ",
                    "effect, write it by hand.", call. = FALSE)
            "   # returned as-is; ETA() -> 0"
          }
          out <- c(out, paste0(pad, s$lhs, " <- ", dep(s$rhs), note))
        } else {
          txt  <- dep(s$rhs)
          note <- if (length(eIdx) > 0L) "   # ETA() -> 0" else ""
          out  <- c(out, paste0(pad, s$lhs, " <- ", txt, note))
        }
      } else {  # if block
        cond      <- dep(s$cond)
        simpleOne <- isTRUE(s$oneline) && length(s$then) == 1L &&
          identical(s$then[[1]]$type, "assign") &&
          length(s$elifs) == 0L && is.null(s$else_)
        if (simpleOne) {
          out <- c(out, paste0(pad, "if (", cond, ") ", trimws(emit(s$then, 0L))))
        } else {
          out <- c(out, paste0(pad, "if (", cond, ") {"), emit(s$then, indent + 1L))
          for (e in s$elifs) {
            out <- c(out, paste0(pad, "} else if (", dep(e$cond), ") {"),
                          emit(e$stmts, indent + 1L))
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
      PMXForest::nmFormatNum(r$value), r$source))
  }

  body <- emit(stmts, 1L)

  ## secondary parameters: inlined verbatim inside local({ }) so a multi-line
  ## string literal (e.g. an mrgsolve model block) is not re-indented.
  secblock <- character(0)
  for (s in secondary) {
    loc <- if (is.na(s$src)) "inline snippet"
           else paste0("inlined from ", basename(s$src))
    secblock <- c(secblock, paste0("  ## ", s$name, "  (", loc, ")"))
    if (length(s$lines) == 1L && nzchar(trimws(s$lines))) {
      secblock <- c(secblock,
                    paste0("  ", s$name, " <- local({ ", trimws(s$lines), " })"))
    } else {
      secblock <- c(secblock, paste0("  ", s$name, " <- local({"),
                    s$lines, "  })")
    }
  }

  retNames <- c(parameters, unname(vapply(secondary, `[[`, "", "name")))
  retval   <- c("  list(",
                paste0("    ", retNames, " = ", retNames,
                       c(rep(",", length(retNames) - 1L), "")),
                "  )")

  c(
    paste0("## Generated by PMXFrem::createFREMParamFunction() from ",
           basename(fremModel), "."),
    "## $PK pruned to what the returned parameters depend on. For the FREM",
    "## covariate parameters the single ETA() reference is replaced in place",
    "## (whatever encloses it) by  covthetas[k] + etas[numSkipOm + k]; every",
    "## other ETA() -> 0. Review against the control stream before use.",
    "",
    paste0(functionName,
           " <- function(basethetas, covthetas, dfrow, etas = rep(0, ", nEtas,
           "), ...) {"),
    "",
    "  .eta <- function(e, i) if (length(e) >= i) e[i] else 0",
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
