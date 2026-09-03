#' Generate a FREM parameter function from the base model's `$PK`
#'
#' @description Translates the `$PK` block of the **base** (pre-FREM) NONMEM
#'   control stream into R source for a parameter function of the shape
#'   `getForestDFFREM()` and `getExplainedVar()` expect,
#'   `function(basethetas, covthetas, dfrow, etas, ...)`. The structural `$PK` is
#'   transliterated as-is (including structural, non-FREM covariates such as
#'   allometric weight). A parameter in `parameters` that carries IIV in `$PK`
#'   (exactly one `ETA()` in its assignment) is treated as a **FREM covariate
#'   parameter**: its `ETA()` reference is replaced **in place** - whatever
#'   encloses it, `exp(mu + ETA)`, `TV * exp(ETA)`, `TV + ETA`, ... - by
#'   `covthetas[k] + etas[numSkipOm + k]`, because the FREM covariate coefficient
#'   is additive on the same (eta) scale. A parameter in `parameters` with no
#'   `ETA()` simply has no covariate effect and is returned as computed by `$PK`.
#'   Every other `ETA(n)` is set to 0.
#'
#'   The result is **source text for you to read, check and edit** - nothing is
#'   evaluated. One generated function serves both uses: call it with `etas = 0`
#'   for a `getForestDFFREM()` forest plot, and with non-zero `etas` for a
#'   `getExplainedVar()` explained-variability plot (the `etas` layout is uniform
#'   across the explained-variability types).
#'
#' @details
#'   **What `covthetas` and `etas` are.** `covthetas[k]` is the FFEM-projected
#'   covariate coefficient for the k-th **FREM covariate parameter** (the k-th
#'   entry of `parameters` that carries an `ETA()`), as computed by [calcFFEM()]
#'   and passed in by `getForestDFFREM()` / `getExplainedVar()`.
#'   `etas` carries the random effects with the `numSkipOm` leading non-FREM
#'   omegas in front, so the k-th parameter's structural eta is
#'   `etas[numSkipOm + k]`; the generated function indexes it with a small
#'   `.eta()` helper that returns 0 when `etas` is shorter (the
#'   explained-variability reference calls pass a long zero vector).
#'
#'   **Structural covariate references.** A covariate named in `$INPUT` and used
#'   but not assigned in `$PK` needs a value for the rows where it is inactive
#'   (`missVal`). The reference is taken from the control stream by the same
#'   rules as [PMXForest::createParamFunction()] and hoisted into a preamble;
#'   override or supply through `covRef`.
#'
#'   **Which parameters get the covariate splice.** A parameter carries a FREM
#'   covariate effect only if it has IIV, i.e. an `ETA()` in `$PK`. So an entry
#'   of `parameters` with exactly one `ETA()` is spliced; one with none is
#'   returned as-is (no covariate effect - not an error); one referencing `ETA()`
#'   more than once is returned as-is with a warning (which eta is "the"
#'   structural one is ambiguous). `covthetas` and `etas[numSkipOm + ...]` are
#'   indexed by position **among the spliced parameters**, in `parameters`
#'   order. If the eta index found for a spliced parameter is not
#'   `numSkipOm + k`, its position is used anyway and a warning is issued.
#'
#' @param baseModel Path to the **base** NONMEM control stream (`.mod` / `.ctl`),
#'   i.e. the model `createFREMmodel()` / `updateFREMmodel()` start from. Its
#'   `$PK` is the structural model; the FREM covariate machinery is added on top.
#' @param parameters Character vector of `$PK` variables the generated function
#'   should return. List the FREM covariate parameters **in the order of the FREM
#'   parameter block** (that order is what `covthetas` / `etas` index); other
#'   `$PK` quantities may be included and are returned as-is.
#' @param numSkipOm Number of diagonal omegas before the FREM block (offsets the
#'   `etas` index). Default 0. [fremModelInfo()] derives it from a FREM model.
#' @param numParCov Optional. If given, a check: it should equal the number of
#'   entries in `parameters` that carry an `ETA()` in `$PK` (the FREM covariate
#'   parameters). A mismatch warns and the derived count is used.
#' @param covRef Optional named list of structural-covariate reference values,
#'   e.g. `list(WT = 75)`, forwarded to [PMXForest::nmParsePK()].
#' @param functionName Name for the generated function. Default `"paramFunction"`.
#' @param extFile Optional path to the base model's `.ext`; when given, the THETA
#'   count is read from its header rather than the `$THETA` records.
#' @param file Optional path to write the generated source to. It is returned
#'   either way.
#' @param missVal The value marking an inactive covariate. Default -99.
#' @param quiet If `FALSE` (default), reports the parameters and structural
#'   covariate references found.
#'
#' @return A list:
#'   \itemize{
#'     \item `code` - the generated R source, a character vector with class
#'       `"pmxFREMParamFunction"` so printing renders it.
#'     \item `functionListName` - `parameters`, for `getForestDFFREM()` /
#'       `getExplainedVar()`.
#'     \item `fremParameters` - the subset of `parameters` that got the covariate
#'       splice (those with an `ETA()` in `$PK`), in order.
#'     \item `noBaseThetas` - the base model's THETA count.
#'     \item `covRef` - the structural-covariate reference used, with its source.
#'     \item `numParCov` - the number of `fremParameters`.
#'     \item `numSkipOm`, `baseModel`, `missVal` - as supplied.
#'   }
#'
#' @seealso [verifyFREMParamFunction()], [PMXForest::nmParsePK()],
#'   [fremModelInfo()], [getForestDFFREM()], [getExplainedVar()].
#'
#' @export
#'
#' @examples
#' baseModel <- system.file("extdata/SimNeb/run30.mod", package = "PMXFrem")
#' out <- createFREMParamFunction(baseModel,
#'                                parameters = c("CL", "V", "MAT"),
#'                                numSkipOm  = 2)
#' cat(out$code, sep = "\n")
#'
#' @family Diagnostics & Plotting
#' @concept diagnostics
createFREMParamFunction <- function(baseModel,
                                    parameters,
                                    numSkipOm    = 0,
                                    numParCov    = NULL,
                                    covRef       = NULL,
                                    functionName = "paramFunction",
                                    extFile      = NULL,
                                    file         = NULL,
                                    missVal      = -99,
                                    quiet        = FALSE) {

  if (!requireNamespace("PMXForest", quietly = TRUE) ||
      !exists("nmParsePK", where = asNamespace("PMXForest"), inherits = FALSE)) {
    stop("createFREMParamFunction() needs a PMXForest that exports nmParsePK(); ",
         "please update PMXForest.", call. = FALSE)
  }
  if (missing(parameters) || length(parameters) < 1) {
    stop("`parameters` must name at least one $PK variable.", call. = FALSE)
  }

  p <- PMXForest::nmParsePK(baseModel, parameters = parameters, covRef = covRef,
                            extFile = extFile, missVal = missVal)

  ## Which of the requested parameters are FREM covariate parameters: those with
  ## exactly one ETA() in their $PK assignment. covthetas[k] / etas[numSkipOm+k]
  ## are indexed by position among these, in `parameters` order. A parameter
  ## with no ETA() just has no covariate effect and is returned as-is.
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

  code <- .fremEmit(p, parameters, fremParams, numSkipOm, functionName,
                    baseModel, missVal, quiet)
  class(code) <- c("pmxFREMParamFunction", "character")

  if (!is.null(file)) writeLines(code, file)

  if (!quiet) {
    message("Translated $PK of ", basename(baseModel), " for FREM: ",
            length(fremParams), " FREM covariate parameter(s) (",
            paste(fremParams, collapse = ", "), "), ",
            length(parameters) - length(fremParams), " returned as-is, ",
            length(p$covariates), " structural covariate(s), ",
            p$noBaseThetas, " base theta(s).")
    for (cov in p$covariates) {
      message("  ", cov, " reference ",
              PMXForest::nmFormatNum(p$covRef[[cov]]$value), " - ",
              p$covRef[[cov]]$source)
    }
    if (!is.null(file)) message("Written to ", file)
  }

  list(code = code, functionListName = parameters, fremParameters = fremParams,
       noBaseThetas = p$noBaseThetas, covRef = p$covRef, numSkipOm = numSkipOm,
       numParCov = numParCov, baseModel = baseModel, missVal = missVal)
}

## ---------------------------------------------------------------------------
## Internal: emit the FREM parameter-function source from an nmParsePK() result
## ---------------------------------------------------------------------------

#' Collect the ETA() indices referenced anywhere in an expression node
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

#' @keywords internal
#' @noRd
.fremEmit <- function(p, parameters, fremParams, numSkipOm, functionName,
                      baseModel, missVal, quiet) {

  nEtas <- numSkipOm + length(fremParams)
  dep   <- function(node, etaVal = "0") {
    PMXForest::nmDeparse(node, thetaVar = "basethetas", etaValue = etaVal)
  }

  emit <- function(stmts, indent) {
    pad <- strrep("  ", indent)
    out <- character(0)
    for (s in stmts) {
      if (identical(s$type, "assign")) {
        k    <- match(s$lhs, fremParams)          # FREM covariate parameter?
        eIdx <- .fremEtaIndices(s$rhs)
        if (!is.na(k)) {
          # FREM covariate parameter: replace its single ETA() reference in
          # place -- whatever encloses it (exp(mu + ETA), TV * exp(ETA),
          # TV + ETA, ...). The FREM covariate coefficient is additive on the
          # eta scale, so it goes exactly where the eta is.
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
          # Requested for the return list but not a FREM covariate parameter.
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
          out <- c(out, paste0(pad, "if (", cond, ") ",
                               trimws(emit(s$then, 0L))))
        } else {
          out <- c(out, paste0(pad, "if (", cond, ") {"),
                        emit(s$then, indent + 1L))
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
  for (cov in p$covariates) {
    r <- p$covRef[[cov]]
    preamble <- c(preamble, sprintf(
      "  %s <- if (!is.null(dfrow$%s) && dfrow$%s != %s) dfrow$%s else %s   # %s",
      cov, cov, cov, PMXForest::nmFormatNum(missVal), cov,
      PMXForest::nmFormatNum(r$value), r$source))
  }

  body   <- emit(p$statements, 1L)
  retval <- c("  list(",
              paste0("    ", parameters, " = ", parameters,
                     c(rep(",", length(parameters) - 1L), "")),
              "  )")

  c(
    paste0("## Generated by PMXFrem::createFREMParamFunction() from ",
           basename(baseModel), "."),
    "## Structural $PK is transliterated. For the FREM covariate parameters the",
    "## single ETA() reference is replaced in place (whatever encloses it) by",
    "## covthetas[k] + etas[numSkipOm + k]; every other ETA() -> 0.",
    "## Review against the control stream before use.",
    "",
    paste0(functionName,
           " <- function(basethetas, covthetas, dfrow, etas = rep(0, ", nEtas,
           "), ...) {"),
    "",
    "  .eta <- function(e, i) if (length(e) >= i) e[i] else 0",
    "",
    if (length(preamble)) {
      c("  ## ---- structural covariate references ----", preamble, "")
    },
    "  ## ---- $PK ----",
    body,
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
