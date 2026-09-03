#' Generate a FREM parameter function from the base model's `$PK`
#'
#' @description Translates the `$PK` block of the **base** (pre-FREM) NONMEM
#'   control stream into R source for a parameter function of the shape
#'   `getForestDFFREM()` and `getExplainedVar()` expect,
#'   `function(basethetas, covthetas, dfrow, etas, ...)`. The structural `$PK` is
#'   transliterated as-is (including structural, non-FREM covariates such as
#'   allometric weight); for the parameters named in `parameters` the FREM
#'   covariate coefficient and random effect are spliced into the exponent, so
#'   the parameter becomes `<structural> * exp(... + covthetas[k] + etas[k])`.
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
#'   covariate coefficient for the k-th parameter in `parameters`, as computed by
#'   [calcFFEM()] and passed in by `getForestDFFREM()` / `getExplainedVar()`.
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
#'   **One `ETA()` per FREM parameter.** A parameter written with no `ETA()` in
#'   `$PK` still gets the FREM covariate effect, applied as
#'   `(<structural>) * exp(covthetas[k] + etas[k])`, with a warning. A parameter
#'   referencing `ETA()` more than once in its assignment is an error.
#'
#' @param baseModel Path to the **base** NONMEM control stream (`.mod` / `.ctl`),
#'   i.e. the model `createFREMmodel()` / `updateFREMmodel()` start from. Its
#'   `$PK` is the structural model; the FREM covariate machinery is added on top.
#' @param parameters Character vector of `$PK` variables that carry FREM
#'   covariate effects, **in the order of the FREM parameter block** - this order
#'   is what `covthetas[k]` and `etas[numSkipOm + k]` index.
#' @param numSkipOm Number of diagonal omegas before the FREM block (offsets the
#'   `etas` index). Default 0. [fremModelInfo()] derives it from a FREM model.
#' @param numParCov Number of FREM covariate parameters. Must equal
#'   `length(parameters)`; kept as an explicit argument so a mismatch is caught.
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
#'     \item `noBaseThetas` - the base model's THETA count.
#'     \item `covRef` - the structural-covariate reference used, with its source.
#'     \item `numSkipOm`, `numParCov`, `baseModel`, `missVal` - as supplied.
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
                                    numParCov    = length(parameters),
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
  if (numParCov != length(parameters)) {
    stop("numParCov (", numParCov, ") must equal length(parameters) (",
         length(parameters), "): covthetas / etas are indexed by parameter ",
         "position.", call. = FALSE)
  }

  p <- PMXForest::nmParsePK(baseModel, parameters = parameters, covRef = covRef,
                            extFile = extFile, missVal = missVal)

  code <- .fremEmit(p, parameters, numSkipOm, functionName, baseModel, missVal, quiet)
  class(code) <- c("pmxFREMParamFunction", "character")

  if (!is.null(file)) writeLines(code, file)

  if (!quiet) {
    message("Translated $PK of ", basename(baseModel), " for FREM: ",
            length(parameters), " FREM parameter(s), ", length(p$covariates),
            " structural covariate(s), ", p$noBaseThetas, " base theta(s).")
    for (cov in p$covariates) {
      message("  ", cov, " reference ",
              PMXForest::nmFormatNum(p$covRef[[cov]]$value), " - ",
              p$covRef[[cov]]$source)
    }
    if (!is.null(file)) message("Written to ", file)
  }

  list(code = code, functionListName = parameters, noBaseThetas = p$noBaseThetas,
       covRef = p$covRef, numSkipOm = numSkipOm, numParCov = numParCov,
       baseModel = baseModel, missVal = missVal)
}

## ---------------------------------------------------------------------------
## Internal: emit the FREM parameter-function source from an nmParsePK() result
## ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.fremEtaCount <- function(node) {
  if (is.null(node)) return(0L)
  switch(node$type,
    eta   = 1L,
    call  = sum(vapply(node$args, .fremEtaCount, integer(1))),
    unop  = .fremEtaCount(node$arg),
    binop = .fremEtaCount(node$lhs) + .fremEtaCount(node$rhs),
    0L)
}

#' @keywords internal
#' @noRd
.fremEmit <- function(p, parameters, numSkipOm, functionName, baseModel, missVal,
                      quiet) {

  nEtas <- numSkipOm + length(parameters)
  dep   <- function(node, etaVal = "0") {
    PMXForest::nmDeparse(node, thetaVar = "basethetas", etaValue = etaVal)
  }

  emit <- function(stmts, indent) {
    pad <- strrep("  ", indent)
    out <- character(0)
    for (s in stmts) {
      if (identical(s$type, "assign")) {
        k <- match(s$lhs, parameters)
        if (!is.na(k)) {
          ne      <- .fremEtaCount(s$rhs)
          fremEta <- sprintf("(covthetas[%d] + .eta(etas, %d))", k, numSkipOm + k)
          if (ne == 1L) {
            out <- c(out, paste0(pad, s$lhs, " <- ", dep(s$rhs, fremEta),
                                 "   # FREM parameter ", k))
          } else if (ne == 0L) {
            out <- c(out, paste0(pad, s$lhs, " <- (", dep(s$rhs), ") * exp(",
                                 fremEta, ")   # FREM parameter ", k,
                                 " (no ETA in $PK)"))
            if (!quiet) {
              warning("Parameter '", s$lhs, "' has no ETA() in $PK; the FREM ",
                      "covariate effect and random effect were applied as ",
                      "`(<structural>) * exp(covthetas + eta)`. Confirm this ",
                      "matches the model.", call. = FALSE)
            }
          } else {
            stop("Parameter '", s$lhs, "' references ETA() ", ne, " times in its ",
                 "$PK assignment; createFREMParamFunction() handles one. Rework ",
                 "$PK or write this parameter by hand.", call. = FALSE)
          }
        } else {
          txt  <- dep(s$rhs)
          note <- if (.fremEtaCount(s$rhs) > 0L) "   # ETA() -> 0" else ""
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
    "## Structural $PK is transliterated; ETA() -> 0 except the FREM parameters,",
    "## where  covthetas[k] + etas[numSkipOm + k]  is spliced into the exponent.",
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
