#' Check a generated FREM parameter function
#'
#' @description Runs three consistency checks on the function from
#'   [createFREMParamFunction()], for a set of test covariate rows:
#'   \enumerate{
#'     \item **Structural match.** With `covthetas = 0` and `etas = 0` a FREM
#'       parameter reduces to its structural typical value. The result is
#'       compared with the SCM-style typical-value function that
#'       [PMXForest::createParamFunction()] generates from the same FREM model.
#'     \item **Covariate splice.** Multiplying every `covthetas[k]` in should
#'       scale parameter `k` by `exp(covthetas[k])` and nothing else.
#'     \item **Random-effect splice.** Setting `etas[numSkipOm + k]` should scale
#'       parameter `k` by `exp(etas[numSkipOm + k])` and nothing else - this also
#'       checks the `numSkipOm` offset is right.
#'   }
#'   Anything that fails is reported loudly; the return value records the maximum
#'   relative difference per parameter.
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
#' @return A data frame with one row per parameter: `PARAMETER`,
#'   `STRUCTURAL` / `COVSPLICE` / `ETASPLICE` (max relative difference for each
#'   check) and `PASS` (all three within `tol`).
#'
#' @seealso [createFREMParamFunction()].
#'
#' @export
#'
#' @examples
#' fremModel <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
#' extFile   <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
#' out <- createFREMParamFunction(fremModel, parameters = c("CL", "V", "MAT"),
#'                                extFile = extFile, quiet = TRUE)
#' verifyFREMParamFunction(out, extFile = extFile)
#'
#' @family Diagnostics & Plotting
#' @concept diagnostics
verifyFREMParamFunction <- function(x,
                                    fun     = NULL,
                                    thetas  = NULL,
                                    extFile = NULL,
                                    dfrows  = NULL,
                                    tol     = 1e-6,
                                    quiet   = FALSE) {

  if (!is.list(x) || is.null(x$code) || is.null(x$fremModel)) {
    stop("`x` must be the list returned by createFREMParamFunction().",
         call. = FALSE)
  }
  params    <- x$functionListName
  numSkipOm <- if (is.null(x$numSkipOm)) 0 else x$numSkipOm
  nNonFREM  <- x$noBaseThetas                       # length of `basethetas`
  if (is.null(fun)) fun <- eval(parse(text = x$code))

  ## ---- FREM model THETA final estimates (all of them) ----
  if (is.null(thetas)) {
    if (is.null(extFile)) {
      extFile <- paste0(tools::file_path_sans_ext(x$fremModel), ".ext")
    }
    if (!file.exists(extFile)) {
      stop("Supply `thetas`, or an `extFile` that exists (looked for ",
           extFile, ").", call. = FALSE)
    }
    dfe    <- getExt(extFile = extFile)
    dfe    <- dfe[dfe$ITERATION == -1000000000, , drop = FALSE]
    thetas <- as.numeric(dfe[1, grep("^THETA", names(dfe)), drop = TRUE])
  }
  basethetas <- thetas[seq_len(nNonFREM)]           # what the FREM fn gets

  ## ---- SCM typical-value function from the same FREM model ----
  scm   <- PMXForest::createParamFunction(x$fremModel, parameters = params,
                                          extFile = extFile, quiet = TRUE)
  scmFn <- eval(parse(text = scm$code))
  scmTh <- thetas[seq_len(scm$noBaseThetas)]        # SCM fn gets all thetas

  ## ---- test rows ----
  if (is.null(dfrows)) {
    covs   <- names(x$covRef)
    dfrows <- if (length(covs)) {
      as.data.frame(stats::setNames(as.list(rep(x$missVal, length(covs))), covs))
    } else {
      data.frame(row.names = 1L)
    }
  }
  np      <- length(params)
  nEtas   <- numSkipOm + np
  relDiff <- function(a, b) max(abs((unlist(a) - unlist(b)) /
                                      ifelse(unlist(b) == 0, 1, unlist(b))))

  structD <- covD <- etaD <- rep(0, np)
  names(structD) <- names(covD) <- names(etaD) <- params

  for (i in seq_len(nrow(dfrows))) {
    dfrow <- dfrows[i, , drop = FALSE]

    base0 <- fun(basethetas, covthetas = rep(0, np), dfrow = dfrow,
                 etas = rep(0, nEtas))
    scm0  <- scmFn(thetas = scmTh, df = dfrow)
    for (p in params) {
      structD[p] <- max(structD[p],
                        abs((base0[[p]] - scm0[[p]]) /
                              ifelse(scm0[[p]] == 0, 1, scm0[[p]])))
    }

    ct   <- seq_len(np) / 7
    covV <- fun(basethetas, covthetas = ct, dfrow = dfrow, etas = rep(0, nEtas))
    for (k in seq_len(np)) {
      exp_k <- unlist(base0)
      exp_k[k] <- exp_k[k] * exp(ct[k])
      covD[k] <- max(covD[k],
                     abs((covV[[k]] - exp_k[k]) /
                           ifelse(exp_k[k] == 0, 1, exp_k[k])))
    }

    for (k in seq_len(np)) {
      e <- rep(0, nEtas); e[numSkipOm + k] <- 0.3
      etaV  <- fun(basethetas, covthetas = rep(0, np), dfrow = dfrow, etas = e)
      exp_k <- unlist(base0)
      exp_k[k] <- exp_k[k] * exp(0.3)
      for (j in seq_len(np)) {
        etaD[j] <- max(etaD[j],
                       abs((etaV[[j]] - exp_k[j]) /
                             ifelse(exp_k[j] == 0, 1, exp_k[j])))
      }
    }
  }

  out <- data.frame(
    PARAMETER  = params,
    STRUCTURAL = structD,
    COVSPLICE  = covD,
    ETASPLICE  = etaD,
    row.names  = NULL,
    stringsAsFactors = FALSE
  )
  out$PASS <- with(out, STRUCTURAL <= tol & COVSPLICE <= tol & ETASPLICE <= tol)

  if (!quiet) {
    message("verifyFREMParamFunction(): ", sum(out$PASS), "/", nrow(out),
            " parameter(s) pass (tol ", tol, ").")
    for (i in seq_len(nrow(out))) {
      message("  ", out$PARAMETER[i], ": ",
              if (out$PASS[i]) "pass" else "FAIL",
              "  (structural ", signif(out$STRUCTURAL[i], 3),
              ", cov ", signif(out$COVSPLICE[i], 3),
              ", eta ", signif(out$ETASPLICE[i], 3), ")")
    }
  }

  invisible(out)
}
