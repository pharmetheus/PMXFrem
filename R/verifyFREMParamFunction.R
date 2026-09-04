#' Check a generated FREM parameter function
#'
#' @description Runs three consistency checks on the function from
#'   [createFREMParamFunction()], for a set of test covariate rows:
#'   \enumerate{
#'     \item **Structural match.** With `covthetas = 0` and `etas = 0` a FREM
#'       parameter reduces to its structural typical value. The result is
#'       compared with the SCM-style typical-value function that
#'       [PMXForest::createParamFunction()] generates from the same FREM model.
#'     \item **Covariate splice.** Setting `covthetas[k]` should scale parameter
#'       `k` by `exp(covthetas[k])` and nothing else.
#'     \item **Random-effect splice.** Setting `etas[numSkipOm + k]` should scale
#'       parameter `k` by `exp(etas[numSkipOm + k])` and nothing else - this also
#'       checks the `numSkipOm` offset is right.
#'   }
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
#' out <- createFREMParamFunction(fremModel, parameters = c("CL", "V", "MAT"),
#'                                quiet = TRUE)
#'
#' if (verifyFREMParamFunction(out, quiet = TRUE)) message("generated function checks out")
#'
#' v <- verifyFREMParamFunction(out, quiet = TRUE)
#' attr(v, "checks")          # per-parameter detail
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
  ## Check the $PK parameters only; `secondary` quantities (AUC, Cmax, ...) are
  ## not comparable to PMXForest::createParamFunction() and have no eta/cov
  ## splice to probe.
  params <- if (!is.null(x$primaryNames)) {
    x$primaryNames
  } else {
    setdiff(x$functionListName,
            if (is.null(x$secondaryNames)) character(0) else x$secondaryNames)
  }
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

  ## Which parameters are log-normal (P = C * exp(<linear in ETA>))? Only those
  ## can be checked with the exp() splice; the rest get NA. `fremEtaScale` is
  ## absent on objects made before this was recorded - assume "exp" then.
  scale   <- if (is.null(x$fremEtaScale)) {
    stats::setNames(rep("exp", np), params)
  } else {
    s <- x$fremEtaScale[params]
    s[is.na(s)] <- "exp"                      # not a FREM covariate parameter
    stats::setNames(s, params)
  }
  isExp   <- scale == "exp"

  structD <- covD <- etaD <- rep(0, np)
  covD[!isExp] <- etaD[!isExp] <- NA_real_
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
    for (k in which(isExp)) {
      exp_k <- unlist(base0)
      exp_k[k] <- exp_k[k] * exp(ct[k])
      covD[k] <- max(covD[k],
                     abs((covV[[k]] - exp_k[k]) /
                           ifelse(exp_k[k] == 0, 1, exp_k[k])))
    }

    for (k in which(isExp)) {
      e <- rep(0, nEtas); e[numSkipOm + k] <- 0.3
      etaV  <- fun(basethetas, covthetas = rep(0, np), dfrow = dfrow, etas = e)
      exp_k <- unlist(base0)
      exp_k[k] <- exp_k[k] * exp(0.3)
      # only parameter k should move; a non-exp parameter j is left out of the
      # comparison (its own splice was not applied here, so it must be unchanged)
      for (j in which(isExp)) {
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
  # NA COVSPLICE / ETASPLICE (non-log-normal parameter) -> PASS is NA when the
  # structural check passes, FALSE when it does not.
  out$PASS <- with(out, STRUCTURAL <= tol & COVSPLICE <= tol & ETASPLICE <= tol)

  nFail <- sum(out$PASS %in% FALSE)
  nNA   <- sum(is.na(out$PASS))

  if (!quiet) {
    message("verifyFREMParamFunction(): ", sum(out$PASS %in% TRUE), "/",
            nrow(out), " parameter(s) pass (tol ", tol, ")",
            if (nNA) paste0(", ", nNA, " not checked (non-log-normal)") else "",
            ".")
    for (i in seq_len(nrow(out))) {
      status <- if (isTRUE(out$PASS[i])) "pass" else if (is.na(out$PASS[i])) {
        "not checked (non-log-normal; structural OK)"
      } else "FAIL"
      message("  ", out$PARAMETER[i], ": ", status,
              "  (structural ", signif(out$STRUCTURAL[i], 3),
              ", cov ", signif(out$COVSPLICE[i], 3),
              ", eta ", signif(out$ETASPLICE[i], 3), ")")
    }
  }

  ## A single logical for use in `if`: FALSE only if a check actually failed;
  ## a non-log-normal parameter (PASS = NA) does not make it FALSE.
  invisible(structure(nFail == 0L, class = "pmxFREMVerify", checks = out))
}

#' @export
print.pmxFREMVerify <- function(x, ...) {
  d    <- attr(x, "checks")
  nNA  <- sum(is.na(d$PASS))
  cat(if (isTRUE(unclass(x)[1])) "PASS" else "FAIL",
      " - verifyFREMParamFunction: ", sum(d$PASS %in% TRUE), "/", nrow(d),
      " parameter(s)",
      if (nNA) paste0(" (", nNA, " not checked - non-log-normal)") else "",
      "\n", sep = "")
  print(d, row.names = FALSE)
  invisible(x)
}
