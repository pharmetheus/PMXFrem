#' Derive the structural integer arguments of a FREM model
#'
#' Works out `numNonFREMThetas`, `numSkipOm`, `numParCov`, `numFREMThetas` and
#' `numSigmas` from a FREM model file and its `.ext` (or a `getSamples()` result),
#' so the user does not have to supply them by hand to the FREM analysis
#' functions.
#'
#' The derivation is:
#'
#' \preformatted{
#' numFREMThetas    = length(getCovNames(modFile)$covNames)
#' numTotThetas     = number of THETA columns in `dfext`
#' numNonFREMThetas = numTotThetas - numFREMThetas
#' numTotEta        = solve k(k+1)/2 = number of OMEGA columns in `dfext`
#' blockN           = N in the final "$OMEGA BLOCK(N)" record of `modFile`
#' numParCov        = blockN - numFREMThetas
#' numSkipOm        = numTotEta - blockN
#' numSigmas        = number of SIGMA columns in `dfext`
#' }
#'
#' `numSkipOm` is taken as `numTotEta - blockN` rather than by counting the simple
#' `$OMEGA` records before the block; that is robust to a structural correlation
#' `$OMEGA BLOCK(k)` sitting before the FREM block.
#'
#' @param modFile Path to the FREM model file. Must contain the PsN-style
#'   `;;;FREM CODE BEGIN COMPACT` / `;;;FREM CODE END COMPACT` markers (models from
#'   `createFREMmodel()`, `updateFREMmodel()` and `createMinimalFremModel()` all
#'   carry them) and a final `$OMEGA BLOCK(N)` record.
#' @param dfext A data frame with `THETA*` / `OMEGA*` / `SIGMA*` columns - a
#'   `getExt()` result or a `PMXForest::getSamples()` result - or a path to a
#'   NONMEM `.ext` file.
#' @param numNonFREMThetas,numSkipOm Optional. If supplied they are returned as
#'   given (an override), but a value that disagrees with the one derived from
#'   the model triggers a warning. Leave `NULL` (default) to use the derived
#'   value.
#'
#' @return A list with components `numNonFREMThetas`, `numFREMThetas`,
#'   `numParCov`, `numSkipOm`, `numSigmas`, `numTotEta`, `numTotThetas`,
#'   `covNames`, `orgCovNames` and `polyCatCovs`.
#'
#' @examples
#' modFile <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
#' extFile <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
#' fremModelInfo(modFile, extFile)
#'
#' @family NONMEM Parsers
#' @concept nonmem_parsers
#' @export
fremModelInfo <- function(modFile,
                          dfext,
                          numNonFREMThetas = NULL,
                          numSkipOm        = NULL) {

  ## ---- ext: accept a data.frame or a path -------------------------------
  if (is.character(dfext) && length(dfext) == 1L) {
    dfext <- getExt(extFile = dfext)
  }
  if (!is.data.frame(dfext)) {
    stop("`dfext` must be a data.frame from getExt() / getSamples(), or a path to a NONMEM .ext file.")
  }

  ## ---- FREM covariates from the model (stops if not a FREM model) -------
  cn            <- getCovNames(modFile = modFile)
  numFREMThetas <- length(cn$covNames)

  ## ---- counts from the ext columns (getExt and getSamples shapes both) --
  nTheta <- length(grep("THETA", names(dfext)))
  nOmCol <- length(grep("OMEGA", names(dfext)))
  nSigma <- length(grep("SIGMA", names(dfext)))
  if (nTheta == 0 || nOmCol == 0) {
    stop("Could not find THETA / OMEGA columns in `dfext`; is it a getExt() / getSamples() result?")
  }
  numTotEta <- (-1 + sqrt(1 + 8 * nOmCol)) / 2
  if (abs(numTotEta - round(numTotEta)) > 1e-8) {
    stop("The number of OMEGA columns in `dfext` (", nOmCol,
         ") is not a triangular number; the .ext looks inconsistent.")
  }
  numTotEta <- as.integer(round(numTotEta))

  ## ---- final $OMEGA BLOCK(N) from the model ----------------------------
  omBlock  <- findrecord(modFile, record = "\\$OMEGA", quiet = TRUE)
  blkLines <- grep("BLOCK\\s*\\(\\s*[0-9]+\\s*\\)", omBlock, value = TRUE, ignore.case = TRUE)
  if (length(blkLines) == 0) {
    stop("Could not find a '$OMEGA BLOCK(N)' record in ", basename(modFile),
         ". Auto-derivation needs the FREM omega block written as an explicit BLOCK(N).")
  }
  blockN <- as.integer(sub(".*BLOCK\\s*\\(\\s*([0-9]+)\\s*\\).*", "\\1",
                           blkLines[length(blkLines)], ignore.case = TRUE))

  ## ---- derive ---------------------------------------------------------
  d_numNonFREMThetas <- nTheta - numFREMThetas
  d_numSkipOm        <- numTotEta - blockN
  d_numParCov        <- blockN - numFREMThetas

  if (d_numNonFREMThetas < 0 || d_numParCov < 1 || d_numSkipOm < 0) {
    warning("Derived FREM structure looks inconsistent (numNonFREMThetas = ",
            d_numNonFREMThetas, ", numParCov = ", d_numParCov,
            ", numSkipOm = ", d_numSkipOm,
            "). Check that `modFile` and `dfext` are from the same run.")
  }

  ## ---- reconcile with explicit overrides: warn, keep the explicit value
  out_numNonFREMThetas <- d_numNonFREMThetas
  if (!is.null(numNonFREMThetas)) {
    if (!isTRUE(all.equal(as.numeric(numNonFREMThetas), as.numeric(d_numNonFREMThetas)))) {
      warning("Supplied numNonFREMThetas (", numNonFREMThetas,
              ") differs from the value derived from the model (", d_numNonFREMThetas,
              "); using the supplied value.")
    }
    out_numNonFREMThetas <- numNonFREMThetas
  }

  out_numSkipOm <- d_numSkipOm
  if (!is.null(numSkipOm)) {
    if (!isTRUE(all.equal(as.numeric(numSkipOm), as.numeric(d_numSkipOm)))) {
      warning("Supplied numSkipOm (", numSkipOm,
              ") differs from the value derived from the model (", d_numSkipOm,
              "); using the supplied value.")
    }
    out_numSkipOm <- numSkipOm
  }

  ## Keep numSkipOm + numParCov + numFREMThetas == numTotEta even when the user
  ## forces an inconsistent numSkipOm, so downstream code stays self-consistent.
  out_numParCov <- numTotEta - out_numSkipOm - numFREMThetas

  list(
    numNonFREMThetas = out_numNonFREMThetas,
    numFREMThetas    = numFREMThetas,
    numParCov        = out_numParCov,
    numSkipOm        = out_numSkipOm,
    numSigmas        = nSigma,
    numTotEta        = numTotEta,
    numTotThetas     = nTheta,
    covNames         = cn$covNames,
    orgCovNames      = cn$orgCovNames,
    polyCatCovs      = cn$polyCatCovs
  )
}
