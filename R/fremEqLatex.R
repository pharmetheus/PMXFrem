#' LaTeX equations for the covariate effects of the FFEM
#'
#' Writes the covariate effects of the FFEM of a FREM model as LaTeX, one
#' equation per parameter, ready to be put in a report. Each equation is a sum
#' of terms, one per covariate: the coefficient times the covariate minus its
#' mean, for example
#' \preformatted{COV_CL = -0.00591 (WT - 86.9) - 0.0287 (HT - 170) + ...}
#'
#' **The covariate effects are on the scale of the ETA.** `COV_CL` is the
#' expression that [calcFFEM()] gives in `Expr` and writes to `eqFile`: in a
#' model with `CL = TVCL * EXP(ETA(3))`, the FFEM is
#' `CL = TVCL * EXP(COV_CL + ETA(3))`.
#'
#' The coefficients are computed by [calcFFEM()] for the covariates in
#' `availCov`. The mean of each covariate is the FREM estimate of that mean, the
#' THETA of the covariate in the FREM model, as in [calcFFEM()].
#'
#' The covariates are labelled with `name_map` and `level_map`, the same
#' objects as in [generateCovNames()]. A FREM covariate named with an
#' underscore and a whole number, such as `RACEL_2`, is the indicator of one
#' level of a categorical covariate, as PsN names it. It is shown as the label
#' of the covariate with the label of the level as a subscript: with
#' `name_map = c(RACEL = "Race")` and
#' `level_map = list(RACEL = c("2" = "Black"))`, `RACEL_2` is shown as
#' "Race" with the subscript "Black". A level without a label, or with `NA`, is
#' shown by its code. An entry of `name_map` for the whole name, such as
#' `RACEL_2`, takes precedence. Characters with a special meaning in LaTeX,
#' such as `_`, `&` and `%`, are escaped in every label.
#'
#' Each equation is an `aligned` environment, from the amsmath LaTeX package,
#' in display math (`\[ ... \]`). In a knitr chunk with `results = "asis"`, the
#' printed equations go straight into the document. In R Markdown, each
#' equation is then a block of display math, which MathJax renders.
#'
#' @inheritParams getFileNames
#' @param dfext A data frame of the ext file of the FREM model, as from
#'   [getExt()]. If `NULL` (default), the ext file of the model found with
#'   `runno` or `modName` (and `modDevDir`) is read.
#' @param numNonFREMThetas Number of structural (non-FREM-covariate) THETAs in
#'   the FREM model. If `NULL` (default), it is derived from the model file
#'   found with `runno` or `modName` (and `modDevDir`) and the ext, with
#'   [fremModelInfo()]; a supplied value that disagrees with the derived one
#'   gives a warning and is kept.
#' @param numSkipOm Number of diagonal OMEGAs before the FREM block that are not
#'   part of the FREM calculations. Derived as `numNonFREMThetas` is when
#'   `NULL` (default); without a model, 0.
#' @param covNames Names of the FREM covariates, in the order of the model, as
#'   in [calcFFEM()]. If `NULL` (default), they are read from the model file
#'   with [getCovNames()]. When the model file is found, a `covNames` that
#'   differs from the covariates of the model in content or order stops the
#'   function, because the coefficients are attached to the names by position.
#' @param parNames Names of the parameters with covariate effects, as in
#'   [calcFFEM()], one per row of the coefficients. They are used in the
#'   left-hand side of each equation (`COV_CL`). If `NULL` (default), `Par1`,
#'   `Par2`, and so on.
#' @param availCov Names of the covariates in the FFEM: all of `covNames` (the
#'   default) or a subset, as in [calcFFEM()]. The terms follow the order of
#'   `covNames`. With no covariates, each equation is `COV = 0`.
#' @param name_map A named character vector of labels for the covariates, such
#'   as `c(WT = "Body weight", RACEL = "Race")`, as in [generateCovNames()]. A
#'   covariate without a label is shown by its name.
#' @param level_map A named list with one entry per categorical covariate: a
#'   named vector of labels for the levels, such as
#'   `list(SEX = c("1" = "Male", "2" = "Female"))`, as in
#'   [generateCovNames()].
#' @param perLine Number of terms on each line of an equation.
#' @param digits Number of significant digits of the coefficients and the
#'   means.
#' @param quiet If `FALSE` (default), the equations are printed, separated by
#'   blank lines. If `TRUE`, nothing is printed.
#'
#' @return The equations, one character string per parameter, named by
#'   `parNames`, invisibly.
#' @export
#'
#' @examples
#' modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
#'
#' ## One equation per parameter, with the names of the covariates
#' fremEqLatex(runno = 31, modDevDir = modDevDir, parNames = c("CL", "V", "MAT"))
#'
#' ## Labels for the covariates and the levels, and two covariates only
#' fremEqLatex(
#'   runno = 31, modDevDir = modDevDir, parNames = c("CL", "V", "MAT"),
#'   availCov = c("WT", "RACEL_2"),
#'   name_map = c(WT = "Weight", RACEL = "Race"),
#'   level_map = list(RACEL = c("2" = "Black"))
#' )
#'
#' @family FFEM Conversion
#' @concept ffem_conversion
fremEqLatex <- function(dfext = NULL,
                        numNonFREMThetas = NULL,
                        numSkipOm = NULL,
                        covNames = NULL,
                        parNames = NULL,
                        availCov = covNames,
                        runno = NULL,
                        modName = NULL,
                        modDevDir = NULL,
                        name_map = NULL,
                        level_map = NULL,
                        perLine = 3,
                        digits = 3,
                        quiet = FALSE) {
  ## Locate the FREM model as getForestDFFREM() and fremParameterTable() do,
  ## and derive what was not given from it.
  if (!is.null(runno) || !is.null(modName)) {
    fileNames <- getFileNames(
      runno = runno, modName = modName,
      modDevDir = if (is.null(modDevDir)) "." else modDevDir
    )
    if (is.null(dfext)) dfext <- getExt(extFile = fileNames$ext)
    info <- fremModelInfo(
      modFile = fileNames$mod, dfext = dfext,
      numNonFREMThetas = numNonFREMThetas, numSkipOm = numSkipOm
    )
    if (is.null(covNames)) covNames <- info$covNames
    .fremCheckCovNamesModel(covNames, info$covNames, fileNames$mod)
    numNonFREMThetas <- info$numNonFREMThetas
    numSkipOm <- info$numSkipOm
  }
  hint <- "(or supply `runno` / `modName` (+ `modDevDir`) so that it can be derived)."
  if (is.null(dfext)) stop("`dfext` is required ", hint, call. = FALSE)
  if (is.null(numNonFREMThetas)) stop("`numNonFREMThetas` is required ", hint, call. = FALSE)
  if (is.null(covNames)) stop("`covNames` is required ", hint, call. = FALSE)
  if (is.null(numSkipOm)) numSkipOm <- 0
  if (!.fremIsCount(perLine)) stop("`perLine` must be a whole number of at least 1.", call. = FALSE)
  if (!.fremIsCount(digits)) stop("`digits` must be a whole number of at least 1.", call. = FALSE)
  unknown <- setdiff(availCov, covNames)
  if (length(unknown) > 0) {
    stop("`availCov` has names that are not FREM covariates: ",
      paste(unknown, collapse = ", "), ".",
      call. = FALSE
    )
  }

  ffem <- calcFFEM(dfext,
    numNonFREMThetas = numNonFREMThetas, numSkipOm = numSkipOm,
    parNames = parNames, covNames = covNames, availCov = availCov, quiet = TRUE
  )
  coef <- as.matrix(ffem$Coefficients)
  if (is.null(parNames)) parNames <- paste0("Par", seq_len(nrow(coef)))

  ## The covariate means are the FREM covariate thetas, taken from the same
  ## columns as in calcFFEM(): the first column of dfext is ITERATION.
  final <- if (nrow(dfext) > 1) dfext[dfext$ITERATION == -1000000000, ] else dfext
  means <- as.numeric(final[, numNonFREMThetas + 1 + seq_along(covNames)])
  keep <- covNames %in% availCov
  covs <- covNames[keep]
  means <- means[keep]

  num <- function(x) trimws(formatC(signif(abs(x), digits), format = "fg", digits = digits))
  symbols <- vapply(covs, .fremEqSymbol, character(1),
    name_map = name_map, level_map = level_map, USE.NAMES = FALSE
  )

  eqs <- vapply(seq_along(parNames), function(p) {
    if (length(covs) == 0) {
      body <- "0"
    } else {
      terms <- paste0(
        ifelse(coef[p, ] < 0, "- ", "+ "), num(coef[p, ]), "\\,(", symbols,
        ifelse(means < 0, " + ", " - "), num(means), ")"
      )
      terms[1] <- sub("^\\+ ", "", sub("^- ", "-", terms[1]))
      lines <- split(terms, ceiling(seq_along(terms) / perLine))
      body <- paste(vapply(lines, paste, character(1), collapse = " "),
        collapse = " \\\\\n  & \\quad "
      )
    }
    ## \[ ... \] rather than equation*: pandoc reads consecutive equation*
    ## environments as one block, which MathJax cannot render, but makes a
    ## separate block of each \[ ... \].
    paste0(
      "\\[\n\\begin{aligned}\n",
      "\\text{COV}_{\\text{", .fremTexEscape(parNames[p]), "}} &= ", body,
      "\n\\end{aligned}\n\\]"
    )
  }, character(1))
  names(eqs) <- parNames

  if (!quiet) cat(paste(eqs, collapse = "\n\n"), "\n", sep = "")
  invisible(eqs)
}

#' The LaTeX symbol of one FREM covariate
#'
#' @param cov The name of the FREM covariate.
#' @inheritParams fremEqLatex
#' @return A LaTeX string: the label in `\text{}`, with the level as a subscript
#'   for the indicator of a level of a categorical covariate.
#' @keywords internal
#' @noRd
.fremEqSymbol <- function(cov, name_map, level_map) {
  label <- function(map, key) {
    if (is.null(map) || !key %in% names(map)) {
      return(NULL)
    }
    value <- map[[key]]
    if (is.na(value) || !nzchar(value)) NULL else as.character(value)
  }
  text <- function(x) paste0("\\text{", .fremTexEscape(x), "}")

  whole <- label(name_map, cov)
  if (!is.null(whole)) {
    return(text(whole))
  }
  if (grepl("_[0-9]+$", cov)) {
    base <- sub("_[0-9]+$", "", cov)
    level <- sub("^.*_", "", cov)
    name <- label(name_map, base)
    levelLabel <- label(level_map[[base]], level)
    return(paste0(
      text(if (is.null(name)) base else name), "_{",
      text(if (is.null(levelLabel)) level else levelLabel), "}"
    ))
  }
  text(cov)
}

#' Escape the characters LaTeX reserves, for use in text mode
#'
#' @param x A character vector.
#' @return `x`, with `& % $ # _ { }` preceded by a backslash and `~`, `^` and
#'   the backslash written as text commands.
#' @keywords internal
#' @noRd
.fremTexEscape <- function(x) {
  x <- gsub("\\\\", "\001", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  gsub("\001", "\\\\textbackslash{}", x)
}

#' A single whole number of at least 1
#'
#' @param x The value to check.
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
.fremIsCount <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 1 && x == round(x)
}
