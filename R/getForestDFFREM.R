#' Compute statistics for a FREM model forest plot
#'
#' Create a data frame with the input to the `forestPlot` function in the
#' `PMXForest` package. Statistics for covariate combination in the input
#' data frame will be computed.
#'
#' @details
#'   **Covariate names are checked against the FREM model and a generated
#'   parameter function.** A function made by [createFREMParamFunction()]
#'   carries the names of the structural covariates it reads, as the attribute
#'   `pmxCovariates`. `getForestDFFREM()` compares the columns of `dfCovs`,
#'   after the encoding set by `oneHot` and without `COVARIATEGROUPS`, with
#'   those names and with the FREM covariates in `covNames`. It warns when two
#'   differences occur together: a column that is neither a FREM covariate nor
#'   read by a function in `functionList`, so that the rows on which only that
#'   column is set have no effect, and a covariate of the model that `dfCovs`
#'   lacks. A name spelled differently from the model always gives that pair,
#'   for example `RACEL2` in `dfCovs` for the FREM covariate `RACEL_2` (the
#'   separator, `sep` in [PMXForest::setupDfCovs()] or `oneHotSep`), or a
#'   `RACEL_1` column for `RACEL_3` (the reference level, `catRef` in
#'   [PMXForest::setupDfCovs()]). Either difference alone gives no warning.
#'   Most Forest plots leave some FREM covariates out of `dfCovs`, so in
#'   practice a column that nothing reads is reported whenever it occurs. A
#'   hand-written function carries no names, so no check is made when
#'   `functionList` holds one. The warning is given whatever the value of
#'   `quiet`. It has the class `pmxCovariateMismatch`, so an intended pair can
#'   be silenced alone:
#'   `withCallingHandlers(getForestDFFREM(...), pmxCovariateMismatch = function(w) invokeRestart("muffleWarning"))`.
#'
#'   **The columns of `dfRefRow` are checked against `dfCovs`.** A column of
#'   `dfRefRow` that `dfCovs` lacks, after the encoding set by `oneHot`, gives
#'   a warning of the same class, with the names in the field `refOnly`. With
#'   a name spelled differently, such as `wt` in `dfRefRow` for `WT` in
#'   `dfCovs`, the reference subject is not conditioned on that covariate, and
#'   every row is shifted. This check is made for hand-written functions too.
#'   A reference row from [PMXForest::setupDfRefRow()] has the columns of
#'   `dfCovs` and is never reported.
#'
#' @import doParallel
#' @import foreach
#' @import dplyr
#' @import stringr
#'
#'
#' @param dfCovs A data frame with covariates to include, if a covariate value
#'   is set -99 or NA, they are assumed missing and will not be included in any
#'   FFEM transformations. If a dfCovs is a list an attempt will be made to
#'   create the appropriate data frame with the createInputForestData function.
#' @param cdfCovsNames A string vector with names of the rows in dfCovs, if not
#'   used, names will be automatically assigned based on the covariate values
#'   and column names in dfCovs.
#' @param functionList A list of functions with input (basethetas,
#'   covthetas,dfrow and ...) from which the change from the reference value
#'   will be calculated. If the function returns a vector of values, each value
#'   will be used but functionListName must contain the names with a length of
#'   all return for all functions in the functionList
#' @param functionListName A vector of strings (names) of the parameters for
#'   each function in the functionList
#' @param dfParameters A data frame with parameter samples from the uncertainty
#'   distribution. The vector of final parameter estimates is assumed to be in
#'   the first row. The column order is assumed the same as in the NONMEM ext
#'   file except the ITERATION and OBJ columns whichshould not be included.
#' @param quiet If `FALSE` (default), the FFEM model of each calculation is
#'   printed by [calcFFEM()], and with `ncores > 1` the progress of the
#'   parallel loop is shown. If `TRUE`, nothing is printed. Mainly for
#'   debugging. The warning about covariate names described in Details is given
#'   whatever the value.
#' @param probs A vector of probabilities that should be computed for each of
#'   the parameters from functionList. These will be used as the as the
#'   uncertainties in the Forest plots. The probs vector position one and two
#'   will be used for plotting the uncertanties (i.e. columns q1 and q2).
#'   Default is c(0.05, 0.95).
#' @param pointFunction The function used to calculate the point for each
#'   covariate in the forest plot. default=median This function is also used for
#'   the reference covariate combination
#' @param dfRefRow A data frame  (one row or equal number of rows as dfCovs)
#'   with the covariate values that will be used as the reference, if NULL the
#'   typical subject is used as reference.
#' @param cGrouping A vector of numbers defining how to group the y-axis of the
#'   Forest plot, the length of the vector should match the number of rows in
#'   dfCovs. If NULL (default) an educated guess of the grouping will be set
#' @param ncores the number of cores to use for the calculations, default = 1
#'   which means no parallellization.
#'   On Windows the workers are new R sessions. They load the same package
#'   versions as this session, including a package loaded from a folder that
#'   is not on `.libPaths()`, and the run stops if they cannot.
#' @param cstrPackages a character vector with package names needed to run the
#'   calculations in parallel, default = NULL
#' @param cstrExports a character vector with variables needed to run the
#'   calculations in parallel, default = NULL
#' @param missVal The missing value number. -99 by default.
#' @param ... additional variables to be forwarded to the the functionList
#'   functions
#' @inheritParams getFileNames
#' @param numNonFREMThetas Number of thetas that are not FREM covariates. These
#'   need to come before the FREM covariate thetas. If `NULL` (default) and the
#'   model file is located via `runno` / `modName` / `modDevDir`, it is derived
#'   via [fremModelInfo()]; an explicit value that disagrees with the derived one
#'   triggers a warning and is kept. Required when the model file is not given.
#' @param numSkipOm Number of diag omegas (variances) that should not be part of
#'   the FREM calculations. Such omegas has to come before the large FREM omega
#'   block. If `NULL` (default) and the model file is located via `runno` /
#'   `modName` / `modDevDir`, it is derived via [fremModelInfo()] (warn-and-keep
#'   on a disagreeing explicit value); otherwise it defaults to 0.
#' @param parNames Names of the parameters
#' @param covNames Names of the FREM covariates, in the order of the model. If
#'   `NULL` (default), they are read from the model file found with `runno` /
#'   `modName` / `modDevDir`; otherwise required. The FFEM coefficients are
#'   attached to the names by position. When the model file is found, a
#'   `covNames` that differs from the covariates of the model in content or
#'   order stops the function. A copy of the column names of `dfCovs` is not
#'   the same: the indicator columns follow the levels of the data and
#'   `catRef`, not the order of the model. Without the model file, nothing is
#'   compared.
#' @param availCov Names of the covariates to use in the calculation of the FFEM
#'   model.
#' @param oneHot An optional one-hot encoding specification (see
#'   [PMXForest::oneHotEncode()]). When supplied, raw multi-level categorical
#'   columns in `dfCovs` (and in `dfRefRow` when it is a data frame) are one-hot
#'   encoded before the FFEM expressions are evaluated, so `dfCovs` can be built
#'   with a raw covariate column instead of the FREM `<cov>_<level>` dummy
#'   columns. The raw column is dropped after encoding. Default `NULL` (no
#'   encoding).
#' @param oneHotSep The separator between covariate name and level in the dummy
#'   column names created when `oneHot` is used. Defaults to `"_"`, matching the
#'   FREM convention.
#'
#'
#' @return A data frame with summary statistics for each parameters and
#'   covariate combinations:
#'
#' @export
#'
#' @examples
#' \donttest{
#' # 1. Setup paths to model and bootstrap results
#' modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
#' modFile <- file.path(modDevDir, "run31.mod")
#' extFile <- file.path(modDevDir, "run31.ext")
#' bsFile <- file.path(modDevDir, "bs31.dir/raw_results_run31.csv")
#'
#' # 2. Setup Covariates
#' dfCovs <- data.frame(
#'   COVARIATEGROUPS = c("AGE", "AGE", "SEX", "SEX"),
#'   AGE = c(30, 60, -99, -99),
#'   SEX = c(-99, -99, 0, 1)
#' )
#'
#' # 3. Define a simple parameter function
#' paramFunction <- function(basethetas, covthetas, dfrow, ...) {
#'   return(basethetas[1] * exp(covthetas[1]))
#' }
#'
#' # 4. Use PMXForest to extract 25 samples from bootstrap results
#' set.seed(123)
#' samples <- PMXForest::getSamples(bsFile, extFile = extFile, n = 25)
#'
#' # 5a. Generate Forest Plot Data -- covNames / numSkipOm / numNonFREMThetas
#' #     passed explicitly
#' dfresFREM <- getForestDFFREM(
#'   dfCovs           = dfCovs,
#'   covNames         = getCovNames(modFile)$covNames,
#'   functionList     = list(paramFunction),
#'   functionListName = "CL",
#'   numSkipOm        = 2,
#'   numNonFREMThetas = 7,
#'   dfParameters     = samples,
#'   quiet            = TRUE
#' )
#'
#' # 5b. Same result -- covNames / numSkipOm / numNonFREMThetas omitted and
#' #     derived from the model file located via runno / modDevDir
#' #     (see fremModelInfo())
#' dfresFREM2 <- getForestDFFREM(
#'   dfCovs           = dfCovs,
#'   functionList     = list(paramFunction),
#'   functionListName = "CL",
#'   dfParameters     = samples,
#'   runno            = 31,
#'   modDevDir        = modDevDir,
#'   quiet            = TRUE
#' )
#'
#' print(head(dfresFREM))
#' }
#' @family Diagnostics & Plotting
#' @concept diagnostics
getForestDFFREM <- function(dfCovs,
                            cdfCovsNames = NULL,
                            functionList = list(function(basethetas, covthetas, dfrow, ...) {
                              return(basethetas[1] * exp(covthetas[1]))
                            }),
                            covNames = NULL,
                            functionListName = "PAR1",
                            numSkipOm = NULL,
                            numNonFREMThetas = NULL,
                            dfParameters,
                            runno = NULL,
                            modName = NULL,
                            modDevDir = NULL,
                            parNames = paste("Par", 1:numParCov, sep = ""),
                            availCov = covNames,
                            quiet = FALSE,
                            probs = c(0.05, 0.95),
                            pointFunction = median,
                            dfRefRow = NULL,
                            cGrouping = NULL,
                            ncores = 1,
                            cstrPackages = NULL,
                            cstrExports = NULL,
                            missVal = -99,
                            oneHot = NULL,
                            oneHotSep = "_",
                            ...) {
  if (is.list(covNames)) stop("covNames should no longer be a list. Perhaps you want to use getCovNames(modFile)$covNames?")

  if (!is.null(dfRefRow) && nrow(dfRefRow) != 1 && nrow(dfRefRow) != nrow(dfCovs)) {
    stop("The number of reference rows (dfRefRow) should be either NULL (missing used as reference), one (this row used as reference) or equal to dfCovs (change reference for each covariate combination)")
  }
  ## Try to make dfCovs into a data.frame if it isn't that already
  if (!is.data.frame(dfCovs)) {
    dfCovs <- PMXForest::createInputForestData(dfCovs)
  }
  ## Coerce to a plain data.frame: the row/column subsetting below assumes
  ## base-R `[` drop-to-vector behaviour, which a tibble does not provide.
  dfCovs <- as.data.frame(dfCovs)
  if (!is.null(dfRefRow) && is.data.frame(dfRefRow)) dfRefRow <- as.data.frame(dfRefRow)
  ## Replace potential NAs in dfCovs with the missing value token
  dfCovs[is.na(dfCovs)] <- missVal

  ## Locate the FREM model file the same way as getExplainedVar() -- from
  ## runno / modName / modDevDir via getFileNames() -- and, when it is given,
  ## derive covNames / numNonFREMThetas / numSkipOm from it (and the ext-shaped
  ## dfParameters) via fremModelInfo(), validating any explicit values (warn,
  ## keep the explicit value). Without it, covNames and numNonFREMThetas are
  ## required and numSkipOm keeps its historical default of 0.
  if (!is.null(runno) || !is.null(modName)) {
    modFile <- getFileNames(
      runno = runno,
      modName = modName,
      modDevDir = if (is.null(modDevDir)) "." else modDevDir
    )$mod
    .info <- fremModelInfo(
      modFile = modFile, dfext = dfParameters,
      numNonFREMThetas = numNonFREMThetas,
      numSkipOm = numSkipOm
    )
    if (is.null(covNames)) covNames <- .info$covNames
    .fremCheckCovNamesModel(covNames, .info$covNames, modFile)
    if (is.null(numNonFREMThetas)) numNonFREMThetas <- .info$numNonFREMThetas
    if (is.null(numSkipOm)) numSkipOm <- .info$numSkipOm
  }
  if (is.null(covNames)) {
    stop("`covNames` is required (or supply `runno` / `modName` (+ `modDevDir`) so it can be derived).")
  }
  if (is.null(numNonFREMThetas)) {
    stop("`numNonFREMThetas` is required (or supply `runno` / `modName` (+ `modDevDir`) so it can be derived).")
  }
  if (is.null(numSkipOm)) numSkipOm <- 0

  ## Optionally one-hot encode raw multi-level categorical columns so dfCovs can
  ## be built without the FREM <cov>_<level> dummy columns.
  if (!is.null(oneHot)) {
    ## The levels are resolved against dfCovs and reused for dfRefRow, as in
    ## PMXForest. Resolved from a one-row dfRefRow, a covariate had one level,
    ## got no indicator columns and kept the raw column, so the reference
    ## subject was not conditioned on it.
    oneHot <- .fremResolveOneHot(oneHot, dfCovs, missVal)
    dfCovs <- PMXForest::oneHotEncode(dfCovs,
      spec = oneHot, sep = oneHotSep,
      missVal = missVal, dropOriginal = TRUE
    )
    if (!is.null(dfRefRow) && is.data.frame(dfRefRow)) {
      ## A dfRefRow that already has the indicator columns and not the raw one
      ## is left as it is, apart from a warning.
      dfRefRow <- suppressWarnings(
        PMXForest::oneHotEncode(dfRefRow,
          spec = oneHot, sep = oneHotSep,
          missVal = missVal, dropOriginal = TRUE
        )
      )
    }
  }

  ## After the one-hot encoding, so the dummy columns are compared and not the
  ## raw covariate, and before the FREM covariates that dfCovs lacks are added
  ## as missing columns below.
  .fremCheckCovariateNames(
    functionList, covNames, setdiff(names(dfCovs), "COVARIATEGROUPS"),
    hint = paste0(
      "Check the separator of indicator columns (sep in setupDfCovs(), oneHotSep ",
      "in getForestDFFREM()), which gives RACEL2 against RACEL_2, and the ",
      "reference level (catRef in setupDfCovs()), which gives RACEL_1 against ",
      "RACEL_3."
    )
  )
  ## Before the FREM covariates that dfCovs lacks are added below, so that a
  ## FREM covariate set only in dfRefRow is reported.
  if (!is.null(dfRefRow) && is.data.frame(dfRefRow)) {
    .fremCheckRefRowNames(names(dfRefRow), names(dfCovs))
  }

  ## A FREM covariate that dfCovs lacks is set to missing on every row, so
  ## that the FFEM conditions only on the covariates given. Leaving some out
  ## is the usual case and gives no message; a misspelled name is reported by
  ## the check above.
  if (!all(covNames %in% names(dfCovs))) {
    dfCovs[, covNames[!(covNames %in% names(dfCovs))]] <- missVal
  }


  groupnames <- NULL # Store the temp groupnames
  if (any(names(dfCovs) == "COVARIATEGROUPS")) {
    groupnames <- dfCovs[, "COVARIATEGROUPS"]
    dfCovs[, "COVARIATEGROUPS"] <- NULL
  }

  ## PMXForest::setupDfRefRow() returns the same COVARIATEGROUPS column
  ## setupDfCovs() does. Left in place it reaches the as.numeric() calls below
  ## as a character column, and every row warns "NAs introduced by coercion".
  if (!is.null(dfRefRow) && any(names(dfRefRow) == "COVARIATEGROUPS")) {
    dfRefRow[, "COVARIATEGROUPS"] <- NULL
  }

  ## Create function to sort out grouping
  getGroups <- function(df) {
    cGroups <- c()
    cUnique <- c()
    iGroup <- 0
    for (i in seq_len(nrow(df))) {
      tmp <- paste0(names(dfCovs[i, , drop = FALSE])[as.numeric(dfCovs[i, , drop = FALSE]) != missVal], collapse = ",")
      if (tmp %in% cUnique) {
        tmpl <- which(tmp == cUnique)
        cGroups <- c(cGroups, tmpl)
      } else {
        iGroup <- iGroup + 1
        cGroups <- c(cGroups, iGroup)
        cUnique <- c(cUnique, tmp)
      }
    }
    return(cGroups)
  }

  if (is.null(cGrouping)) {
    cGrouping <- getGroups(dfCovs)
  }


  ## Register to allow for paralell computing. Tear the cluster down on exit
  ## (including on error), not only at the end of a successful run.
  if (ncores > 1) {
    stopWorkers <- .fremStartWorkers(ncores, pkgs = unique(c("PMXFrem", cstrPackages)))
    on.exit(stopWorkers(), add = TRUE)
  }
  numParCov <- calcNumParCov(cbind(first = 0, dfParameters), numNonFREMThetas, numSkipOm)

  ## Calculate the parameters. Result rows per parameter vector k: one per
  ## (dfCovs row) x (element returned by the functionList). Fill typed column
  ## vectors and build the data.frame once - a per-cell data.frame() +
  ## bind_rows() loop is markedly slower for closed-form parameter functions.
  nOut <- length(functionListName)
  internalCalc <- function(k) {
    dfext <- cbind(first = 0, dfParameters[k, ])
    thetas <- as.numeric(dfext[2:(numNonFREMThetas + 1)])
    nRow <- nrow(dfCovs) * nOut
    ITER <- integer(nRow)
    COVS <- integer(nRow)
    NAME <- character(nRow)
    VALUE <- numeric(nRow)
    VALUEBASE <- numeric(nRow)
    p <- 0L

    for (i in seq_len(nrow(dfCovs))) {
      ## Calculate the ffemObj in case reference covariates have been specified
      if (!is.null(dfRefRow)) {
        indi <- min(i, nrow(dfRefRow))

        ffemObjRef <- calcFFEM(
          numSkipOm        = numSkipOm,
          numNonFREMThetas = numNonFREMThetas,
          dfext            = dfext,
          covNames         = covNames,
          availCov         = names(dfRefRow[indi, , drop = FALSE])[as.numeric(dfRefRow[indi, , drop = FALSE]) != missVal][names(dfRefRow[indi, , drop = FALSE])[as.numeric(dfRefRow[indi, , drop = FALSE]) != missVal] %in% covNames],
          quiet            = quiet
        )
      }

      ## Calculate the ffemObj for each set of parameters
      ffemObj <- calcFFEM(
        numSkipOm = numSkipOm,
        numNonFREMThetas = numNonFREMThetas,
        dfext = dfext,
        covNames = covNames,
        availCov = names(dfCovs[i, , drop = FALSE])[as.numeric(dfCovs[i, , drop = FALSE]) != missVal][names(dfCovs[i, , drop = FALSE])[as.numeric(dfCovs[i, , drop = FALSE]) != missVal] %in% covNames],
        quiet = quiet
      )


      coveffects <- rep(0, length(parNames))
      coveffects_base <- rep(0, length(parNames))
      data47_jxrtp <- dfCovs[i, , drop = FALSE]

      ## Compute the ffem expressions
      for (j in seq_along(parNames)) {
        ffem_expr <- str_replace_all(ffemObj$Expr[j], pattern = "data\\$", replacement = "data47_jxrtp$")

        if (length(names(dfCovs[i, , drop = FALSE])[as.numeric(dfCovs[i, , drop = FALSE]) != missVal]) != 0) {
          coveffects[j] <- as.numeric(eval(parse(text = ffem_expr)))
        }

        if (!is.null(dfRefRow)) {
          data47_jxrtp_ref <- dfRefRow[indi, , drop = FALSE]
          ffem_expr_base <- str_replace_all(ffemObjRef$Expr[j], pattern = "data\\$", replacement = "data47_jxrtp_ref$")
          coveffects_base[j] <- as.numeric(eval(parse(text = ffem_expr_base)))
        }
      }

      ## Send the expresssions to the functions in functionList
      n <- 1
      for (j in seq_along(functionList)) {
        val <- functionList[[j]](thetas, coveffects, dfrow = dfCovs[i, , drop = FALSE], ...)

        ## Do it for the reference
        if (!is.null(dfRefRow)) {
          valbase <- functionList[[j]](basethetas = thetas, covthetas = coveffects_base, dfrow = dfRefRow[indi, , drop = FALSE], ...)
        } else {
          dfmissing <- dfCovs[1, , drop = FALSE]
          dfmissing[, ] <- missVal
          valbase <- functionList[[j]](basethetas = thetas, covthetas = rep(0, length(parNames)), dfrow = dfmissing, ...)
        }

        ## Do it for the parameters
        for (l in seq_along(val)) {
          p <- p + 1L
          ITER[p] <- k
          COVS[p] <- i
          NAME[p] <- functionListName[n]
          VALUE[p] <- val[[l]]
          VALUEBASE[p] <- valbase[[l]]
          n <- n + 1L
        }
      }
    }

    if (p != nRow) {
      idx <- seq_len(p)
      ITER <- ITER[idx]
      COVS <- COVS[idx]
      NAME <- NAME[idx]
      VALUE <- VALUE[idx]
      VALUEBASE <- VALUEBASE[idx]
    }
    data.frame(
      ITER = ITER, COVS = COVS, NAME = NAME, VALUE = VALUE,
      VALUEBASE = VALUEBASE, stringsAsFactors = FALSE
    )
  }

  if (ncores > 1) {
    parts <- foreach(
      k = seq_len(nrow(dfParameters)), .packages = cstrPackages,
      ## Bundle the local environment for PSOCK workers (Windows); foreach's
      ## static global detection does not reliably follow `internalCalc`'s free
      ## variables. `cstrExports` covers anything outside this frame.
      .export = c(ls(environment()), cstrExports),
      .verbose = !quiet
    ) %dopar% {
      internalCalc(k)
    }
  } else {
    parts <- vector("list", nrow(dfParameters))
    for (k in seq_len(nrow(dfParameters))) parts[[k]] <- internalCalc(k)
  }
  dfres <- bind_rows(parts)


  ## Assemble the return data.frame
  getCovNameString <- function(dfrow) {
    strName <- ""
    colnames <- names(dfrow)

    for (i in seq_len(ncol(dfrow))) {
      if (dfrow[1, i] != missVal) {
        if (strName == "") {
          strName <- paste0(colnames[i], "=", dfrow[1, i])
        } else {
          strName <- paste0(strName, ", ", colnames[i], "=", dfrow[1, i])
        }
      }
    }

    if (strName == "") {
      strName <- "Ref cov"
    }
    return(strName)
  }

  dfret <- data.frame()
  for (i in seq_len(nrow(dfCovs))) {
    if (is.null(cdfCovsNames)) {
      covname <- getCovNameString(dfCovs[i, , drop = FALSE])
    } else {
      covname <- cdfCovsNames[i]
    }

    group <- cGrouping[i]
    groupname <- group
    if (!is.null(groupnames)) groupname <- groupnames[i]

    for (j in seq_along(functionListName)) {
      dft <- dfres[dfres$COVS == i & dfres$NAME == functionListName[j], ]
      quant <- quantile(dft$VALUE, probs = probs, names = FALSE, na.rm = TRUE)
      # Calculate the point value of the forest plot
      FUNCVAL <- pointFunction(dft$VALUE)

      # Define the relative without parameter uncertainty
      dft$RELINTERNAL <- dft$VALUE / dft$VALUEBASE
      # Get quantile and pointvalue when uncertainty is not taken into account
      quantrel <- quantile(dft$RELINTERNAL, probs = probs, names = FALSE, na.rm = TRUE)
      FUNCNOVAR <- pointFunction(dft$RELINTERNAL)

      # Calculate reference value based one the pointFunction
      func_base <- pointFunction(dft$VALUEBASE)
      true_base <- dft$VALUEBASE[dft$ITER == 1]

      # Quantiles of the value relative to the reference. Computed on the ratio
      # directly (rather than dividing `quant` by the scalar base) so the endpoints
      # stay ordered as lower/upper even when the reference value is negative.
      # Fall back to quant/base when the base is NA so an all-NA reference yields
      # NA columns rather than erroring in quantile().
      quant_reffunc <- if (is.na(func_base)) quant / func_base else quantile(dft$VALUE / func_base, probs = probs, names = FALSE, na.rm = TRUE)
      quant_reffinal <- if (is.na(true_base)) quant / true_base else quantile(dft$VALUE / true_base, probs = probs, names = FALSE, na.rm = TRUE)

      dfrow <- cbind(
        dfCovs[i, , drop = FALSE],
        data.frame(
          GROUP = group,
          GROUPNAME = groupname,
          COVNUM = i, COVNAME = covname, PARAMETER = functionListName[j],
          REFFUNC = func_base, REFFINAL = true_base, POINT = FUNCVAL, POINT_NOVAR_REL_REFFUNC = FUNCNOVAR,
          POINT_REL_REFFUNC = FUNCVAL / func_base, POINT_REL_REFFINAL = FUNCVAL / true_base,
          COVEFF = !all(dft$RELINTERNAL == 1)
        )
      )

      for (k in seq_along(probs)) {
        dfp <- data.frame(X1 = 1)
        dfp[[paste0("Q", k)]] <- quant[k]
        dfp[[paste0("Q", k, "_REL_REFFUNC")]] <- quant_reffunc[k]
        dfp[[paste0("Q", k, "_REL_REFFINAL")]] <- quant_reffinal[k]
        dfrow <- cbind(dfrow, dfp[, 2:4])
      }
      for (k in seq_along(probs)) {
        dfp <- data.frame(X1 = 1)
        dfp[[paste0("Q", k, "_NOVAR_REL_REFFUNC")]] <- quantrel[k]
        dfrow <- cbind(dfrow, dfp[, 2])
        names(dfrow)[ncol(dfrow)] <- paste0("Q", k, "_NOVAR_REL_REFFUNC")
      }
      dfret <- rbind(dfret, dfrow)
    }
  }

  ## (cluster teardown is handled by on.exit() registered above)

  ## Add a column with YES/NO depending on if refRow was provided or if it was set to the default NULL
  dfret <- dfret %>% mutate(REFROW = ifelse(is.null(dfRefRow), "NO", "YES"))

  ## Make sure GROUPNAME, COVNAME, PARAMETER are factors
  dfret$GROUPNAME <- factor(dfret$GROUPNAME, levels = unique(dfret$GROUPNAME))
  dfret$COVNAME <- factor(dfret$COVNAME, levels = unique(dfret$COVNAME))
  dfret$PARAMETER <- factor(dfret$PARAMETER, levels = unique(dfret$PARAMETER))

  return(dfret)
}

#' Check the covariate columns of dfCovs against the FREM model and the
#' parameter functions
#'
#' A function made by [createFREMParamFunction()] carries the structural
#' covariates it reads as the attribute `pmxCovariates`; the FREM covariates
#' are `covNames`. A name in `dfCovs` spelled differently from the model, such
#' as `RACEL2` for `RACEL_2`, gives two differences at once: a column that is
#' neither a FREM covariate nor read by a function, so its rows have no
#' effect, and a covariate of the model that `dfCovs` lacks. Only that pair is
#' reported, because either difference alone is often a deliberate choice of
#' rows. A FREM covariate left out of `dfCovs` is the usual case, so for a
#' FREM model the pair is close to "a column nothing reads". The check is made
#' only when every function carries the list: a hand-written function may
#' read any column as a structural covariate, and the FREM covariates alone
#' cannot tell such a column from a misspelled one. The rule is the one
#' PMXForest applies in getForestDFSCM() (epic/1.5.0). Used by
#' getForestDFFREM() and getExplainedVar().
#'
#' @param functionList The parameter functions.
#' @param covNames The FREM covariates, after one-hot encoding.
#' @param cols The column names of `dfCovs`, with every categorical covariate
#'   under its FREM names (`RACEL_2`, `RACEL_3`) and without
#'   `COVARIATEGROUPS`.
#' @param hint The last paragraph of the message: where a misspelled name
#'   usually comes from in the calling function.
#' @return `NULL`, invisibly. Warns with class `pmxCovariateMismatch`, whose
#'   fields `unread` and `absent` hold the names.
#' @keywords internal
#' @noRd
.fremCheckCovariateNames <- function(functionList, covNames, cols, hint) {
  reads <- lapply(functionList, attr, which = "pmxCovariates", exact = TRUE)
  ## A function without the list may read any column, so no column can be
  ## called unread, and the pair cannot be found.
  if (any(vapply(reads, is.null, logical(1)))) {
    return(invisible(NULL))
  }
  known <- unique(c(covNames, unlist(reads)))
  unread <- setdiff(cols, known)
  absent <- setdiff(known, cols)
  ## Either alone is an extra row or a deliberate subset; a misspelled name
  ## always gives both.
  if (length(unread) == 0 || length(absent) == 0) {
    return(invisible(NULL))
  }

  msg <- c(
    paste0(
      "The covariate columns of dfCovs and the covariates of the FREM model and ",
      "the parameter function differ in both directions, as they do when a name ",
      "in dfCovs is spelled differently from the model."
    ),
    paste0(
      "These columns of dfCovs are neither FREM covariates nor read by a ",
      "parameter function: ", paste(unread, collapse = ", "),
      ". Rows on which only these columns are set have no effect."
    ),
    paste0(
      "dfCovs has no column for these FREM covariates or covariates the ",
      "parameter function reads: ", paste(absent, collapse = ", "),
      ". A FREM covariate among them is treated as missing on every row, and ",
      "any other covariate stays at the reference value of the function."
    ),
    paste(hint, "If both lists are intended, the warning can be ignored.")
  )
  warning(structure(
    class = c("pmxCovariateMismatch", "warning", "condition"),
    list(
      message = paste(msg, collapse = "\n"), call = NULL,
      unread = unread, absent = absent
    )
  ))
  invisible(NULL)
}

#' Stop when covNames differs from the FREM covariates of the model
#'
#' [calcFFEM()] attaches the coefficient of the c-th FREM covariate to
#' `covNames[c]`, so a `covNames` in another order than the model, or with
#' other names, puts the effects on the wrong covariates without an error. A
#' `covNames` copied from the columns of a `dfCovs` is such a case: the
#' indicator columns follow the levels of the data, not the order of the model,
#' and follow `catRef`. The covariate-name check of the columns of `dfCovs`
#' cannot see it, because `dfCovs` and `covNames` then agree.
#'
#' @param covNames The `covNames` that will be used.
#' @param modelCovNames The FREM covariates of the model, from [getCovNames()].
#' @param modFile The model file, named in the message.
#' @return `NULL`, invisibly; stops when the two differ in content or order.
#' @keywords internal
#' @noRd
.fremCheckCovNamesModel <- function(covNames, modelCovNames, modFile) {
  if (identical(as.character(covNames), as.character(modelCovNames))) {
    return(invisible(NULL))
  }
  if (length(covNames) == length(modelCovNames)) {
    pos <- which(covNames != modelCovNames)
    detail <- paste0(
      "Position ", pos, ": ", covNames[pos], " in `covNames`, ",
      modelCovNames[pos], " in the model."
    )
  } else {
    detail <- paste0(
      "`covNames` has ", length(covNames), " names and the model ",
      length(modelCovNames), "."
    )
    notCov <- setdiff(modelCovNames, covNames)
    notMod <- setdiff(covNames, modelCovNames)
    if (length(notCov) > 0) detail <- c(detail, paste0("Not in `covNames`: ", paste(notCov, collapse = ", "), "."))
    if (length(notMod) > 0) detail <- c(detail, paste0("Not in the model: ", paste(notMod, collapse = ", "), "."))
    detail <- paste(detail, collapse = " ")
  }
  stop(
    paste(
      c(
        paste0(
          "`covNames` differs from the FREM covariates of the model ",
          basename(modFile), ". The FFEM coefficients are attached to `covNames` ",
          "by position, so the effects would be given to the wrong covariates."
        ),
        detail,
        paste0(
          "Leave `covNames` out to read the names from the model. A copy of the ",
          "column names of dfCovs is not the same: the indicator columns follow ",
          "the levels of the data and catRef, not the order of the model."
        )
      ),
      collapse = "\n"
    ),
    call. = FALSE
  )
}

#' Warn when dfRefRow has a column that dfCovs lacks
#'
#' A column of `dfRefRow` that `dfCovs` lacks is either a name spelled
#' differently from `dfCovs`, which leaves the reference subject unconditioned
#' on that covariate (or at the reference of the parameter function, for a
#' structural covariate), or a covariate set in the reference subject and in no
#' row. Either way every row of the Forest plot is shifted. The check needs no
#' list of the columns a function reads, so it is made for hand-written
#' functions too. A column of `dfCovs` that `dfRefRow` lacks is not reported.
#' The rule is the one PMXForest applies in getForestDFSCM() (epic/1.5.0).
#'
#' @param refNames The column names of `dfRefRow` after one-hot encoding.
#' @param covNames The column names of `dfCovs` after one-hot encoding, before
#'   the FREM covariates that `dfCovs` lacks are added.
#' @return `NULL`, invisibly. Warns with class `pmxCovariateMismatch`, whose
#'   field `refOnly` holds the names.
#' @keywords internal
#' @noRd
.fremCheckRefRowNames <- function(refNames, covNames) {
  ## PMXForest::setupDfRefRow() copies COVARIATEGROUPS from dfCovs.
  refOnly <- setdiff(refNames, c(covNames, "COVARIATEGROUPS"))
  if (length(refOnly) == 0) {
    return(invisible(NULL))
  }

  msg <- c(
    paste0("dfRefRow has columns that dfCovs lacks: ", paste(refOnly, collapse = ", "), "."),
    paste0(
      "With a name spelled differently from dfCovs, such as wt for WT, the reference ",
      "subject is not conditioned on that covariate, or is held at the reference of ",
      "the parameter function for a structural covariate, and every row of the ",
      "Forest plot is shifted."
    ),
    paste0(
      "A covariate set in dfRefRow and in no column of dfCovs has a value in the ",
      "reference subject and none in the rows, so every row is shifted."
    ),
    paste0(
      "PMXForest::setupDfRefRow() builds dfRefRow with the columns of dfCovs. If the ",
      "columns are intended, the warning can be ignored."
    )
  )
  warning(structure(
    class = c("pmxCovariateMismatch", "warning", "condition"),
    list(message = paste(msg, collapse = "\n"), call = NULL, refOnly = refOnly)
  ))
  invisible(NULL)
}

#' Resolve a one-hot specification against dfCovs
#'
#' Gives each covariate of `oneHot` that `dfCovs` has an explicit reference
#' level and an explicit list of the levels that get an indicator column, both
#' taken from `dfCovs`, in the form [PMXForest::oneHotEncode()] accepts. The same
#' specification then encodes `dfRefRow`, whose single row would otherwise give
#' one level and no indicator columns. A covariate with explicit `levels`, one
#' that `dfCovs` lacks, one with fewer than two levels in `dfCovs`, and one whose
#' reference level does not occur in `dfCovs` are left as they are, so that
#' `oneHotEncode()` gives the same warnings as before.
#'
#' @param oneHot The `oneHot` argument: a character vector or a named list.
#' @param dfCovs The covariate data frame, before encoding.
#' @param missVal The missing value.
#' @return The specification as a named list.
#' @keywords internal
#' @noRd
.fremResolveOneHot <- function(oneHot, dfCovs, missVal) {
  if (is.character(oneHot)) {
    oneHot <- stats::setNames(vector("list", length(oneHot)), oneHot)
  }
  for (cov in intersect(names(oneHot), names(dfCovs))) {
    el <- oneHot[[cov]]
    if (is.list(el) && !is.null(el$levels)) next
    obs <- dfCovs[[cov]]
    levs <- sort(unique(obs[!is.na(obs) & obs != missVal]))
    ref <- if (is.list(el)) el$ref else el
    if (is.null(ref)) ref <- levs[1]
    if (length(levs) < 2 || !ref %in% levs) next
    oneHot[[cov]] <- list(ref = ref, levels = setdiff(levs, ref))
  }
  oneHot
}
