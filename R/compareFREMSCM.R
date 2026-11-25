#' Compare the Forest plots of an SCM model and a FREM model of the same data
#'
#' @description Draws the Forest plot of a model built by stepwise covariate
#'   modelling (SCM) next to the Forest plot of a full random effects model
#'   (FREM) of the same data, set up so that the two plots can be compared.
#'   One plot is drawn per parameter, with the covariates that the SCM model
#'   has on that parameter. The function also returns a table of the effects
#'   of both models, a text that describes the differences, and a figure
#'   legend per plot. The text and the legends are kept in the returned object
#'   and are printed only on request.
#'
#' @details A FREM Forest plot can be compared with an SCM Forest plot only
#'   when two conditions hold together:
#'
#'   * **The FREM predictions on a parameter cover the covariates of the SCM
#'   model on that parameter:** the rows and the reference row of a parameter
#'   hold the covariates that the SCM model has on that parameter. On each
#'   row, these covariates are at the values of the reference subject, apart
#'   from the covariate that varies on the row. The other FREM covariates are
#'   missing, so that the FREM predictions are integrated over them. An SCM
#'   model leaves those covariates out of the parameter, so both models
#'   average over the same covariates.
#'   * **Both plots have the same reference subject:** a relative value is the
#'   prediction on a row divided by the prediction for the reference subject.
#'
#'   The covariates that the SCM model has on a parameter are found from the
#'   SCM model itself: a covariate is on the parameter when the relative value
#'   of the parameter differs from 1 on a row of that covariate in at least one
#'   sample. For a secondary parameter such as `CLF = CL / FREL`, these are the
#'   covariates of `CL` and of `FREL` together. A parameter on which the SCM
#'   model has no covariate gets no plot, and a message is given.
#'
#'   For each parameter, the rows come from one call of
#'   [PMXForest::setupDfCovs()] with `useMissVal = FALSE`. That call covers the
#'   covariates of the parameter and every covariate that either model reads
#'   in `$PK`, so that all of them are set on every row; a covariate read in
#'   `$PK` that is not on the parameter stays at the reference value. Only the
#'   rows that vary a covariate of the parameter are kept. The reference row
#'   comes from [PMXForest::setupDfRefRow()]: by default, the median of each
#'   continuous covariate and the most common level of each categorical
#'   covariate.
#'
#'   A model can read a categorical covariate as the column itself or as
#'   indicator columns, such as `GENO1` or `GENO_1` for level 1 of `GENO`.
#'   Each model is given a copy of the rows and of the reference row in the
#'   coding that the model reads, so that the same subjects reach both models.
#'   Indicator columns that cover all levels but one of a covariate in `data`,
#'   such as `GENO1`, `GENO3` and `GENO4` of `GENO`, are shown as that
#'   covariate, with one row per level.
#'
#'   The uncertainty of each model is computed with
#'   [PMXForest::getSamples()]. By default, both models use samples from the
#'   covariance matrix of the run. A message is given when the two models get
#'   their uncertainty from different methods.
#'
#'   A model has no effect of a covariate on a parameter when the relative
#'   value is 1 in every sample, as for a level of a categorical covariate that
#'   the model does not have on that parameter. The direction of an effect is
#'   judged from the point estimate, relative to 1.
#'
#'   No verdict on the agreement of the two models is given in the text. For
#'   each parameter, the rows are counted by the direction of the two effects,
#'   and each row is described with the two point estimates. Where only one
#'   model has an effect, the interval of that model is given as well. Rows
#'   where the two effects are in opposite directions are listed first,
#'   ordered by the size of the difference, |log(FREM/SCM)|, computed before
#'   rounding. The other rows follow in the same order. Rows at the reference
#'   level, which are 1 in both models, are left out.
#'
#' @param scmModel The path of the control stream of the SCM model. The `.ext`
#'   file of the run is expected next to it, with the same name.
#' @param fremModel The path of the control stream of the FREM model. The
#'   `.ext` file of the run is expected next to it, with the same name.
#' @param data A data frame with the covariates of the subjects, such as the
#'   data set of the SCM model. The covariate values of the rows and of the
#'   reference subject are taken from it.
#' @param parameters The parameters of the models to compute, passed to
#'   [PMXForest::createParamFunction()] and [createFREMParamFunction()], for
#'   example `c("FREL", "CL")`.
#' @param secondary Secondary parameters computed from `parameters`, passed to
#'   both functions, for example `list(CLF = "CL / FREL")`.
#' @param plotParameters The parameters to plot and describe. By default, all
#'   parameters and secondary parameters are plotted. To compare the apparent
#'   clearance alone, compute `FREL` and `CL` with `parameters`, define `CLF`
#'   with `secondary` and set `plotParameters = "CLF"`.
#' @param parameterLabels Labels of the parameters for the panels, the text
#'   and the legends, as a named character vector such as `c(CLF = "CL/F")`.
#'   A parameter without a label is shown by name.
#' @param covariates The covariates on the plots. By default, each parameter
#'   gets the covariates that the SCM model has on that parameter. When
#'   `covariates` is given, every parameter gets these covariates, and the FREM
#'   predictions are integrated over the other FREM covariates.
#' @param contRef,catRef The reference of continuous and categorical
#'   covariates, passed to [PMXForest::setupDfCovs()] and
#'   [PMXForest::setupDfRefRow()]: by default, the median and the most common
#'   level.
#' @param samplesSCM,samplesFREM The parameter samples for the uncertainty of
#'   each model. Each is a path to a NONMEM `.cov` file (the covariance matrix
#'   of the estimates), a path to a raw results file of a bootstrap or of a
#'   sampling importance resampling (SIR) run of PsN, or a data frame from
#'   [PMXForest::getSamples()] with the final estimates in the first row. By
#'   default, `nSamples` samples are drawn from the `.cov` file of the run.
#' @param nSamples The number of samples drawn from a `.cov` file. Default is
#'   200.
#' @param probs The probabilities of the lower and upper limits of the
#'   confidence intervals. The default, `c(0.05, 0.95)`, gives 90% intervals.
#' @param covRef Reference values of covariates for
#'   [PMXForest::createParamFunction()] and [createFREMParamFunction()], set
#'   only when one of these functions stops and asks for them, for a
#'   covariate whose reference the control stream does not state. The values
#'   do not change the plots, because every covariate that a model reads in
#'   `$PK` is set on every row and in the reference row.
#' @param idVar The name of the subject identifier column in `data`.
#' @param missVal The value of a missing covariate.
#' @param name_map,unit_map,level_map,group_map Labels of the covariates and
#'   levels, passed to [PMXForest::generateCovNames()], and used for the
#'   reference subject in the legends.
#' @param logScale If `TRUE` (default), the x axis is logarithmic.
#' @param quiet If `TRUE` (default), the messages of the Forest plot functions
#'   are not printed.
#' @param ... Other arguments of [PMXForest::forestPlot()], such as `xlim`.
#'
#' @return An object of class `compareFREMSCM`: a list with
#'   * `plot`: a list with one Forest plot per plotted parameter, named by the
#'   parameter. Each plot has an SCM panel and a FREM panel.
#'   * `comparison`: one row per parameter and row of the plot, with the
#'   label, the point estimate and the interval of each model
#'   (`SCM`, `SCM_LOWER`, `SCM_UPPER`, `FREM`, `FREM_LOWER`, `FREM_UPPER`),
#'   the ratio FREM/SCM, and the direction: `"reference"`, `"same"`,
#'   `"opposite"`, `"no effect in SCM"`, `"no effect in FREM"` or
#'   `"no effect in either"`. The row number `COVNUM` counts the rows of
#'   each parameter.
#'   * `dfres`: the output of [PMXForest::getForestDFSCM()] and
#'   [getForestDFFREM()] for the plotted parameters, with the model in the
#'   column `MODEL`.
#'   * `description`: the text that describes the differences, one sentence
#'   per element.
#'   * `legend`: the figure legend of each plot, named by the parameter.
#'   * `settings`: per parameter, the covariates, the rows and the reference
#'   row (shared, and the copy of each model) and the FREM covariates that the
#'   predictions are integrated over; and the uncertainty method and number of
#'   samples of each model, and `probs`.
#'
#'   Printing the object draws the plots. `print(x, text = TRUE)` also prints
#'   the text and the legends.
#'
#' @seealso The deep dive "Reconciling FREM and SCM Forest Plots",
#'   `vignette("Part3-deep-dive-forest-plot-references", package = "PMXFrem")`,
#'   which builds the same comparison step by step.
#' @concept diagnostics
#' @export
#'
#' @examples
#' \donttest{
#' simVal <- system.file("extdata/SimVal", package = "PMXForest")
#' res <- compareFREMSCM(
#'   scmModel        = file.path(simVal, "run7.mod"),
#'   fremModel       = file.path(simVal, "run22-3.mod"),
#'   data            = read.csv(file.path(simVal, "DAT-1-MI-PMX-2.csv")),
#'   parameters      = c("FREL", "CL"),
#'   secondary       = list(CLF = "CL / FREL"),
#'   plotParameters  = "CLF",
#'   parameterLabels = c(CLF = "CL/F"),
#'   samplesSCM      = file.path(simVal, "bs7n30.dir", "raw_results_run7bs.csv"),
#'   samplesFREM     = file.path(simVal, "bs22-3n30.dir", "raw_results_run22-3bs.csv")
#' )
#' res
#' res$description
#' res$legend
#' }
compareFREMSCM <- function(scmModel,
                           fremModel,
                           data,
                           parameters,
                           secondary = NULL,
                           plotParameters = NULL,
                           parameterLabels = NULL,
                           covariates = NULL,
                           contRef = "median",
                           catRef = "mode",
                           samplesSCM = NULL,
                           samplesFREM = NULL,
                           nSamples = 200,
                           probs = c(0.05, 0.95),
                           covRef = NULL,
                           idVar = "ID",
                           missVal = -99,
                           name_map = NULL,
                           unit_map = NULL,
                           level_map = NULL,
                           group_map = NULL,
                           logScale = TRUE,
                           quiet = TRUE,
                           ...) {
  for (f in c(scmModel, fremModel)) {
    if (!file.exists(f)) stop("The model file ", f, " does not exist.")
  }
  data <- as.data.frame(data)
  models <- c(
    SCM = tools::file_path_sans_ext(basename(scmModel)),
    FREM = tools::file_path_sans_ext(basename(fremModel))
  )

  ## The parameter functions, and the covariates each model reads in $PK
  genSCM <- .cfsGenerate(
    PMXForest::createParamFunction, scmModel, parameters, secondary, covRef
  )
  genFREM <- .cfsGenerate(
    createFREMParamFunction, fremModel, parameters, secondary, covRef
  )
  funSCM <- eval(parse(text = paste(genSCM$code, collapse = "\n")))
  funFREM <- eval(parse(text = paste(genFREM$code, collapse = "\n")))
  readsSCM <- union(names(genSCM$covRef), attr(funSCM, "pmxCovariates"))
  readsFREM <- union(names(genFREM$covRef), attr(funFREM, "pmxCovariates"))
  fremInfo <- getCovNames(modFile = fremModel)
  fremCovNames <- fremInfo$covNames

  if (is.null(plotParameters)) plotParameters <- genSCM$functionListName
  for (p in plotParameters) {
    if (!(p %in% genSCM$functionListName) || !(p %in% genFREM$functionListName)) {
      stop("The parameter ", p, " in `plotParameters` is not computed for both models.")
    }
  }

  absent <- setdiff(covariates, names(data))
  if (length(absent) > 0) {
    stop("Covariates not in `data`: ", paste(absent, collapse = ", "), ".")
  }
  ## The covariates of the data behind the names that the models read in $PK
  bases <- .cfsBases(
    reads = unique(c(readsSCM, readsFREM)), data = data,
    bases = unique(covariates), missVal = missVal
  )
  if (length(bases$dropped) > 0) {
    message(
      "Covariates read by a model and not in `data` are left out: ",
      paste(bases$dropped, collapse = ", "), "."
    )
  }
  readsSCM <- setdiff(readsSCM, bases$dropped)
  readsFREM <- setdiff(readsFREM, bases$dropped)

  ## A single setting is passed as list(default = ...), because
  ## generateCovNames() of PMXForest before 1.4.2 stops on a single catRef
  ## value. The conversion can go once PMXFrem requires PMXForest 1.4.2.
  if (!is.list(contRef)) contRef <- list(default = contRef)
  if (!is.list(catRef)) catRef <- list(default = catRef)

  ## The rows and the reference row for the covariates `covs`, and a copy of
  ## each in the coding each model reads. Every covariate read in $PK is set;
  ## a FREM covariate that cannot be derived from `covs` is missing.
  rowsFor <- function(covs) {
    allCovs <- unique(c(covs, bases$bases))
    dfAll <- PMXForest::setupDfCovs(data,
      covariates = allCovs, useMissVal = FALSE, contRef = contRef,
      catRef = catRef, idVar = idVar, missVal = missVal, sep = "_"
    )
    dfRefRow <- PMXForest::setupDfRefRow(dfAll,
      data = data, covariates = allCovs, contRef = contRef, catRef = catRef,
      idVar = idVar, missVal = missVal, sep = "_"
    )
    dfCovs <- dfAll[dfAll$COVARIATEGROUPS %in% covs, , drop = FALSE]
    rownames(dfCovs) <- NULL
    raw <- .cfsDecode(dfCovs, allCovs, data, missVal)
    rawRef <- .cfsDecode(dfRefRow, allCovs, data, missVal)
    fremSet <- fremCovNames[.cfsDerivable(fremCovNames, data, covs, missVal)]
    fremMissing <- setdiff(fremCovNames, c(fremSet, readsFREM))
    mapSCM <- .cfsResolve(readsSCM, data, allCovs, missVal)
    mapFREM <- .cfsResolve(unique(c(readsFREM, fremSet)), data, allCovs, missVal)
    groups <- data.frame(COVARIATEGROUPS = dfCovs$COVARIATEGROUPS)
    list(
      dfCovs = dfCovs, dfRefRow = dfRefRow, raw = raw, rawRef = rawRef,
      dfCovsSCM = cbind(.cfsEncode(raw, mapSCM, missVal), groups),
      dfRefRowSCM = .cfsEncode(rawRef, mapSCM, missVal),
      dfCovsFREM = cbind(
        .cfsMissing(.cfsEncode(raw, mapFREM, missVal), fremMissing, missVal), groups
      ),
      dfRefRowFREM = .cfsMissing(.cfsEncode(rawRef, mapFREM, missVal), fremMissing, missVal),
      integrated = fremInfo$orgCovNames[
        !.cfsDerivable(fremInfo$orgCovNames, data, covs, missVal)
      ]
    )
  }

  ## The uncertainty of each model
  smpSCM <- .cfsSamples(samplesSCM, scmModel, nSamples)
  smpFREM <- .cfsSamples(samplesFREM, fremModel, nSamples)
  if (smpSCM$method != smpFREM$method) {
    message(
      "The uncertainty of the SCM model is from ", .cfsMethodText(smpSCM),
      " and the uncertainty of the FREM model from ", .cfsMethodText(smpFREM), "."
    )
  }
  runSCM <- function(r) {
    as.data.frame(PMXForest::getForestDFSCM(
      dfCovs           = r$dfCovsSCM,
      dfRefRow         = r$dfRefRowSCM,
      functionList     = list(funSCM),
      functionListName = genSCM$functionListName,
      noBaseThetas     = genSCM$noBaseThetas,
      dfParameters     = smpSCM$df,
      probs            = probs,
      quiet            = quiet
    ))
  }
  runFREM <- function(r) {
    as.data.frame(getForestDFFREM(
      dfCovs           = r$dfCovsFREM,
      dfRefRow         = r$dfRefRowFREM,
      covNames         = fremCovNames,
      numNonFREMThetas = genFREM$numNonFREMThetas,
      numSkipOm        = genFREM$numSkipOm,
      dfParameters     = smpFREM$df,
      functionList     = list(funFREM),
      functionListName = genFREM$functionListName,
      probs            = probs,
      quiet            = quiet
    ))
  }

  ## The covariates of each parameter: given, or those with an effect on the
  ## parameter in the SCM model
  if (is.null(covariates)) {
    candidates <- .cfsShown(readsSCM, data, bases$bases, missVal)
    r0 <- rowsFor(candidates)
    d0 <- runSCM(r0)
    d0$COVARIATE <- r0$dfCovs$COVARIATEGROUPS[d0$COVNUM]
    covsBy <- lapply(plotParameters, function(p) {
      hit <- d0$COVARIATE[as.character(d0$PARAMETER) == p & d0$COVEFF]
      candidates[candidates %in% hit]
    })
  } else {
    covsBy <- rep(list(covariates), length(plotParameters))
  }
  names(covsBy) <- plotParameters
  none <- plotParameters[lengths(covsBy) == 0]
  if (length(none) == length(plotParameters)) {
    stop("The SCM model has no covariate effect on any parameter in `plotParameters`.")
  }
  if (length(none) > 0) {
    message(
      "The SCM model has no covariate effect on ", paste(none, collapse = ", "),
      ", which ", if (length(none) == 1) "is" else "are", " not plotted."
    )
  }
  covsBy <- covsBy[lengths(covsBy) > 0]
  plotParameters <- names(covsBy)

  ## One comparison per parameter
  per <- lapply(plotParameters, function(p) {
    r <- rowsFor(covsBy[[p]])
    label <- function(dfres) {
      d <- as.data.frame(PMXForest::generateCovNames(r$dfCovs, dfres,
        name_map = name_map, unit_map = unit_map, level_map = level_map,
        group_map = group_map, warn_unused = FALSE
      ))
      d$PARAMETER <- as.character(d$PARAMETER)
      d[d$PARAMETER == p, , drop = FALSE]
    }
    s <- label(runSCM(r))
    f <- label(runFREM(r))
    cols <- intersect(names(s), names(f))
    dfres <- rbind(cbind(MODEL = "SCM", s[, cols]), cbind(MODEL = "FREM", f[, cols]))
    rownames(dfres) <- NULL
    list(
      rows = r, dfres = dfres,
      comparison = .cfsCompare(dfres, r$raw, r$rawRef, r$dfCovs$COVARIATEGROUPS, p)
    )
  })
  names(per) <- plotParameters
  pick <- function(what) lapply(per, function(x) x$rows[[what]])
  cols <- Reduce(intersect, lapply(per, function(x) names(x$dfres)))
  dfres <- do.call(rbind, lapply(per, function(x) x$dfres[, cols]))
  rownames(dfres) <- NULL
  comparison <- do.call(rbind, lapply(per, `[[`, "comparison"))
  rownames(comparison) <- NULL
  comparison$DIRECTION <- .cfsClassify(comparison)

  settings <- list(
    models = models, covariates = covsBy, covariatesGiven = !is.null(covariates),
    fremCovariates = fremInfo$orgCovNames, integrated = pick("integrated"),
    dfCovs = pick("dfCovs"), dfRefRow = pick("dfRefRow"),
    dfCovsSCM = pick("dfCovsSCM"), dfCovsFREM = pick("dfCovsFREM"),
    dfRefRowSCM = pick("dfRefRowSCM"), dfRefRowFREM = pick("dfRefRowFREM"),
    reference = lapply(plotParameters, function(p) {
      per[[p]]$rows$rawRef[, covsBy[[p]], drop = FALSE]
    }),
    uncertainty = data.frame(
      method = c(smpSCM$method, smpFREM$method),
      n = c(smpSCM$n, smpFREM$n), row.names = c("SCM", "FREM")
    ),
    probs = probs, contRef = contRef, catRef = catRef,
    plotParameters = plotParameters, parameterLabels = parameterLabels,
    labels = list(name_map = name_map, unit_map = unit_map, level_map = level_map),
    logScale = logScale
  )
  names(settings$reference) <- plotParameters

  plot <- lapply(plotParameters, function(p) .cfsPlot(per[[p]]$dfres, p, settings, ...))
  names(plot) <- plotParameters
  legend <- vapply(plotParameters, .cfsLegend, character(1), settings = settings)

  structure(
    list(
      plot = plot,
      comparison = comparison,
      dfres = dfres,
      description = .cfsDescribe(comparison, settings),
      legend = legend,
      settings = settings
    ),
    class = "compareFREMSCM"
  )
}

#' Print a comparison of an SCM and a FREM Forest plot
#'
#' @param x An object returned by [compareFREMSCM()].
#' @param text If `TRUE`, the description and the figure legends are printed
#'   after the plots are drawn. Default is `FALSE`.
#' @param ... Not used.
#' @return `x`, invisibly.
#' @concept diagnostics
#' @export
print.compareFREMSCM <- function(x, text = FALSE, ...) {
  for (p in x$plot) print(p)
  if (text) {
    cat(paste0(x$description, "\n"), sep = "")
    cat("\n", paste0(x$legend, "\n"), sep = "")
  }
  invisible(x)
}

## Which of `names` (FREM covariates) can be derived from the covariates
## `covs`: the covariate itself, or an indicator of a level of it.
.cfsDerivable <- function(names, data, covs, missVal) {
  vapply(names, function(n) {
    if (n %in% covs) {
      return(TRUE)
    }
    s <- .cfsSplit(n, data, missVal)
    !is.null(s) && s$base %in% covs
  }, logical(1), USE.NAMES = FALSE)
}

## Add the columns `names` as missing to `df`.
.cfsMissing <- function(df, names, missVal) {
  for (n in names) df[[n]] <- rep(missVal, nrow(df))
  df
}

## Build a parameter function. Without `covRef` first; with `covRef` only when
## the function stops without it. The warnings are about the references of
## covariates that are missing on a row, which never happens here.
.cfsGenerate <- function(generator, modFile, parameters, secondary, covRef) {
  build <- function(cr) {
    suppressWarnings(generator(modFile,
      parameters = parameters, secondary = secondary, covRef = cr,
      quiet = TRUE
    ))
  }
  tryCatch(build(NULL), error = function(e) {
    if (is.null(covRef)) stop(e)
    build(covRef)
  })
}

## The levels of a covariate in the data.
.cfsLevels <- function(x, missVal) {
  x <- x[!is.na(x) & x != missVal]
  sort(unique(x))
}

## Split an indicator name such as GENO1 or GENO_1 into a covariate of the
## data and a level of it. NULL when the name is not of that form.
.cfsSplit <- function(name, data, missVal) {
  cand <- names(data)[startsWith(name, names(data)) & names(data) != name]
  cand <- cand[order(-nchar(cand))]
  for (b in cand) {
    rest <- sub("^_", "", substring(name, nchar(b) + 1))
    lv <- .cfsLevels(data[[b]], missVal)
    if (nzchar(rest) && rest %in% as.character(lv)) {
      return(list(base = b, level = rest))
    }
  }
  NULL
}

## The covariates of the rows. `bases` are the covariates given (on the plot,
## or FREM covariates). A name read by a model is added as itself, unless it is
## an indicator of a covariate in `bases`, or the names read cover all levels
## but one of a covariate in the data, which is then added instead.
.cfsBases <- function(reads, data, bases, missVal) {
  reads <- setdiff(reads, bases)
  split <- lapply(reads, .cfsSplit, data = data, missVal = missVal)
  names(split) <- reads
  isInd <- !vapply(split, is.null, logical(1))
  indBase <- vapply(split[isInd], `[[`, character(1), "base")
  for (b in unique(indBase)) {
    if (b %in% bases) next
    lv <- vapply(split[isInd][indBase == b], `[[`, character(1), "level")
    nonData <- any(!(names(indBase)[indBase == b] %in% names(data)))
    if (length(unique(lv)) == length(.cfsLevels(data[[b]], missVal)) - 1 || nonData) {
      bases <- c(bases, b)
    }
  }
  dropped <- character()
  for (n in reads) {
    s <- split[[n]]
    if (!is.null(s) && s$base %in% bases) next
    if (n %in% names(data)) {
      bases <- c(bases, n)
    } else {
      dropped <- c(dropped, n)
    }
  }
  list(bases = unique(bases), dropped = dropped)
}

## The covariates on the plot by default: the names the SCM model reads, an
## indicator shown as the covariate it belongs to.
.cfsShown <- function(reads, data, bases, missVal) {
  out <- character()
  for (n in reads) {
    if (n %in% bases) {
      out <- c(out, n)
    } else {
      s <- .cfsSplit(n, data, missVal)
      if (!is.null(s) && s$base %in% bases) out <- c(out, s$base)
    }
  }
  unique(out)
}

## Map each name a model reads to a covariate of the rows and, for an
## indicator, a level. Stops when a data column with the name of an indicator
## does not equal the indicator computed from its covariate.
.cfsResolve <- function(names, data, bases, missVal = -99) {
  out <- data.frame(name = character(), base = character(), level = character(), stringsAsFactors = FALSE)
  for (n in names) {
    if (n %in% bases) {
      out[nrow(out) + 1, ] <- c(n, n, NA)
      next
    }
    s <- .cfsSplit(n, data, missVal)
    if (is.null(s) || !(s$base %in% bases)) {
      stop("The covariate ", n, " is read by a model but cannot be derived from the covariates in `data`.")
    }
    if (n %in% names(data)) {
      b <- data[[s$base]]
      ok <- !is.na(b) & b != missVal & !is.na(data[[n]]) & data[[n]] != missVal
      if (any(data[[n]][ok] != as.numeric(as.character(b[ok]) == s$level))) {
        stop(
          "The column ", n, " in `data` is not the indicator of ", s$base,
          " = ", s$level, " in every record."
        )
      }
    }
    out[nrow(out) + 1, ] <- c(n, s$base, s$level)
  }
  out
}

## Write the columns a model reads from covariate values `raw`.
.cfsEncode <- function(raw, map, missVal) {
  out <- data.frame(row.names = seq_len(nrow(raw)))
  for (i in seq_len(nrow(map))) {
    v <- raw[[map$base[i]]]
    if (!is.na(map$level[i])) {
      v <- ifelse(v == missVal, missVal, as.numeric(as.character(v) == map$level[i]))
    }
    out[[map$name[i]]] <- v
  }
  out
}

## The value of each covariate on each row, from the indicator columns that
## setupDfCovs() writes for a covariate with more than two levels.
.cfsDecode <- function(df, covs, data, missVal) {
  out <- data.frame(row.names = seq_len(nrow(df)))
  for (b in covs) {
    if (b %in% names(df)) {
      out[[b]] <- df[[b]]
      next
    }
    lv <- as.character(.cfsLevels(data[[b]], missVal))
    ind <- intersect(paste0(b, "_", lv), names(df))
    left <- setdiff(lv, sub(paste0("^", b, "_"), "", ind))
    v <- rep(suppressWarnings(as.numeric(left[1])), nrow(df))
    for (col in ind) {
      v[df[[col]] == 1] <- as.numeric(sub(paste0("^", b, "_"), "", col))
    }
    if (length(ind) > 0) v[df[[ind[1]]] == missVal] <- missVal
    out[[b]] <- v
  }
  out
}

## Read the samples for the uncertainty of one model.
.cfsSamples <- function(x, modFile, n) {
  extFile <- paste0(tools::file_path_sans_ext(modFile), ".ext")
  if (is.null(x)) x <- paste0(tools::file_path_sans_ext(modFile), ".cov")
  if (is.data.frame(x)) {
    return(list(df = x, method = "supplied data frame", n = nrow(x) - 1))
  }
  if (!file.exists(x)) stop("The file ", x, " does not exist.")
  ext <- tolower(tools::file_ext(x))
  if (ext == "cov") {
    df <- PMXForest::getSamples(x, extFile = extFile, n = n, quiet = TRUE)
    method <- "covariance matrix"
  } else if (ext == "csv") {
    head <- names(utils::read.csv(x, nrows = 1, check.names = FALSE))
    method <- if (all(c("resamples", "sample_order") %in% head)) "SIR" else "bootstrap"
    df <- PMXForest::getSamples(x, extFile = extFile, quiet = TRUE)
  } else {
    stop("The samples file ", x, " is neither a .cov file nor a PsN raw results .csv file.")
  }
  list(df = df, method = method, n = nrow(df) - 1)
}

.cfsMethodText <- function(s) {
  switch(s$method,
    "covariance matrix" = paste(s$n, "samples from the covariance matrix"),
    "bootstrap" = paste("a bootstrap of", s$n, "samples"),
    "SIR" = paste("a SIR of", s$n, "samples"),
    paste("a supplied data frame of", s$n, "samples")
  )
}

## One row per parameter and row of the plot, with the effects of both models.
.cfsCompare <- function(dfres, raw, rawRef, groups, plotParameters) {
  pick <- function(model) {
    d <- dfres[dfres$MODEL == model, ]
    d[, c(
      "PARAMETER", "COVNUM", "COVNAME", "POINT_NOVAR_REL_REFFUNC",
      "Q1_NOVAR_REL_REFFUNC", "Q2_NOVAR_REL_REFFUNC", "COVEFF"
    )]
  }
  s <- pick("SCM")
  f <- pick("FREM")
  names(s)[4:7] <- c("SCM", "SCM_LOWER", "SCM_UPPER", "SCM_COVEFF")
  names(f)[4:7] <- c("FREM", "FREM_LOWER", "FREM_UPPER", "FREM_COVEFF")
  cmp <- merge(s, f[, -3], by = c("PARAMETER", "COVNUM"), sort = FALSE)
  names(cmp)[names(cmp) == "COVNAME"] <- "LABEL"
  cmp$PARAMETER <- as.character(cmp$PARAMETER)
  cmp$LABEL <- as.character(cmp$LABEL)
  cmp$COVARIATE <- groups[cmp$COVNUM]
  cmp$LEVEL <- vapply(seq_len(nrow(cmp)), function(i) {
    as.character(raw[[cmp$COVARIATE[i]]][cmp$COVNUM[i]])
  }, character(1))
  cmp$AT_REFERENCE <- vapply(seq_len(nrow(cmp)), function(i) {
    raw[[cmp$COVARIATE[i]]][cmp$COVNUM[i]] == rawRef[[cmp$COVARIATE[i]]][1]
  }, logical(1))
  cmp$RATIO <- cmp$FREM / cmp$SCM
  cmp <- cmp[order(match(cmp$PARAMETER, plotParameters), cmp$COVNUM), ]
  rownames(cmp) <- NULL
  cmp[, c(
    "PARAMETER", "COVNUM", "COVARIATE", "LEVEL", "LABEL", "SCM", "SCM_LOWER",
    "SCM_UPPER", "FREM", "FREM_LOWER", "FREM_UPPER", "RATIO", "SCM_COVEFF",
    "FREM_COVEFF", "AT_REFERENCE"
  )]
}

## The direction of the two effects of each row, relative to 1.
.cfsClassify <- function(cmp) {
  dirOf <- function(x, eff) ifelse(eff, sign(log(x)), 0)
  s <- dirOf(cmp$SCM, cmp$SCM_COVEFF)
  f <- dirOf(cmp$FREM, cmp$FREM_COVEFF)
  out <- ifelse(s == f, "same", "opposite")
  out[s == 0 & f != 0] <- "no effect in SCM"
  out[s != 0 & f == 0] <- "no effect in FREM"
  out[s == 0 & f == 0] <- "no effect in either"
  out[cmp$AT_REFERENCE] <- "reference"
  out
}

## A number in the text, with three significant digits.
.cfsNum <- function(x) formatC(signif(x, 3), digits = 3, format = "fg", flag = "#")

## The label of a parameter, or the name when it has none.
.cfsParLabel <- function(p, labels) {
  if (!is.null(labels) && p %in% names(labels)) labels[[p]] else p
}


## The label of a covariate, or the name when it has none.
.cfsCovLabel <- function(cv, lab) {
  if (!is.null(lab$name_map) && cv %in% names(lab$name_map)) lab$name_map[[cv]] else cv
}

## A list of words joined by commas and "and".
.cfsAnd <- function(x) {
  if (length(x) < 2) {
    return(paste(x, collapse = ""))
  }
  paste(paste(utils::head(x, -1), collapse = ", "), "and", utils::tail(x, 1))
}

## The sentence on what the FREM predictions are conditioned on.
.cfsConditionText <- function(settings) {
  if (isTRUE(settings$covariatesGiven)) {
    paste0(
      "On each parameter, the FREM predictions are conditioned on the ",
      "covariates on the plot and integrated over the other FREM covariates."
    )
  } else {
    paste0(
      "On each parameter, the FREM predictions are conditioned on the ",
      "covariates that the SCM model has on that parameter and integrated ",
      "over the other FREM covariates."
    )
  }
}

## The description: for each parameter, the covariates and the rows counted
## by direction, then one sentence per row, opposite directions first. Each
## sentence names the row and the parameter.
.cfsDescribe <- function(cmp, settings) {
  m <- settings$models
  out <- c(
    paste0(
      "All values are covariate effects relative to one reference subject, ",
      "the same for the SCM model ", m[["SCM"]], " and the FREM model ",
      m[["FREM"]], "."
    ),
    .cfsConditionText(settings),
    paste0(
      "A model has no effect of a covariate on a parameter when the relative ",
      "value is 1 in every sample."
    )
  )
  classText <- c(
    "opposite" = "the two effects are in opposite directions",
    "same" = "the two effects are in the same direction",
    "no effect in SCM" = "only FREM has an effect",
    "no effect in FREM" = "only the SCM model has an effect",
    "no effect in either" = "neither model has an effect"
  )
  interval <- function(model, lo, hi) {
    paste0("The ", model, " interval is ", .cfsNum(lo), " to ", .cfsNum(hi), ".")
  }
  for (p in unique(cmp$PARAMETER)) {
    pl <- .cfsParLabel(p, settings$parameterLabels)
    covs <- settings$covariates[[p]]
    if (length(covs) > 0) {
      labs <- vapply(covs, .cfsCovLabel, character(1), lab = settings$labels)
      out <- c(out, paste0("The plot of ", pl, " has rows for ", .cfsAnd(labs), "."))
    }
    all <- cmp[cmp$PARAMETER == p, ]
    d <- all[all$DIRECTION != "reference", ]
    n <- nrow(d)
    out <- c(out, paste0(
      "On ", pl, ", ", n, " of the ", nrow(all),
      " rows of the plot are not at the reference level."
    ))
    for (cl in names(classText)) {
      k <- sum(d$DIRECTION == cl)
      if (k > 0) {
        out <- c(out, paste0(
          "In ", k, " of the ", n, " rows on ", pl,
          " that are not at the reference level, ", classText[[cl]], "."
        ))
      }
    }
    size <- abs(log(d$FREM / d$SCM))
    d <- d[order(d$DIRECTION != "opposite", -size), ]
    for (i in seq_len(nrow(d))) {
      r <- d[i, ]
      head <- paste0(r$LABEL, ", ", pl, ": ")
      sent <- switch(r$DIRECTION,
        "opposite" = c(
          paste0(
            head, .cfsNum(r$SCM), " in the SCM model and ", .cfsNum(r$FREM),
            " in FREM, in opposite directions."
          ),
          interval("SCM", r$SCM_LOWER, r$SCM_UPPER),
          interval("FREM", r$FREM_LOWER, r$FREM_UPPER)
        ),
        "same" = paste0(
          head, .cfsNum(r$SCM), " in the SCM model and ", .cfsNum(r$FREM),
          " in FREM, both ", if (r$SCM > 1) "above" else "below", " 1."
        ),
        "no effect in SCM" = c(
          paste0(head, "no effect in the SCM model and ", .cfsNum(r$FREM), " in FREM."),
          interval("FREM", r$FREM_LOWER, r$FREM_UPPER)
        ),
        "no effect in FREM" = c(
          paste0(head, .cfsNum(r$SCM), " in the SCM model and no effect in FREM."),
          interval("SCM", r$SCM_LOWER, r$SCM_UPPER)
        ),
        "no effect in either" = paste0(head, "no effect in either model.")
      )
      out <- c(out, sent)
    }
  }
  out
}

## The reference subject in words, with the labels of the covariates.
.cfsReferenceText <- function(ref, lab) {
  parts <- vapply(names(ref), function(cv) {
    v <- ref[[cv]][1]
    lv <- lab$level_map[[cv]]
    value <- if (!is.null(lv) && as.character(v) %in% names(lv)) {
      lv[[as.character(v)]]
    } else {
      unit <- if (!is.null(lab$unit_map) && cv %in% names(lab$unit_map)) paste0(" ", lab$unit_map[[cv]]) else ""
      paste0(format(v), unit)
    }
    paste0(.cfsCovLabel(cv, lab), " = ", value)
  }, character(1))
  paste(parts, collapse = ", ")
}

## The figure legend of the plot of parameter `p`.
.cfsLegend <- function(p, settings) {
  m <- settings$models
  u <- settings$uncertainty
  pl <- .cfsParLabel(p, settings$parameterLabels)
  refKind <- if (identical(settings$contRef, list(default = "median")) &&
    identical(settings$catRef, list(default = "mode"))) {
    "the median of each continuous covariate and the most common level of each categorical covariate"
  } else {
    "the reference values set by `contRef` and `catRef`"
  }
  rowsText <- if (isTRUE(settings$covariatesGiven)) {
    "The rows hold the covariates on the plot. "
  } else {
    paste0("The rows hold the covariates that the SCM model has on ", pl, ". ")
  }
  nInt <- length(settings$integrated[[p]])
  level <- round(100 * diff(settings$probs))
  methodOf <- function(model) .cfsMethodText(list(method = u[model, "method"], n = u[model, "n"]))
  paste0(
    "Covariate effects on ", pl, " in the SCM model ", m[["SCM"]],
    " (SCM panel) and the FREM model ", m[["FREM"]],
    " (FREM panel), relative to one reference subject for both models. ",
    "The reference subject has ", refKind, ": ",
    .cfsReferenceText(settings$reference[[p]], settings$labels), ". ",
    rowsText,
    "On each row, these covariates are at the values of the reference subject, ",
    "apart from the covariate of the row. ",
    "The FREM predictions are integrated over the other ", nInt, " of the ",
    length(settings$fremCovariates), " FREM covariates. ",
    "Points are point estimates, and bars are ", level, "% confidence intervals. ",
    "The intervals are from ", methodOf("SCM"), " for the SCM model and from ",
    methodOf("FREM"), " for the FREM model. ",
    "The grey band is the range from 0.8 to 1.25 times the reference.",
    if (isTRUE(settings$logScale)) " The x axis is logarithmic." else ""
  )
}

## The Forest plot of parameter `p`, with an SCM and a FREM panel.
.cfsPlot <- function(dfres, p, settings, ...) {
  d <- dfres
  pl <- .cfsParLabel(p, settings$parameterLabels)
  d$PARAMETER <- paste(d$MODEL, pl)
  d$MODEL <- NULL
  info <- if (isTRUE(settings$covariatesGiven)) {
    "FREM conditioned on the covariates on the plot"
  } else {
    "FREM conditioned on the SCM covariates of the parameter"
  }
  args <- list(
    dfres = d, parameters = c(paste("SCM", pl), paste("FREM", pl)),
    rightStrip = FALSE, statisticsLabel = "",
    referenceInfo = paste0("Reference: one subject for both models; ", info)
  )
  if (isTRUE(settings$logScale)) {
    args$addcodeErr <- "scale_x_log10(breaks = c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8))"
  }
  args <- utils::modifyList(args, list(...))
  do.call(PMXForest::forestPlot, args)
}
