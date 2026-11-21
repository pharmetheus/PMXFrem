## compareFREMSCM() is checked against the step-by-step recipe of the deep
## dive "Reconciling FREM and SCM Forest Plots" (section 10), on the SCM and
## FREM models of the PMXForest SimVal data with the 30-sample bootstraps.

cfsSimVal <- function(...) {
  file.path(system.file("extdata/SimVal", package = "PMXForest"), ...)
}

## Computed once per test run: the function result, the warnings given while
## it ran, and the reference result of the recipe.
cfsFixture <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      return(cache)
    }
    data <- read.csv(cfsSimVal("DAT-1-MI-PMX-2.csv"))
    warns <- list()
    msgs <- character()
    res <- withCallingHandlers(
      compareFREMSCM(
        scmModel    = cfsSimVal("run7.mod"),
        fremModel   = cfsSimVal("run22-3.mod"),
        data        = data,
        parameters  = c("FREL", "CL"),
        secondary   = list(CLF = "CL / FREL"),
        samplesSCM  = cfsSimVal("bs7n30.dir", "raw_results_run7bs.csv"),
        samplesFREM = cfsSimVal("bs22-3n30.dir", "raw_results_run22-3bs.csv")
      ),
      warning = function(w) {
        warns[[length(warns) + 1]] <<- w
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )

    ## The recipe of the deep dive: genotype coded as the models read it,
    ## every FREM covariate set on every row, one reference row for both.
    shown <- res$settings$covariates
    fremCovs <- getCovNames(modFile = cfsSimVal("run22-3.mod"))$orgCovNames
    allCovs <- unique(c(shown, fremCovs))
    dfCovsAll <- PMXForest::setupDfCovs(data,
      covariates = allCovs, useMissVal = FALSE, idVar = "ID",
      catRef = list(GENO = 2), sep = ""
    )
    dfCovs <- dfCovsAll[dfCovsAll$COVARIATEGROUPS %in% shown, ]
    dfRefRow <- PMXForest::setupDfRefRow(dfCovsAll,
      data = data, covariates = allCovs, idVar = "ID",
      catRef = list(GENO = 2), sep = ""
    )
    secondary <- list(CLF = "CL / FREL")
    genSCM <- suppressWarnings(PMXForest::createParamFunction(cfsSimVal("run7.mod"),
      parameters = c("FREL", "CL"), covRef = list(GENO1 = 0, GENO3 = 0),
      secondary = secondary, quiet = TRUE
    ))
    funSCM <- eval(parse(text = paste(genSCM$code, collapse = "\n")))
    genFREM <- suppressWarnings(createFREMParamFunction(cfsSimVal("run22-3.mod"),
      parameters = c("FREL", "CL"), covRef = list(GENO1 = 0, GENO3 = 0),
      secondary = secondary, quiet = TRUE
    ))
    funFREM <- eval(parse(text = genFREM$code))
    samplesSCM <- PMXForest::getSamples(cfsSimVal("bs7n30.dir", "raw_results_run7bs.csv"),
      extFile = cfsSimVal("run7.ext"), quiet = TRUE
    )
    samplesFREM <- PMXForest::getSamples(cfsSimVal("bs22-3n30.dir", "raw_results_run22-3bs.csv"),
      extFile = cfsSimVal("run22-3.ext"), quiet = TRUE
    )
    refSCM <- PMXForest::getForestDFSCM(
      dfCovs = dfCovs, dfRefRow = dfRefRow, functionList = list(funSCM),
      functionListName = genSCM$functionListName,
      noBaseThetas = genSCM$noBaseThetas, dfParameters = samplesSCM,
      quiet = TRUE
    )
    refFREM <- suppressWarnings(getForestDFFREM(
      dfCovs = dfCovs, dfRefRow = dfRefRow, modName = "run22-3",
      modDevDir = cfsSimVal(), dfParameters = samplesFREM,
      functionList = list(funFREM),
      functionListName = genFREM$functionListName, quiet = TRUE
    ))
    cache <<- list(
      res = res, warns = warns, msgs = msgs, data = data,
      refSCM = refSCM, refFREM = refFREM, dfCovs = dfCovs
    )
    cache
  }
})

## The relative values that forestPlot() draws, by row number and parameter.
cfsValues <- function(dfres) {
  cols <- c("COVNUM", "PARAMETER", "POINT_NOVAR_REL_REFFUNC", "Q1_NOVAR_REL_REFFUNC", "Q2_NOVAR_REL_REFFUNC")
  out <- as.data.frame(dfres)[, cols]
  out <- out[order(out$PARAMETER, out$COVNUM), ]
  rownames(out) <- NULL
  out
}

test_that("compareFREMSCM gives the values of the step-by-step recipe for both models", {
  skip_on_cran()
  fx <- cfsFixture()
  got <- fx$res$dfres
  expect_equal(cfsValues(got[got$MODEL == "SCM", ]), cfsValues(fx$refSCM), tolerance = 1e-10)
  expect_equal(cfsValues(got[got$MODEL == "FREM", ]), cfsValues(fx$refFREM), tolerance = 1e-10)
})

test_that("compareFREMSCM shows the covariates that the SCM model reads, genotype as one covariate", {
  skip_on_cran()
  fx <- cfsFixture()
  expect_setequal(fx$res$settings$covariates, c("SEX", "GENO", "FORM", "FOOD", "WT"))
})

test_that("compareFREMSCM gives rows at the reference level the value 1 in both models", {
  skip_on_cran()
  fx <- cfsFixture()
  cmp <- fx$res$comparison
  ref <- cmp[cmp$DIRECTION == "reference", ]
  ## one reference row each for sex, formulation, food and genotype, per parameter
  expect_equal(nrow(ref), 4 * 3)
  expect_true(all(ref$SCM == 1))
  expect_true(all(ref$FREM == 1))
})

test_that("compareFREMSCM gives the weight rows the same values in both models", {
  skip_on_cran()
  fx <- cfsFixture()
  cmp <- fx$res$comparison
  wt <- cmp[cmp$COVARIATE == "WT", ]
  expect_equal(nrow(wt), 2 * 3)
  expect_equal(wt$SCM, wt$FREM, tolerance = 1e-12)
})

test_that("compareFREMSCM sets every covariate of each model on every row and in the reference row", {
  skip_on_cran()
  fx <- cfsFixture()
  s <- fx$res$settings
  for (df in list(s$dfCovsSCM, s$dfCovsFREM, s$dfRefRowSCM, s$dfRefRowFREM)) {
    num <- df[, setdiff(names(df), "COVARIATEGROUPS"), drop = FALSE]
    expect_false(any(num == -99))
  }
  ## the FREM copy holds every FREM covariate, the SCM copy the genotype
  ## indicators that run7 reads
  expect_true(all(getCovNames(modFile = cfsSimVal("run22-3.mod"))$covNames %in% names(s$dfCovsFREM)))
  expect_true(all(c("GENO1", "GENO3", "GENO4") %in% names(s$dfCovsSCM)))
})

test_that("compareFREMSCM passes genotype to the SCM model in the coding the model reads", {
  skip_on_cran()
  fx <- cfsFixture()
  cmp <- fx$res$comparison
  pm <- cmp[cmp$COVARIATE == "GENO" & cmp$LEVEL == "4" & cmp$PARAMETER == "CLF", ]
  expect_equal(nrow(pm), 1)
  expect_lt(pm$SCM, 0.2)
  expect_equal(pm$DIRECTION, "same")
})

test_that("compareFREMSCM has one comparison row per row of the plot and parameter", {
  skip_on_cran()
  fx <- cfsFixture()
  cmp <- fx$res$comparison
  expect_equal(nrow(cmp), nrow(fx$dfCovs) * 3)
  expect_equal(as.vector(table(cmp$PARAMETER)), rep(nrow(fx$dfCovs), 3))
  expect_false(any(duplicated(cmp[, c("PARAMETER", "COVNUM")])))
})

test_that("compareFREMSCM gives no covariate-name warning on the SimVal pair", {
  skip_on_cran()
  fx <- cfsFixture()
  mismatch <- vapply(fx$warns, function(w) inherits(w, "pmxCovariateMismatch"), logical(1))
  expect_equal(sum(mismatch), 0)
})

test_that("compareFREMSCM reports the uncertainty methods in the legend and settings", {
  skip_on_cran()
  fx <- cfsFixture()
  s <- fx$res$settings
  expect_equal(unname(s$uncertainty$method), c("bootstrap", "bootstrap"))
  expect_equal(unname(s$uncertainty$n), c(30, 30))
  leg <- paste(fx$res$legend, collapse = " ")
  expect_match(leg, "90%")
  expect_match(leg, "bootstrap")
  expect_match(leg, "30")
  ## the same method for both models gives no message about it
  expect_false(any(grepl("uncertainty", fx$msgs, ignore.case = TRUE)))
})

test_that("compareFREMSCM gives one message when the uncertainty methods differ", {
  skip_on_cran()
  fx <- cfsFixture()
  msgs <- character()
  res <- withCallingHandlers(
    suppressWarnings(compareFREMSCM(
      scmModel = cfsSimVal("run7.mod"), fremModel = cfsSimVal("run22-3.mod"),
      data = fx$data, parameters = c("FREL", "CL"),
      secondary = list(CLF = "CL / FREL"), plotParameters = "CLF",
      samplesSCM = cfsSimVal("bs7n30.dir", "raw_results_run7bs.csv"),
      nSamples = 10,
      ## the same label on the four genotype rows: the models are joined by
      ## row number, not by label
      level_map = list(GENO = c("1" = "genotype", "2" = "genotype", "3" = "genotype", "4" = "genotype"))
    )),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_equal(sum(grepl("uncertainty", msgs, ignore.case = TRUE)), 1)
  expect_equal(unname(res$settings$uncertainty$method), c("bootstrap", "covariance matrix"))
  expect_equal(unname(res$settings$uncertainty$n), c(30, 10))
  expect_equal(unique(res$comparison$PARAMETER), "CLF")
  expect_equal(sum(res$comparison$LABEL == "genotype"), 4)
  expect_equal(nrow(res$comparison), nrow(fx$dfCovs))
  expect_false(any(duplicated(res$comparison$COVNUM)))
})

test_that("the description gives no verdict and is printed only on request", {
  skip_on_cran()
  fx <- cfsFixture()
  txt <- paste(fx$res$description, collapse = " ")
  expect_true(nchar(txt) > 0)
  expect_false(grepl("agree|consistent|similar|confirm|match", txt, ignore.case = TRUE))
  expect_identical(
    .cfsDescribe(fx$res$comparison, fx$res$settings),
    fx$res$description
  )
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  quietOut <- capture.output(print(fx$res))
  expect_false(any(grepl(fx$res$description[1], quietOut, fixed = TRUE)))
  textOut <- capture.output(print(fx$res, text = TRUE))
  expect_true(any(grepl(fx$res$description[1], textOut, fixed = TRUE)))
  expect_true(any(grepl(fx$res$legend[1], textOut, fixed = TRUE)))
})

test_that("effects in opposite directions come first, ordered by the size of the difference", {
  cmp <- data.frame(
    PARAMETER = "CL", COVNUM = 1:5, COVARIATE = c("A", "B", "C", "D", "E"),
    LEVEL = NA_character_, LABEL = c("A", "B", "C", "D", "E"),
    SCM = c(1.5, 1.02, 1.3, 1, 1), FREM = c(0.95, 0.7, 1.25, 0.9, 1),
    SCM_LOWER = c(1.3, 0.9, 1.2, 1, 1), SCM_UPPER = c(1.7, 1.1, 1.4, 1, 1),
    FREM_LOWER = c(0.8, 0.6, 1.1, 0.8, 1), FREM_UPPER = c(1.1, 0.8, 1.4, 1, 1),
    SCM_COVEFF = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    FREM_COVEFF = c(TRUE, TRUE, TRUE, TRUE, FALSE),
    AT_REFERENCE = c(FALSE, FALSE, FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  cls <- .cfsClassify(cmp)
  expect_equal(cls, c("opposite", "opposite", "same", "no effect in SCM", "reference"))
  cmp$DIRECTION <- cls
  txt <- .cfsDescribe(cmp, list(models = c(SCM = "run7", FREM = "run22-3")))
  posA <- grep("^A, CL:", txt)
  posB <- grep("^B, CL:", txt)
  posC <- grep("^C, CL:", txt)
  expect_length(posA, 1)
  expect_length(posB, 1)
  expect_true(posA < posB)
  expect_true(posB < posC)
  expect_false(any(grepl("^E, CL:", txt)))
  ## an opposite row carries the interval of both models, a row with an
  ## effect in one model the interval of that model
  expect_equal(txt[posA + 1], "The SCM interval is 1.30 to 1.70.")
  expect_equal(txt[posA + 2], "The FREM interval is 0.800 to 1.10.")
  posD <- grep("^D, CL:", txt)
  expect_equal(txt[posD + 1], "The FREM interval is 0.800 to 1.00.")
})

test_that("the re-encoder writes each model's coding of a categorical covariate", {
  data <- data.frame(ID = 1:4, GENO = c(1, 2, 3, 4), WT = c(60, 70, 80, 90))
  map <- .cfsResolve(c("GENO", "GENO1", "GENO_1", "GENO4", "WT"), data, bases = c("GENO", "WT"))
  raw <- data.frame(GENO = c(1, 2, 4, -99), WT = c(70, 80, 90, 85))
  enc <- .cfsEncode(raw, map, missVal = -99)
  expect_equal(enc$GENO, c(1, 2, 4, -99))
  expect_equal(enc$GENO1, c(1, 0, 0, -99))
  expect_equal(enc$GENO_1, c(1, 0, 0, -99))
  expect_equal(enc$GENO4, c(0, 0, 1, -99))
  expect_equal(enc$WT, c(70, 80, 90, 85))
})

test_that("the re-encoder stops when a data indicator column disagrees with its base column", {
  data <- data.frame(ID = 1:4, GENO = c(1, 2, 3, 4), GENO1 = c(1, 1, 0, 0))
  expect_error(.cfsResolve("GENO1", data, bases = "GENO"), "GENO1")
})
