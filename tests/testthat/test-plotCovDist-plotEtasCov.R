# Coverage for the two diagnostic plot builders plotCovDist() and
# plotEtasCov(). Both are pure ggplot constructors on a calcEtas() data frame,
# so one real calcEtas() run feeds every case.

indParams <- local({
  d <- read.csv(
    system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")
  )
  d <- d[d$BLQ != 1, ]
  calcEtas(
    modName            = "run31",
    modDevDir          = system.file("extdata/SimNeb/", package = "PMXFrem"),
    numNonFREMThetas   = 7,
    numSkipOm          = 2,
    dataFile           = d,
    parNames           = c("CL", "V", "MAT"),
    ffemModName        = "run31max0",
    appendMissingFlags = TRUE
  )
})

## Layer geom class of the nth layer of a built plot.
geomClass <- function(p, layer = 1L) class(p$layers[[layer]]$geom)[1]


# --- plotCovDist -----------------------------------------------------------

test_that("plotCovDist returns a histogram ggplot by default", {
  p <- plotCovDist(indParams, covNames = "WT")
  expect_s3_class(p, "ggplot")
  expect_identical(geomClass(p), "GeomBar")          # geom_histogram -> GeomBar
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plotCovDist honours plotType = 'density'", {
  p <- plotCovDist(indParams, covNames = "WT", plotType = "density")
  expect_identical(geomClass(p), "GeomDensity")
})

test_that("plotCovDist facets several covariates and passes layout args", {
  p <- plotCovDist(indParams, covNames = c("WT", "AGE", "GENO2", "AST"),
                   scales = "free", ncol = 2, bins = 15)
  built <- ggplot2::ggplot_build(p)
  # one panel per covariate
  expect_equal(length(unique(built$data[[1]]$PANEL)), 4L)
  expect_equal(p$facet$params$ncol, 2L)
})

test_that("plotCovDist maps the two missingness levels to the given fills", {
  p <- plotCovDist(indParams, covNames = "WT",
                   fillNonMissing = "#111111", fillMissing = "#222222")
  fills <- unique(ggplot2::ggplot_build(p)$data[[1]]$fill)
  # setequal, not %in%: both levels must actually be rendered, so an all-zero
  # missingness flag or a dropped level fails rather than passing silently.
  expect_setequal(fills, c("#111111", "#222222"))
})

test_that("plotCovDist errors on a non-data-frame", {
  expect_error(plotCovDist(list(), covNames = "WT"),
               "must be a data frame")
})

test_that("plotCovDist errors when a requested covariate is absent", {
  expect_error(plotCovDist(indParams, covNames = c("WT", "NOPE")),
               "not in the data frame: NOPE")
})

test_that("plotCovDist warns and treats all as non-missing when flags are absent", {
  noFlags <- indParams[, !grepl("_MISSING$", names(indParams))]
  expect_warning(p <- plotCovDist(noFlags, covNames = "WT"),
                 "Missingness flags not found")
  lvls <- levels(ggplot2::ggplot_build(p)$plot$data$Missingness)
  expect_identical(lvls, c("Non-missing", "Missing"))
  # nothing flagged missing
  expect_true(all(ggplot2::ggplot_build(p)$plot$data$Missingness == "Non-missing"))
})


# --- plotEtasCov ---------------------------------------------------------------

test_that("plotEtasCov returns a ggplot with FREM + Prim facet rows by default", {
  p <- plotEtasCov(indParams, covName = "WT", etaNames = c("ETA3", "ETA4"))
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_identical(geomClass(p), "GeomPoint")
  # Type factor drives the facet rows: two requested
  expect_setequal(as.character(unique(built$plot$data$Type)),
                  c("FREM ETA", "ETA Prim"))
})

test_that("plotEtasCov can plot the EBE type and respects row order", {
  p <- plotEtasCov(indParams, covName = "WT", etaNames = c("ETA3", "ETA4"),
                   etaTypes = c("EBE", "FREM"),
                   typeLabels = c(EBE = "FFEM EBE", FREM = "FREM EBE"))
  lvls <- levels(ggplot2::ggplot_build(p)$plot$data$Type)
  expect_identical(lvls, c("FFEM EBE", "FREM EBE"))
})

test_that("plotEtasCov auto-detects ETA columns when etaNames is NULL", {
  p <- plotEtasCov(indParams, covName = "AGE", etaTypes = "FREM")
  etaLevels <- levels(ggplot2::ggplot_build(p)$plot$data$ETA)
  expect_setequal(etaLevels, paste0("ETA", 1:5))
})

test_that("plotEtasCov applies custom etaLabels", {
  p <- plotEtasCov(indParams, covName = "WT", etaNames = c("ETA3", "ETA4"),
                   etaTypes = "FREM", etaLabels = c("Clearance", "Volume"))
  expect_setequal(levels(ggplot2::ggplot_build(p)$plot$data$ETA),
                  c("Clearance", "Volume"))
})

test_that("plotEtasCov adds a missing-data smooth layer when asked", {
  pNo <- plotEtasCov(indParams, covName = "WT", etaNames = "ETA3",
                     etaTypes = "FREM")
  pYes <- plotEtasCov(indParams, covName = "WT", etaNames = "ETA3",
                      etaTypes = "FREM", showMissing = TRUE, smoothMissing = TRUE)
  # non-missing smooth only vs. non-missing + missing smooth
  expect_equal(length(pYes$layers), length(pNo$layers) + 1L)
})

test_that("plotEtasCov excludes missing subjects unless showMissing = TRUE", {
  pStrict <- plotEtasCov(indParams, covName = "WT", etaNames = "ETA3",
                         etaTypes = "FREM")
  pAll <- plotEtasCov(indParams, covName = "WT", etaNames = "ETA3",
                      etaTypes = "FREM", showMissing = TRUE)
  dStrict <- ggplot2::ggplot_build(pStrict)$plot$data
  dAll    <- ggplot2::ggplot_build(pAll)$plot$data

  # The default really drops the missing-covariate subjects ...
  expect_true(all(dStrict$Missingness == "Non-missing"))
  # ... and showMissing = TRUE really keeps more rows than that. A `>=` here
  # would hold by construction and would not notice the filter disappearing.
  expect_gt(nrow(dAll), nrow(dStrict))
  expect_true(any(dAll$Missingness == "Missing"))
})

test_that("plotEtasCov errors on a non-data-frame", {
  expect_error(plotEtasCov(1:10, covName = "WT"), "must be a data frame")
})

test_that("plotEtasCov errors when the covariate is missing", {
  expect_error(plotEtasCov(indParams, covName = "NOPE"),
               "'NOPE' not found")
})

test_that("plotEtasCov errors on an unknown requested ETA", {
  expect_error(plotEtasCov(indParams, covName = "WT", etaNames = "ETA99"),
               "not in the data frame: ETA99")
})

test_that("plotEtasCov errors on an etaLabels length mismatch", {
  expect_error(
    plotEtasCov(indParams, covName = "WT", etaNames = c("ETA3", "ETA4"),
                etaLabels = "only-one"),
    "same length as `etaNames`")
})

test_that("plotEtasCov errors when PRIM columns are absent for the Prim type", {
  noPrim <- indParams[, !grepl("_PRIM$", names(indParams))]
  expect_error(
    plotEtasCov(noPrim, covName = "WT", etaNames = "ETA3", etaTypes = "Prim"),
    "Missing PRIM columns")
})

test_that("plotEtasCov errors when EBE columns are absent for the EBE type", {
  noEbe <- indParams[, !grepl("^EBE_", names(indParams))]
  expect_error(
    plotEtasCov(noEbe, covName = "WT", etaNames = "ETA3", etaTypes = "EBE"),
    "Missing EBE columns")
})

test_that("plotEtasCov warns when a missingness flag is absent but showMissing = TRUE", {
  noFlag <- indParams[, names(indParams) != "WT_MISSING"]
  expect_warning(
    plotEtasCov(noFlag, covName = "WT", etaNames = "ETA3", etaTypes = "FREM",
                showMissing = TRUE),
    "flag 'WT_MISSING' not found")
})
