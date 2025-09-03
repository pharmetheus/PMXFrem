test_that("plotExplainedVar generates correct plot data", {

  # No need for vdiffr or RNGversion anymore.
  # Our stabilize() and standardize_plot_data() helpers are sufficient.

  set.seed(2342)
  modDevDir <- system.file("extdata/SimNeb",package="PMXFrem")
  fremRunno <- 31
  modFile   <- file.path(modDevDir,paste0("run",fremRunno,".mod"))
  covNames  <- getCovNames(modFile = modFile)

  ## Set up dfCovs
  dfData <- read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")) %>%
    dplyr::filter(BLQ == 0) %>%
    dplyr::distinct(ID,.keep_all = T)

  dfCovs <- dfData  %>%
    dplyr::select(covNames$orgCovNames) %>%
    dplyr::mutate_all(function(x) return(1)) %>%
    dplyr::slice(rep(1,ncol(.)+1))

  for(i in 2:nrow(dfCovs)) {
    dfCovs[i, names(dfCovs) != names(dfCovs)[i-1]] <- -99
  }

  cstrCovariates <- c("All",names(dfCovs))

  ## The parameter function list
  functionList2 <- list(
    function(basethetas,covthetas, dfrow, etas, ...){ return(basethetas[2]*exp(covthetas[1] + etas[3]))},
    function(basethetas,covthetas, dfrow, etas, ...){ return(basethetas[3]*exp(covthetas[2] + etas[4]))},
    function(basethetas,covthetas, dfrow, etas, ...){ return(basethetas[4]*exp(covthetas[3] + etas[5]))}
  )
  functionListName2 <- c("CL","V","MAT")

  dfres0 <- getExplainedVar(type             = 0,
                            data             = NULL,
                            dfCovs           = dfCovs,
                            numNonFREMThetas = 7,
                            numSkipOm        = 2,
                            functionList     = functionList2,
                            functionListName = functionListName2,
                            cstrCovariates   = cstrCovariates,
                            modDevDir        = modDevDir,
                            runno            = fremRunno,
                            ncores           = 1,
                            quiet            = TRUE,
                            seed             = 123
  )

  # --- Create all the plot objects ---
  p1    <- plotExplainedVar(dfres0)
  p1med <- plotExplainedVar(dfres0, reordFun = "median")
  p2    <- plotExplainedVar(dfres0, maxVar=2)
  p3    <- plotExplainedVar(dfres0, maxVar=2, parameters = "CL")
  covLabels <- c("All covariates","Age","ALT","AST","Bilirubin","BMI","BSA","Createnine clearance","Ethnicity","Genotype","Height","Lean body weight","NCI","Race","Sex","Smoking","Bodyweight")
  p4 <- plotExplainedVar(dfres0, covariateLabels = covLabels)
  p5 <- plotExplainedVar(dfres0, parameters="CL", parameterLabels = "Cleareance[2]", labelfun = label_parsed)
  p6 <- plotExplainedVar(dfres0, parameters = c("CL","V"))
  p7 <- plotExplainedVar(dfres0, parameters = c("V","CL"))
  p8 <- plotExplainedVar(dfres0, parameters = c("MAT"))
  p9 <- plotExplainedVar(dfres0, parameters = c("CL","V"), parameterLabels = c("Clearance","Volume"))
  p10 <- plotExplainedVar(dfres0, parameters = c("V","CL"), parameterLabels = c("Volume","Clearance"))
  p11 <- plotExplainedVar(dfres0, parameters = c("MAT"), parameterLabels="Mean absorption time")

  # --- Helper function to snapshot the plot data ---
  snapshot_plot_data <- function(p) {
    built_data <- ggplot2::ggplot_build(p)$data
    std_data <- standardize_plot_data(built_data)
    stable_data <- stabilize(std_data)
    expect_snapshot_value(stable_data, style = "serialize")
  }

  # --- Snapshot the data blueprint of each plot ---
  snapshot_plot_data(p1)
  snapshot_plot_data(p1med)
  snapshot_plot_data(p2)
  snapshot_plot_data(p3)
  snapshot_plot_data(p4)
  snapshot_plot_data(p5)
  snapshot_plot_data(p6)
  snapshot_plot_data(p7)
  snapshot_plot_data(p8)
  snapshot_plot_data(p9)
  snapshot_plot_data(p10)
  snapshot_plot_data(p11)

  ## Test some error conditions
  expect_error(plotExplainedVar(dfres0,maxVar=3))
  expect_error(plotExplainedVar(dfres0,parameterLabels = c("CL","V","FREL","KA")))
  expect_error(plotExplainedVar(dfres0,covariateLabels = "WT"))
  expect_error(plotExplainedVar(dfres0,parameters = c("CL","V","FREL")))
})
