test_that("plotExplained variability works", {
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

  expect_snapshot(stabilize(dfCovs))

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

  expect_snapshot(stabilize(dfres0))

  # Create all the plot objects
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

  # Snapshot the data "blueprint" of each plot using the new 3-step pattern

  # p1
  plot_data_1 <- ggplot2::ggplot_build(p1)$data
  std_plot_data_1 <- standardize_plot_data(plot_data_1)
  expect_snapshot(stabilize(std_plot_data_1))

  # p1med
  plot_data_1med <- ggplot2::ggplot_build(p1med)$data
  std_plot_data_1med <- standardize_plot_data(plot_data_1med)
  expect_snapshot(stabilize(std_plot_data_1med))

  # p2
  plot_data_2 <- ggplot2::ggplot_build(p2)$data
  std_plot_data_2 <- standardize_plot_data(plot_data_2)
  expect_snapshot(stabilize(std_plot_data_2))

  # ... and so on for all plots
  plot_data_3 <- ggplot2::ggplot_build(p3)$data
  std_plot_data_3 <- standardize_plot_data(plot_data_3)
  expect_snapshot(stabilize(std_plot_data_3))

  plot_data_4 <- ggplot2::ggplot_build(p4)$data
  std_plot_data_4 <- standardize_plot_data(plot_data_4)
  expect_snapshot(stabilize(std_plot_data_4))

  plot_data_5 <- ggplot2::ggplot_build(p5)$data
  std_plot_data_5 <- standardize_plot_data(plot_data_5)
  expect_snapshot(stabilize(std_plot_data_5))

  plot_data_6 <- ggplot2::ggplot_build(p6)$data
  std_plot_data_6 <- standardize_plot_data(plot_data_6)
  expect_snapshot(stabilize(std_plot_data_6))

  plot_data_7 <- ggplot2::ggplot_build(p7)$data
  std_plot_data_7 <- standardize_plot_data(plot_data_7)
  expect_snapshot(stabilize(std_plot_data_7))

  plot_data_8 <- ggplot2::ggplot_build(p8)$data
  std_plot_data_8 <- standardize_plot_data(plot_data_8)
  expect_snapshot(stabilize(std_plot_data_8))

  plot_data_9 <- ggplot2::ggplot_build(p9)$data
  std_plot_data_9 <- standardize_plot_data(plot_data_9)
  expect_snapshot(stabilize(std_plot_data_9))

  plot_data_10 <- ggplot2::ggplot_build(p10)$data
  std_plot_data_10 <- standardize_plot_data(plot_data_10)
  expect_snapshot(stabilize(std_plot_data_10))

  plot_data_11 <- ggplot2::ggplot_build(p11)$data
  std_plot_data_11 <- standardize_plot_data(plot_data_11)
  expect_snapshot(stabilize(std_plot_data_11))


  ## Test some error conditions
  expect_error(plotExplainedVar(dfres0,maxVar=3))
  expect_error(plotExplainedVar(dfres0,maxVar=3))
  expect_error(plotExplainedVar(dfres0,parameterLabels = c("CL","V","FREL","KA")))
  expect_error(plotExplainedVar(dfres0,covariateLabels = "WT"))
  expect_error(plotExplainedVar(dfres0,parameters = c("CL","V","FREL")))
})
