test_that("plotExplained variability works", {

  set.seed(2342)
  PMXRenv::library.unqualified("vdiffr")
  modDevDir <- system.file("extdata/SimNeb",package="PMXFrem")
  fremRunno <- 31
  modFile   <- file.path(modDevDir,paste0("run",fremRunno,".mod"))
  covNames  <- getCovNames(modFile = modFile)

  ## Set up dfCovs
  dfData <- read.csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")) %>%
    filter(BLQ == 0) %>%
    distinct(ID,.keep_all = T)

  dfCovs <- dfData  %>%
    select(covNames$orgCovNames) %>%
    mutate_all(function(x) return(1)) %>%
    slice(rep(1,ncol(.)+1))

  for(i in 2:nrow(dfCovs)) {
    dfCovs[i, names(dfCovs) != names(dfCovs)[i-1]] <- -99
  }

  expect_snapshot(dfCovs)

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

  expect_snapshot(dfres0)

  svg() # Start a device to make plots cosistent between different ways of running the tests
  p1 <- plotExplainedVar(dfres0)
  p2 <- plotExplainedVar(dfres0,maxVar=2)
  p3 <- plotExplainedVar(dfres0,maxVar=2,parameters = "CL")

  covLabels <- c("All covariates","Age","ALT","AST","Bilirubin","BMI","BSA","Createnine clearance","Ethnicity","Genotype","Height","Lean body weight","NCI","Race","Sex","Smoking","Bodyweight")
  p4 <- plotExplainedVar(dfres0,covariateLabels = covLabels)

  p5 <- plotExplainedVar(dfres0,parameters="CL",parameterLabels = "Cleareance[2]",labelfun = label_parsed)
  dev.off()

  vdiffr::expect_doppelganger("Explained variability plot with default options", p1)
  vdiffr::expect_doppelganger("Explained variability plot with type=2", p2)
  vdiffr::expect_doppelganger("Explained variability plot with type=2 and only CL", p3)
  vdiffr::expect_doppelganger("Explained variability plot with covariateLabels", p4)
  vdiffr::expect_doppelganger("Explained variability plot with labeller", p5)

  ## Test some error conditions
  expect_error(plotExplainedVar(dfres0,maxVar=3))
  expect_error(plotExplainedVar(dfres0,maxVar=3))
  expect_error(plotExplainedVar(dfres0,parameterLabels = c("CL","V","FREL","KA")))
  expect_error(plotExplainedVar(dfres0,covariateLabels = "WT"))
  expect_error(plotExplainedVar(dfres0,parameters = c("CL","V","FREL")))

  ## Test the selection of parameters and order
  p6 <- plotExplainedVar(dfres0,parameters = c("CL","V"))
  p7 <- plotExplainedVar(dfres0,parameters = c("V","CL"))
  p8 <- plotExplainedVar(dfres0,parameters = c("MAT"))
  p9 <- plotExplainedVar(dfres0,parameters = c("CL","V"),parameterLabels = c("Clearance","Volume"))
  p10 <- plotExplainedVar(dfres0,parameters = c("V","CL"),parameterLabels = c("Volume","Clearance"))
  p11 <- plotExplainedVar(dfres0,parameters = c("MAT"),parameterLabels="Mean absorption time")

  vdiffr::expect_doppelganger("Explained variability plot with CL and V", p6)
  vdiffr::expect_doppelganger("Explained variability plot with V and C", p7)
  vdiffr::expect_doppelganger("Explained variability plot with MAT", p8)
  vdiffr::expect_doppelganger("Explained variability plot with CL and V+parameter labels", p9)
  vdiffr::expect_doppelganger("Explained variability plot with V and C+parameter labels", p10)
  vdiffr::expect_doppelganger("Explained variability plot with MAT+parameter labels", p11)
})
