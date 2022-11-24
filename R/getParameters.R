getParameters <- function(runno = NULL,
                          modName = NULL,
                          modDevDir = ".",
                          numNonFREMEtas,
                          #numNonFREMThetas,
                          #numSkipOm = 0,
                          #numParCov = NULL,
                          #numFREMThetas = length(grep("THETA",names(dfext)))-numNonFREMThetas,
                          availCov      = covNames,
                          #quiet         = FALSE,
                          #fremETA       = NULL,
                          ...) {

  fileNames <- getFileNames(runno,modName,modDevDir)

  covNames <- getCovNames(fileNames$mod)$orgCovNames

  phis <- getPhi(fileNames$phi)[,2:(numNonFREMEtas+2)]


  dfExt <- getExt(fileNames$ext)

  # browser()
  #
  # calcFFEMout <- calcFFEM(dfExt,fremETA = TRUE,quiet=TRUE,numSkipOm = 2,...)
  #





  # return(phis)


}

