#' Generate the input to a parameter table for a FREM model
#'
#' The data for a parameter estimates table for a FREM model is assembled,
#' including the calculation of the appropriate omega prims given the covariates
#' to take into account.
#'
#' A FREM model often generates a multitude of parameter estimates (one for each
#' parameter and covariate combination) and it is typically not very informative
#' to tabulate these estimates. Forest plots or explained variability plots are
#' better at communicating the impact of the covariates. However, it is of
#' interest to see what impact the covariates had on the parameters in the base
#' model. The `fremParameterTable()` function facilitates this by assembling a
#' data frame with the estimates of the base model THETAs, the omega prims and
#' sigmas. There are options to not include all parameters in the output.
#'
#' The derivation of the omega prims is ultimately done by the `calcFFEM()`
#' function. Since the omega prims are dependent on the covariates included in
#' the calculations, the `availCov` argument allows the user to specify a subset
#' of the available covariates. By default all covariates in the FREM model is
#' used in the calculations.
#'
#' Relative standard errors (RSEs) can be included in the output by setting
#' `includeRSE` to `TRUE`. Although NONMEM can provide standard errors for all
#' parameters in the model, the standard errors of the omega prims require a
#' numerical derivation, and the `fremParameterTable()` can do this bases on
#' either a `.cov` from NONMEM or PsN bootstrap or sir raw_results file.
#'
#' In case `bsfile` is `NULL`when `includeRSE` is `TRUE` a
#' .cov file that match the `runno` will be used as input.  `n` parameter vectors are sampled
#' multivariately from the variance-covariance matrix. For each sample, the
#' corresponding THETA, omega prim and sigma are derived (based on `thetaNum`,
#' `omegaNum` and `sigmaNum`). The RSEs are calculated as the ratio of standard
#' deviation of the derived parameter to their mean.
#'
#' In case a .csv file is provided as input and `n`=NULL, the input is assumed
#' to be a sir or bootstrap file. In case of a sir file, the RSEs will be
#' derived using all samples in the .csv file. In case it is a bootstrap file,
#' the RSEs will be derived using all samples with a nonzero value in the ofv
#' column will be used.
#'
#' In case a .csv file is provided as input and n is not NULL, it is assumed
#' that the raw results come from a boostrap analysis. A variance-covariance
#' matrix will be created from the samples with a non-zero value in the ofv
#' column and the RSEs will be derived based on `n` multivariately sampled
#' parameter vectors from this variance-covariance matrix.
#'
#' The actual sampling is done by the `PMXForest::getSamples()` function.
#'
#' The `Condition` number that is included in the returned list when
#' `includeRSE`is `TRUE`, is calculated from the `Samples` data frame also
#' included in the returned list:
#'
#' `Condition <- max(eigen(cor(Samples))$values)/min(eigen(cor(Samples))$values)`
#'
#' The `parameterTable` data frame can be used to generate nice looking
#' parameter tables for inclusion in reports and paper. The examples section
#' provides a simple example.
#'
#' @inheritParams getFileNames
#' @inheritParams createFFEMdata
#' @inheritParams calcFFEM
#' @inheritParams calcParameterEsts
#' @param thetaLabels A vector with labels for the THETAs. Should be as long as
#'   `thetaNum`.
#' @param omegaLabels A vector with labels for the OMEGAs. Should be as long as
#'   `omegaNum`.
#' @param sigmaLabels A vector with labels for the SIGMAs Should be as long as
#'   `sigmaNum`.
#' @param omegaSD Logical. If the omega prims should be reported on the SD scale
#'   (i.e. i.e. the square root of omega prim). Default is `TRUE`.
#' @param sigmaSD Logical. If the sigmas should be reported on the SD scale
#'   (i.e. i.e. the square root of sigma). Default is `TRUE`.
#' @param includeRSE Logical. Should the output include relative standard errors
#'   (RSE). Default is `FALSE`. See Details.
#' @param bsFile The name of a PsNbootstrap or sir file raw_results file. To be
#'   used for RSE calculations based on bootstrap output. See Details.
#' @param n The number of samples to use in the RSE calculations. See Details.
#'
#' @return A list of three components:
#'
#' * parameterTable: Is a data frame with information for the parameter table.
#' * Samples: A data frame with the parameter vector samples used to derive the RSE information (see Details).
#' * Condition: The condition number for variance-covariance matrix of the parameters in the parameter table.
#' @export
#'
#' @seealso [PMXForest::getSamples()] [calcFFEM()]
#'
#' @examples
#'
#' set.seed(123)
#' runno            <- 31
#' modDevDir        <- system.file("extdata/SimNeb/",package = "PMXFrem")
#' numNonFREMThetas <- 7
#' numSkipOm        <- 2
#'
#' ## Generate the raw outout
#' tmp <- fremParameterTable(
#'       runno           = runno,
#'       modDevDir        = modDevDir,
#'       thetaNum         = 2:7,
#'       omegaNum         = c(1,3,4,5),
#'       sigmaNum         = 1,
#'       thetaLabels      = c("CL (L/h)","V (L)","MAT (h)","D1 (h)","Food on Frel","Food on MAT"),
#'       omegaLabels      = c("IIV on RUV","IIV on CL","IIV on V","IIV on MAT"),
#'       sigmaLabels      = c("RUV"),
#'       includeRSE       = TRUE,
#'       numNonFREMThetas = numNonFREMThetas,
#'       numSkipOm        = numSkipOm,
#'       availCov         = "all",
#'       quiet            = TRUE)
#'
#' ## Use kable_extra to generate a nice looking table
#' tmp$parameterTable %>%
#'     mutate(Estimate=as.character(signif(Estimate,3)))%>%
#'     kbl() %>%
#'     kable_classic(full_width = F, html_font = "Cambria")
fremParameterTable <- function(
    runno         = NULL,
    modDevDir     = NULL,
    numNonFREMThetas,
    numSkipOm     = 0,
    thetaNum,
    omegaNum,
    sigmaNum,
    thetaLabels   = paste0("THETA",thetaNum),
    omegaLabels   = paste0("OMEGA",omegaNum),
    sigmaLabels   = paste0("SIGMA",sigmaNum),
    omegaSD       = TRUE,
    sigmaSD       = TRUE,
    includeRSE    = FALSE,
    bsFile        = NULL,
    n             = 175,
    availCov      = "all",
    dfext         = NULL,
    modName       = NULL,
    modExt        = ".mod",
    lstExt        = ".lst",
    quiet         = FALSE) {

  ## Input checks
  if(is.null(runno) & is.null(modName)) stop("Either runno or modName has to be specified")
  if(is.null(availCov)) stop("availCov must be part of the FREM model or 'all'")

  fileNames <- getFileNames(runno = runno, modName = modName, modDevDir = modDevDir)
  modFile   <- fileNames$mod
  extFile   <- fileNames$ext
  covFile   <- fileNames$cov

  if(!file.exists(modFile)) stop(paste("Can not find model file",modFile))
  if(!file.exists(extFile)) stop(paste("Can not find ext file",extFile))

  if(includeRSE) {
    rseFile <- ifelse(is.null(bsFile),covFile,bsFile)
    if(!file.exists(rseFile)) stop(paste("Can not find the file for RSE calculations:",rseFile))
  }

  if(length(thetaNum) != length(thetaLabels))
    stop("The number of theta labels must be the same as the number of thetas in the parameter table")
  if(length(omegaNum) != length(omegaLabels))
    stop("The number of omega labels must be the same as the number of omegas in the parameter table")
  if(length(sigmaNum) != length(sigmaLabels))
    stop("The number of sigma labels must be the same as the number of sigmas in the parameter table")

  ## Process input
  if(is.null(dfext)) {
    dfExt <- getExt(extFile)
  } else {
    dfExt <- dfext
  }
  extRes <- dfExt %>% dplyr::filter(ITERATION == -1000000000)

  ## Sort out the covariates to use
  covNames <- getCovNames(modFile)$covNames
  if (length(availCov) == 1 && availCov == "all") availCov <- getCovNames(modFile)$covNames

  if(any(!(availCov %in% covNames))) stop("availCov must be part of the FREM model or 'all'")

  ## Compute the parameter estimates
  fremParEsts <- calcParameterEsts(extRes,thetaNum,omegaNum,sigmaNum,numNonFREMThetas,numSkipOm,
                                   covNames=covNames, availCov=availCov,quiet=quiet)

  ## Assemble the output
  retList <- list()
  retList$parameterTable <- data.frame(Type=c(rep("THETA",length(thetaLabels)),
                                              rep("OMEGA",length(omegaLabels)),
                                              rep("SIGMA",length(sigmaLabels))),
                                       Parameter=c(thetaLabels,omegaLabels,sigmaLabels),
                                       Estimate=fremParEsts)
  retList$Samples <- data.frame()

  ## Add the RSEs if requested
  if(includeRSE) {
    if(is.null(rseFile)) stop("You need to set rseFile to either a .cov file or a bootstrap raw results file")
    dfSamplesBS <- PMXForest::getSamples(rseFile,extFile=extFile,n=n)
    dfSamplesBS <- cbind(ITER=1,dfSamplesBS)
    fremParRses <- data.frame(matrix(rep(NA,nrow(dfSamplesBS)*(length(omegaNum)+length(thetaNum)+length(sigmaNum))),
                                     ncol = length(omegaNum)+length(thetaNum)+length(sigmaNum)))
    for(i in (1:nrow(dfSamplesBS))) {
      fremParRses[i,] <- calcParameterEsts(dfSamplesBS[i,],thetaNum,omegaNum,sigmaNum,numNonFREMThetas,numSkipOm,
                                           covNames=covNames, availCov=availCov,quiet=quiet)
    }
    names(fremParRses) <- retList$parameterTable$Parameter
    sampleMeans <- fremParRses %>% dplyr::summarise_all(mean)
    sampleSD    <- fremParRses %>% dplyr::summarise_all(sd)
    sampleRSE   <- abs(100*sampleSD/sampleMeans)
    retList$parameterTable$`RSE (%)` <- as.numeric(sampleRSE)

    # --- Start of Robustness Fix for Condition Number ---
    # Check for columns with zero variance, as they make cor() produce NA/NaN
    # and cause eigen() to fail.
    col_sds <- sapply(fremParRses, sd, na.rm = TRUE)
    fremParRses_for_cor <- fremParRses[, col_sds > 1e-9, drop = FALSE]

    # Only compute condition number if there's more than one varying parameter
    if (ncol(fremParRses_for_cor) > 1) {
      correlation_matrix <- suppressWarnings(cor(fremParRses_for_cor))

      if (anyNA(correlation_matrix) || any(is.infinite(correlation_matrix))) {
        retList$Condition <- NA
        warning("Could not compute condition number due to issues in the correlation matrix.")
      } else {
        eigen_values <- eigen(correlation_matrix)$values
        retList$Condition <- max(eigen_values) / min(eigen_values)
      }
    } else {
      retList$Condition <- NA # Not enough varying parameters to compute
    }
    # --- End of Robustness Fix ---

    retList$Samples <- fremParRses
  }

  ## Final processing of the results
  if(omegaSD) {
    retList$parameterTable <-
      retList$parameterTable %>%
      dplyr::mutate(Estimate=ifelse(Type=="OMEGA",Estimate^0.5,Estimate)) %>%
      dplyr::mutate(Parameter=ifelse(Type=="OMEGA",paste(Parameter,"(SD)"),Parameter))
  }

  if(sigmaSD) {
    retList$parameterTable <-
      retList$parameterTable %>%
      dplyr::mutate(Estimate=ifelse(Type=="SIGMA",Estimate^0.5,Estimate)) %>%
      dplyr::mutate(Parameter=ifelse(Type=="SIGMA",paste(Parameter,"(SD)"),Parameter))
  }

  return(retList)
}
