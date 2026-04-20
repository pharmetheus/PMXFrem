### FILE: fremParameterTable.R ###

#' Generate the input to a parameter table for a FREM model
#'
#' The data for a parameter estimates table for a FREM model is assembled,
#' including the calculation of the appropriate omega prims given the covariates
#' to take into account. It also computes the covariate coefficients (Individual 
#' Covariate Effects) mapping each covariate to each structural parameter.
#'
#' @inheritParams getFileNames
#' @inheritParams createFFEMdata
#' @inheritParams calcFFEM
#' @inheritParams calcParameterEsts
#' @param thetaLabels A character vector with labels for the THETAs. Should be exactly as long as \code{thetaNum}. Note: This is used strictly for the base parameter table, which may include non-FREM structural parameters.
#' @param omegaLabels A vector with labels for the OMEGAs. Should be as long as `omegaNum`.
#' @param sigmaLabels A vector with labels for the SIGMAs Should be as long as `sigmaNum`.
#' @param parNames A character vector with labels for the structural parameters affected by covariates (which will become the rows in the coefficient table). Should be exactly as long as \code{numParCov}. 
#' If \code{NULL}, defaults to Par1, Par2, etc. Note: Do not confuse this with \code{thetaLabels}, as the number of structural ETAs (\code{numParCov}) often differs from the total \code{thetaNum}.
#' @param covLabels A character vector with labels for the covariates (columns in the coefficient table). Should match the length of `availCov`.
#' @param omegaSD Logical. If the omega prims should be reported on the SD scale (i.e. the square root of omega prim). Default is `TRUE`.
#' @param sigmaSD Logical. If the sigmas should be reported on the SD scale (i.e. the square root of sigma). Default is `TRUE`.
#' @param includeRSE Logical. Should the output include relative standard errors (RSE). Default is `FALSE`.
#' @param uncertainty A character string specifying how uncertainty should be summarized. Either \code{"RSE"} (default) or \code{"CI"}.
#' @param ciLevel A numeric value between 0 and 1 specifying the confidence interval level. Default is 0.90.
#' @param sigDigs An integer specifying the number of significant digits to use for formatting output estimates and uncertainties. Default is 3.
#' @param bsFile The name of a PsNbootstrap or sir file raw_results file. To be used for RSE calculations based on bootstrap output.
#' @param n The number of samples to use in the RSE calculations.
#' @param ... Additional arguments passed directly to \code{PMXForest::getSamples()} (e.g., \code{seed} for reproducibility).
#'
#' @return A list of five components:
#'
#' * parameterTable: A data frame with information for the base parameter table.
#' * coefficientTable_long: A tidy, long-format data frame of the covariate coefficients (optimal for internal reporting APIs).
#' * coefficientTable_wide: A document-ready wide-format data frame of the covariate coefficients (Covariates x Parameters).
#' * Samples: A data frame with the parameter vector samples used to derive the RSE information.
#' * Condition: The condition number for the variance-covariance matrix of the parameters.
#' 
#' @export
#' @seealso [PMXForest::getSamples()] [calcFFEM()]
#' 
#' @examples 
#' 
#'if (requireNamespace("kableExtra", quietly = TRUE)) {
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
#'       uncertainty = "CI",
#'       numNonFREMThetas = numNonFREMThetas,
#'       numSkipOm        = numSkipOm,
#'       availCov         = "all",
#'       quiet            = TRUE)
#'
#' ## Use kable_extra to generate a nice looking table
#' tmp$parameterTable %>%
#'     mutate(Estimate=as.character(signif(Estimate,3)))%>%
#'     kbl() %>%
#'     kable_classic(full_width = FALSE, html_font = "Cambria")
#' }
#' @family Diagnostics & Plotting
#' @concept diagnostics
fremParameterTable <- function(
    runno         = NULL,
    modDevDir     = NULL,
    numNonFREMThetas,
    numSkipOm     = 0,
    thetaNum,
    omegaNum,
    sigmaNum,
    thetaLabels   = paste0("THETA", thetaNum),
    omegaLabels   = paste0("OMEGA", omegaNum),
    sigmaLabels   = paste0("SIGMA", sigmaNum),
    parNames      = NULL,
    covLabels     = NULL,
    omegaSD       = TRUE,
    sigmaSD       = TRUE,
    includeRSE    = FALSE,
    uncertainty   = c("RSE", "CI"),
    ciLevel       = 0.90,
    sigDigs       = 3,
    bsFile        = NULL,
    n             = 175,
    availCov      = "all",
    dfext         = NULL,
    modName       = NULL,
    modExt        = ".mod",
    lstExt        = ".lst",
    quiet         = FALSE,
    seed          = NULL,
    ...) {
  
  ## 1. Input checks & File Resolution
  if(is.null(runno) & is.null(modName)) stop("Either runno or modName has to be specified")
  if(is.null(availCov)) stop("availCov must be part of the FREM model or 'all'")
  
  fileNames <- getFileNames(runno = runno, modName = modName, modDevDir = modDevDir)
  modFile   <- fileNames$mod
  extFile   <- fileNames$ext
  covFile   <- fileNames$cov
  
  if(!file.exists(modFile)) stop(paste("Can not find model file", modFile))
  if(!file.exists(extFile)) stop(paste("Can not find ext file", extFile))
  
  uncertainty <- match.arg(uncertainty)
  
  if(includeRSE) {
    rseFile <- ifelse(is.null(bsFile), covFile, bsFile)
    if(!file.exists(rseFile)) stop(paste("Can not find the file for RSE calculations:", rseFile))
  }
  
  if(length(thetaNum) != length(thetaLabels)) stop("The number of theta labels must be the same as the number of thetas in the parameter table")
  if(length(omegaNum) != length(omegaLabels)) stop("The number of omega labels must be the same as the number of omegas in the parameter table")
  if(length(sigmaNum) != length(sigmaLabels)) stop("The number of sigma labels must be the same as the number of sigmas in the parameter table")
  
  ## 2. Process Initial Estimates
  if(is.null(dfext)) {
    dfExt <- getExt(extFile)
  } else {
    dfExt <- dfext
  }
  extRes <- dfExt %>% dplyr::filter(ITERATION == -1000000000)
  
  ## 3. Sort out Covariates
  extractedCovNames <- getCovNames(modFile)$covNames
  if (length(availCov) == 1 && availCov == "all") availCov <- extractedCovNames
  if(any(!(availCov %in% extractedCovNames))) stop("availCov must be part of the FREM model or 'all'")
  
  if (!is.null(covLabels) && length(covLabels) != length(availCov)) {
    stop("covLabels must have the same length as the evaluated covariates (availCov).")
  }
  cov_names_display <- if (!is.null(covLabels)) covLabels else availCov
  
  ## 4. Compute Point Estimates (Base & Coefficients)
  fremParEsts <- calcParameterEsts(extRes, thetaNum, omegaNum, sigmaNum, numNonFREMThetas, numSkipOm,
                                   covNames = extractedCovNames, availCov = availCov, quiet = quiet)
  
  ffem_point <- calcFFEM(dfext = extRes, numNonFREMThetas = numNonFREMThetas, numSkipOm = numSkipOm, 
                         covNames = extractedCovNames, availCov = availCov, quiet = TRUE)
  
  point_coeff <- as.matrix(ffem_point$Coefficients)
  # Handle potential dimension drop if only 1 covariate exists
  if (ncol(point_coeff) != length(availCov)) point_coeff <- t(point_coeff) 
  
  if (!is.null(parNames) && length(parNames) != nrow(point_coeff)) {
    stop("parNames must match the number of structural parameters affected by covariates.")
  }
  par_names_display <- if (!is.null(parNames)) parNames else paste0("Par", 1:nrow(point_coeff))
  
  rownames(point_coeff) <- par_names_display
  colnames(point_coeff) <- cov_names_display
  
  ## Assemble Output Base
  retList <- list()
  retList$parameterTable <- data.frame(
    Type      = c(rep("THETA", length(thetaLabels)), rep("OMEGA", length(omegaLabels)), rep("SIGMA", length(sigmaLabels))),
    Parameter = c(thetaLabels, omegaLabels, sigmaLabels),
    Estimate  = fremParEsts
  )
  retList$Samples <- data.frame()
  
  ## 5. RSE / Uncertainty Engine
  if(includeRSE) {
    if(!is.null(seed)) set.seed(seed) 
    
    # Capture arguments in ... for getSamples
    dfSamplesBS <- PMXForest::getSamples(rseFile, extFile = extFile, n = n, ...)
    dfSamplesBS <- cbind(ITER = 1, dfSamplesBS)
    
    # Pre-allocate containers
    fremParRses <- data.frame(matrix(NA, nrow = nrow(dfSamplesBS), ncol = length(omegaNum) + length(thetaNum) + length(sigmaNum)))
    coeff_samples <- array(NA, dim = c(nrow(point_coeff), ncol(point_coeff), nrow(dfSamplesBS)))
    
    for(i in 1:nrow(dfSamplesBS)) {
      # Sample Base Parameters
      fremParRses[i, ] <- calcParameterEsts(dfSamplesBS[i, ], thetaNum, omegaNum, sigmaNum, numNonFREMThetas, numSkipOm,
                                            covNames = extractedCovNames, availCov = availCov, quiet = TRUE)
      
      # Sample Coefficients
      tmp_ffem <- calcFFEM(dfext = dfSamplesBS[i, , drop = FALSE], numNonFREMThetas = numNonFREMThetas, 
                           numSkipOm = numSkipOm, covNames = extractedCovNames, availCov = availCov, quiet = TRUE)
      c_mat <- as.matrix(tmp_ffem$Coefficients)
      if (ncol(c_mat) != ncol(point_coeff)) c_mat <- t(c_mat)
      coeff_samples[,,i] <- c_mat
    }
    
    names(fremParRses) <- retList$parameterTable$Parameter
    
    # Condition Number Logic (Robust)
    col_sds <- sapply(fremParRses, sd, na.rm = TRUE)
    fremParRses_for_cor <- fremParRses[, col_sds > 1e-9, drop = FALSE]
    if (ncol(fremParRses_for_cor) > 1) {
      correlation_matrix <- suppressWarnings(cor(fremParRses_for_cor))
      if (anyNA(correlation_matrix) || any(is.infinite(correlation_matrix))) {
        retList$Condition <- NA
      } else {
        eigen_values <- eigen(correlation_matrix)$values
        retList$Condition <- max(eigen_values) / min(eigen_values)
      }
    } else {
      retList$Condition <- NA
    }
  }
  
  ## 5.5 SD Scale Transformations
  # Apply the transformation to the base estimates, and to the samples if they exist,
  # BEFORE Section 6 applies formatting and extracts the CIs/RSEs.
  if (omegaSD) {
    idx_om <- which(retList$parameterTable$Type == "OMEGA")
    retList$parameterTable$Estimate[idx_om] <- sqrt(retList$parameterTable$Estimate[idx_om])
    retList$parameterTable$Parameter[idx_om] <- paste(retList$parameterTable$Parameter[idx_om], "(SD)")
    if (includeRSE) fremParRses[, idx_om] <- sqrt(fremParRses[, idx_om])
  }
  
  if (sigmaSD) {
    idx_sig <- which(retList$parameterTable$Type == "SIGMA")
    retList$parameterTable$Estimate[idx_sig] <- sqrt(retList$parameterTable$Estimate[idx_sig])
    retList$parameterTable$Parameter[idx_sig] <- paste(retList$parameterTable$Parameter[idx_sig], "(SD)")
    if (includeRSE) fremParRses[, idx_sig] <- sqrt(fremParRses[, idx_sig])
  }
  
  # Update names and the final exported samples object to reflect the SD transformations
  if (includeRSE) {
    names(fremParRses) <- retList$parameterTable$Parameter
    retList$Samples <- fremParRses
  }
  
  ## 6. Build Coefficient Tables
  # Dynamic formatting strings based on sigDigs (using %# to preserve trailing zeros)
  fmt_val     <- paste0("%#.", sigDigs, "g")
  fmt_rse     <- paste0(fmt_val, " (", fmt_val, "%%)")
  fmt_ci      <- paste0(fmt_val, " [", fmt_val, " - ", fmt_val, "]")
  fmt_base_ci <- paste0("[", fmt_val, " - ", fmt_val, "]")
  
  # Initialize Long format (Tidy)
  coeff_long <- expand.grid(Parameter = par_names_display, Covariate = cov_names_display, stringsAsFactors = FALSE)
  
  # Apply signif to point estimates in numeric columns
  coeff_long$Estimate <- signif(as.vector(point_coeff), sigDigs)
  retList$parameterTable$Estimate <- signif(retList$parameterTable$Estimate, sigDigs)
  
  if (includeRSE) {
    if (uncertainty == "RSE") {
      # Base Table RSE
      sampleMeans <- fremParRses %>% dplyr::summarise_all(mean)
      sampleSD    <- fremParRses %>% dplyr::summarise_all(sd)
      retList$parameterTable$`RSE (%)` <- signif(as.numeric(abs(100 * sampleSD / sampleMeans)), sigDigs)
      
      # Coeff Table RSE
      coeff_mean <- apply(coeff_samples, c(1, 2), mean, na.rm = TRUE)
      coeff_sd   <- apply(coeff_samples, c(1, 2), sd, na.rm = TRUE)
      coeff_long$SD <- signif(as.vector(coeff_sd), sigDigs)
      coeff_long$`RSE (%)` <- signif(as.vector(abs(100 * coeff_sd / coeff_mean)), sigDigs)
      
      # String Formatting
      coeff_long$`Estimate_Formatted` <- sprintf(fmt_rse, coeff_long$Estimate, coeff_long$`RSE (%)`)
      
    } else if (uncertainty == "CI") {
      alpha <- 1 - ciLevel
      
      # Base Table CI
      ci_lower <- as.numeric(apply(fremParRses, 2, quantile, probs = alpha / 2, na.rm = TRUE))
      ci_upper <- as.numeric(apply(fremParRses, 2, quantile, probs = 1 - (alpha / 2), na.rm = TRUE))
      ci_col_name <- sprintf("%g%% CI", ciLevel * 100)
      retList$parameterTable[[ci_col_name]] <- sprintf(fmt_base_ci, ci_lower, ci_upper)
      
      # Coeff Table CI
      coeff_long$CI_Lower <- signif(as.vector(apply(coeff_samples, c(1, 2), quantile, probs = alpha / 2, na.rm = TRUE)), sigDigs)
      coeff_long$CI_Upper <- signif(as.vector(apply(coeff_samples, c(1, 2), quantile, probs = 1 - (alpha / 2), na.rm = TRUE)), sigDigs)
      
      # String Formatting
      coeff_long$`Estimate_Formatted` <- sprintf(fmt_ci, coeff_long$Estimate, coeff_long$CI_Lower, coeff_long$CI_Upper)
    }
  } else {
    # Fast path (No uncertainty)
    coeff_long$`Estimate_Formatted` <- sprintf(fmt_val, coeff_long$Estimate)
  }
  
  # Wide format (Reporting)
  coeff_wide <- coeff_long %>%
    dplyr::select(Covariate, Parameter, Estimate_Formatted) %>%
    tidyr::pivot_wider(names_from = Parameter, values_from = Estimate_Formatted)
  
  retList$coefficientTable_long <- coeff_long
  retList$coefficientTable_wide <- coeff_wide
  
  return(retList)
}