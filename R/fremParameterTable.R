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
#' @param includeRSE Logical. Should the output include relative standard errors (RSE). Default is \code{FALSE}.
#' @param includeShrinkage Logical. If \code{TRUE}, calculates and includes shrinkages in the table using a specified FFEM model output. Defaults to \code{FALSE}.
#' @param shrinkageType Character string specifying which type of shrinkage to report. Options are \code{"ETA_Var"}, \code{"ETA_SD"}, \code{"EBV_Var"}, or \code{"EBV_SD"}. Defaults to \code{"ETA_SD"} (the legacy NONMEM ETA SD shrinkage).
#' @param rawShrinkage Logical. If \code{FALSE} (default), negative shrinkages are floored and reported as \code{1.0000e-10} to exactly mimic NONMEM's standard output. If \code{TRUE}, the raw (potentially negative) calculated shrinkages are reported.
#' @param ffemModName A character string specifying the model name of the FFEM run (typically a MAX=0 run) to be used exclusively for shrinkage calculations. Required if \code{includeShrinkage} is \code{TRUE}.
#' @param shkDigs An integer specifying the number of decimal places to use when formatting the shrinkage output. Default is 2.
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
#' * coefficientTable_wide: A document-ready wide-format data frame of the covariate coefficients (Covariates x Parameters). Estimates and uncertainties are separated into distinct columns.
#' * Samples: A data frame with the parameter vector samples used to derive the RSE information.
#' * Condition: The condition number for the variance-covariance matrix of the parameters.
#' 
#' @export
#' @seealso [PMXForest::getSamples()] [calcFFEM()] [calcFremShrinkage()]
#' 
#' @examples 
#' 
#'if (requireNamespace("kableExtra", quietly = TRUE)) {
#' library(kableExtra)
#' set.seed(123)
#' runno            <- 31
#' modName          <- "run31"
#' ffemModName      <- "run31max0"
#' modDevDir        <- system.file("extdata/SimNeb/",package = "PMXFrem")
#' numNonFREMThetas <- 7
#' numSkipOm        <- 2
#'
#' ## Generate the raw output
#' tmp <- fremParameterTable(
#'       runno            = runno,
#'       modName          = modName,
#'       modDevDir        = modDevDir,
#'       thetaNum         = 2:7,
#'       omegaNum         = c(1,3,4,5),
#'       sigmaNum         = 1,
#'       thetaLabels      = c("CL (L/h)","V (L)","MAT (h)","D1 (h)","Food on Frel","Food on MAT"),
#'       omegaLabels      = c("IIV on RUV","IIV on CL","IIV on V","IIV on MAT"),
#'       sigmaLabels      = c("RUV"),
#'       parNames         = c("CL", "V", "MAT"),
#'       includeRSE       = TRUE,
#'       includeShrinkage = TRUE,
#'       shrinkageType    = "ETA_SD",
#'       rawShrinkage     = FALSE,
#'       ffemModName      = ffemModName,
#'       shkDigs          = 2,
#'       uncertainty      = "CI",
#'       numNonFREMThetas = numNonFREMThetas,
#'       numSkipOm        = numSkipOm,
#'       availCov         = "all",
#'       quiet            = TRUE)
#'
#' ## Use kable_extra to generate a nice looking table of the main parameters
#' tmp$parameterTable %>%
#'     mutate(Estimate=as.character(signif(Estimate,3)))%>%
#'     kbl() %>%
#'     kable_classic(full_width = FALSE, html_font = "Cambria")
#'     
#' ## Use kable_extra to generate a nice looking table of the covariate coefficients
#' tmp$coefficientTable_wide %>%
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
    includeShrinkage = FALSE,
    shrinkageType    = "ETA_SD",
    rawShrinkage     = FALSE,
    ffemModName      = NULL,
    shkDigs          = 2,
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
    
    # Condition Number Logic
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
  
  if (includeRSE) {
    names(fremParRses) <- retList$parameterTable$Parameter
    retList$Samples <- fremParRses
  }
  
  ## ------------------------------------------------------------------------
  ## 6. Build Coefficient Tables
  ## ------------------------------------------------------------------------
  fmt_val     <- paste0("%#.", sigDigs, "g")
  fmt_base_ci <- paste0("[", fmt_val, " - ", fmt_val, "]")
  
  coeff_long <- expand.grid(Parameter = par_names_display, Covariate = cov_names_display, stringsAsFactors = FALSE)
  coeff_long$Estimate <- as.vector(point_coeff)
  
  if (includeRSE) {
    if (uncertainty == "RSE") {
      sampleMeans <- fremParRses %>% dplyr::summarise_all(mean)
      sampleSD    <- fremParRses %>% dplyr::summarise_all(sd)
      retList$parameterTable$`RSE (%)` <- signif(as.numeric(abs(100 * sampleSD / sampleMeans)), sigDigs)
      
      coeff_mean <- apply(coeff_samples, c(1, 2), mean, na.rm = TRUE)
      coeff_sd   <- apply(coeff_samples, c(1, 2), sd, na.rm = TRUE)
      coeff_long$SD <- signif(as.vector(coeff_sd), sigDigs)
      coeff_long$`RSE (%)` <- signif(as.vector(abs(100 * coeff_sd / coeff_mean)), sigDigs)
      
      coeff_long$Estimate_String <- sprintf(fmt_val, signif(coeff_long$Estimate, sigDigs))
      coeff_long$Uncertainty <- sprintf(paste0("(", fmt_val, "%%)"), coeff_long$`RSE (%)`)
      unc_label <- "RSE"
      
    } else if (uncertainty == "CI") {
      alpha <- 1 - ciLevel
      
      ci_lower <- as.numeric(apply(fremParRses, 2, quantile, probs = alpha / 2, na.rm = TRUE))
      ci_upper <- as.numeric(apply(fremParRses, 2, quantile, probs = 1 - (alpha / 2), na.rm = TRUE))
      ci_col_name <- sprintf("%g%% CI", ciLevel * 100)
      retList$parameterTable[[ci_col_name]] <- sprintf(fmt_base_ci, signif(ci_lower, sigDigs), signif(ci_upper, sigDigs))
      
      coeff_long$CI_Lower <- signif(as.vector(apply(coeff_samples, c(1, 2), quantile, probs = alpha / 2, na.rm = TRUE)), sigDigs)
      coeff_long$CI_Upper <- signif(as.vector(apply(coeff_samples, c(1, 2), quantile, probs = 1 - (alpha / 2), na.rm = TRUE)), sigDigs)
      
      coeff_long$Estimate_String <- sprintf(fmt_val, signif(coeff_long$Estimate, sigDigs))
      coeff_long$Uncertainty <- sprintf(fmt_base_ci, coeff_long$CI_Lower, coeff_long$CI_Upper)
      unc_label <- sprintf("%g%% CI", ciLevel * 100)
    }
    
    # Pivot into separated wide format
    coeff_wide <- coeff_long %>%
      dplyr::select(Covariate, Parameter, Estimate_String, Uncertainty) %>%
      tidyr::pivot_wider(
        names_from = Parameter, 
        values_from = c(Estimate_String, Uncertainty),
        names_glue = "{Parameter}_{.value}"
      )
    
    # Sort columns cleanly: Covariate, Par1, Par1 Unc, Par2, Par2 Unc...
    param_cols <- unlist(lapply(par_names_display, function(p) c(paste0(p, "_Estimate_String"), paste0(p, "_Uncertainty"))))
    coeff_wide <- coeff_wide[, c("Covariate", intersect(param_cols, names(coeff_wide)))]
    
    # Rename for final output table
    names(coeff_wide) <- gsub("_Estimate_String$", "", names(coeff_wide))
    names(coeff_wide) <- gsub("_Uncertainty$", paste0(" ", unc_label), names(coeff_wide))
    
  } else {
    # No uncertainty generated
    coeff_long$Estimate_String <- sprintf(fmt_val, signif(coeff_long$Estimate, sigDigs))
    
    coeff_wide <- coeff_long %>%
      dplyr::select(Covariate, Parameter, Estimate_String) %>%
      tidyr::pivot_wider(names_from = Parameter, values_from = Estimate_String)
  }
  
  retList$coefficientTable_long <- coeff_long
  retList$coefficientTable_wide <- coeff_wide
  
  ## ------------------------------------------------------------------------
  ## 7. Shrinkage Integration
  ## ------------------------------------------------------------------------
  if (includeShrinkage) {
    if (is.null(ffemModName)) {
      stop("ffemModName must be provided when includeShrinkage is TRUE.", call. = FALSE)
    }
    
    shk_df <- calcFremShrinkage(
      modName           = ffemModName, 
      modDevDir         = modDevDir, 
      dropUninformative = TRUE, 
      quiet             = quiet
    )
    
    if (!(shrinkageType %in% names(shk_df))) {
      stop(sprintf("Invalid shrinkageType '%s'. Valid options are: %s", 
                   shrinkageType, paste(names(shk_df)[-1], collapse = ", ")))
    }
    
    if (!rawShrinkage) {
      shk_df[[shrinkageType]][!is.na(shk_df[[shrinkageType]]) & shk_df[[shrinkageType]] < 0] <- 1.0000e-10
    }
    
    # Format to user-specified decimals or scientific notation for the floor
    shk_fmt <- paste0("%.", shkDigs, "f")
    shk_formatted <- sapply(shk_df[[shrinkageType]], function(x) {
      if (is.na(x)) return(NA_character_)
      if (x == 1e-10) return("1.0000e-10")
      return(sprintf(shk_fmt, x))
    })
    
    # Map the structural ETA numbers back to the parameterTable's row labels
    eta_names <- paste0("ETA", omegaNum)
    idx_om <- which(retList$parameterTable$Type == "OMEGA")
    
    shk_map <- data.frame(
      ETA_Name = eta_names,
      Shrinkage = NA_character_,
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(shk_map$ETA_Name)) {
      match_idx <- which(shk_df$Parameter == shk_map$ETA_Name[i])
      if (length(match_idx) > 0) {
        shk_map$Shrinkage[i] <- shk_formatted[match_idx]
      }
    }
    
    # Apply to main table
    retList$parameterTable$`Shrinkage (%)` <- "-"
    if(length(idx_om) == nrow(shk_map)) {
      retList$parameterTable$`Shrinkage (%)`[idx_om] <- ifelse(is.na(shk_map$Shrinkage), "-", shk_map$Shrinkage)
    }
  }
  
  return(retList)
}