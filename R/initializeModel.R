#' Initialize Model Parameters from FREM Output
#'
#' Extracts initial model parameter information either from a NONMEM .ext file
#' or by parsing a .mod file. This function serves as the first step in the
#' model update process, gathering the necessary parameter vectors and matrices.
#'
#' @param strFREMModel File name of the FREM-model.
#' @param numNonFREMThetas The number of non-FREM THETAs in the model.
#' @param numSkipOm The number of OMEGA elements to skip.
#' @param numParCov The number of parameters related to covariates. If NULL, the
#'   function will attempt to calculate this from the .ext file. It is required
#'   if no .ext file is available.
#'
#' @return A list (a "model state object") containing the initial parameter
#'   information:
#'   \item{theta}{A numeric vector of THETA values.}
#'   \item{thetaFix}{A numeric vector indicating if a THETA is fixed (1) or not (0).}
#'   \item{omegaMatrix}{The full variance-covariance OMEGA matrix.}
#'   \item{numTheta}{The total number of THETAs.}
#'   \item{numOmega}{The dimension of the OMEGA matrix (number of ETAs).}
#'   \item{numParCov}{The number of parameters for covariates, either passed in or calculated.}
#' @family FREM model management Internal
#' @concept frem_model_management
#' @keywords internal
initializeModelParameters <- function(strFREMModel,
                                      numNonFREMThetas,
                                      numSkipOm,
                                      numParCov) {
  
  numTheta <- -1
  numOmega <- -1
  extFile <- paste0(tools::file_path_sans_ext(strFREMModel), ".ext")
  
  if (file.exists(extFile)) {
    # 1. Read the full .ext file data
    ext_data <- getExt(extFile = extFile)
    
    # 2. Force strict numeric coercion to bypass factor/character parsing vulnerabilities
    iter_num <- suppressWarnings(as.numeric(as.character(ext_data$ITERATION)))
    
    dfext    <- ext_data[!is.na(iter_num) & iter_num == -1000000000, , drop = FALSE]
    dfextfix <- ext_data[!is.na(iter_num) & iter_num == -1000000006, , drop = FALSE]
    
    numTheta <- length(names(dfext)[regexpr("THETA.*", names(dfext)) == 1])
    numOmegaElements <- length(names(dfext)[regexpr("OMEGA.*", names(dfext)) == 1])
    numOmega <- -1 / 2 + sqrt(1 / 4 + 2 * numOmegaElements)
    
    # 3. unlist() ensures safe flattening of data.frames before numeric coercion
    THETA    <- as.numeric(unlist(dfext[, names(dfext)[regexpr("THETA.*", names(dfext)) == 1]]))
    
    if (nrow(dfextfix) > 0) {
      THETAFIX <- as.numeric(unlist(dfextfix[, names(dfextfix)[regexpr("THETA.*", names(dfextfix)) == 1]]))
    } else {
      THETAFIX <- rep(0, numTheta)
    }
    
    # Defensive fallback if the fixed vector length mismatches
    if (length(THETAFIX) == 0 || length(THETAFIX) != numTheta) {
      THETAFIX <- rep(0, numTheta)
    }
    
    OMEGA    <- as.numeric(unlist(dfext[, names(dfext)[regexpr("OMEGA.*", names(dfext)) == 1]]))
    
    OM                              <- matrix(0, nrow = numOmega, ncol = numOmega)
    OM[upper.tri(OM, diag = TRUE)]  <- OMEGA
    tOM                             <- t(OM)
    OM[lower.tri(OM, diag = FALSE)] <- tOM[lower.tri(tOM, diag = FALSE)]
    
    if (is.null(numParCov)) {
      numParCov <- calcNumParCov(dfext, numNonFREMThetas, numSkipOm)
    }
    
    modelState <- list(
      theta = THETA,
      thetaFix = THETAFIX,
      omegaMatrix = OM,
      numTheta = numTheta,
      numOmega = numOmega,
      numParCov = numParCov
    )
    
  } else {
    if (file.exists(strFREMModel)) {
      mod <- scan(strFREMModel, what = "character", sep = "\n", quiet = TRUE)
      osTheta <- mod[grep("THETA\\([0-9]+\\)", mod)]
      
      for (str in osTheta) {
        tmp <- gsub(".*THETA\\(([0-9]+)\\).*", "\\1", str)
        if (as.numeric(tmp) > numTheta) {
          numTheta <- as.numeric(tmp)
        }
      }
      
      osOmega <- mod[grep("\\bETA\\([0-9]+\\)", mod)] 
      
      for (str in osOmega) {
        tmp <- gsub(".*ETA\\(([0-9]+)\\).*", "\\1", str)
        if (as.numeric(tmp) > numOmega) {
          numOmega <- as.numeric(tmp)
        }
      }
      
      if (is.null(numParCov)) {
        stop("If no *.ext file exist, the number of parameters (numParCov) needs to be specified!")
      }
      
      modelState <- list(
        theta = NULL,
        thetaFix = NULL,
        omegaMatrix = NULL,
        numTheta = numTheta,
        numOmega = numOmega,
        numParCov = numParCov
      )
      
    } else {
      stop(paste0("Cannot find the FREM model: ", strFREMModel))
    }
  }
  
  return(modelState)
}