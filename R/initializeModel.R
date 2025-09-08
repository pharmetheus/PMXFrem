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
#' @export
initializeModelParameters <- function(strFREMModel,
                                      numNonFREMThetas,
                                      numSkipOm,
                                      numParCov) {
  
  numTheta <- -1
  numOmega <- -1
  extFile <- paste0(tools::file_path_sans_ext(strFREMModel), ".ext")
  
  ## Use the ext file to figure out number of THETAS, OMEGAS and SIGMAS
  if (file.exists(extFile)) {
    # Read in parameter values
    dfext     <- subset(getExt(extFile = extFile), ITERATION == "-1000000000")
    # Read in which parameters are fixed
    dfextfix  <- subset(getExt(extFile = extFile), ITERATION == "-1000000006")
    
    numTheta <- length(names(dfext)[regexpr("THETA.*", names(dfext)) == 1])
    numOmegaElements <- length(names(dfext)[regexpr("OMEGA.*", names(dfext)) == 1])
    numOmega <- -1 / 2 + sqrt(1 / 4 + 2 * numOmegaElements) # Get the number of diagonals from the number of OM elements
    THETA    <- as.numeric(dfext[, names(dfext)[regexpr("THETA.*", names(dfext)) == 1]])
    THETAFIX <- as.numeric(dfextfix[, names(dfextfix)[regexpr("THETA.*", names(dfextfix)) == 1]])
    OMEGA    <- as.numeric(dfext[, names(dfext)[regexpr("OMEGA.*", names(dfext)) == 1]])
    
    OM                              <- matrix(0, nrow = numOmega, ncol = numOmega) # Define an empty matrix
    OM[upper.tri(OM, diag = TRUE)]  <- OMEGA # Assign upper triangular + diag
    tOM                             <- t(OM) # Get a transposed matrix
    OM[lower.tri(OM, diag = FALSE)] <- tOM[lower.tri(tOM, diag = FALSE)] # Assign the lower triangular except diag
    
    if (is.null(numParCov)) {
      numParCov <- calcNumParCov(dfext, numNonFREMThetas, numSkipOm)
    }
    
    # Populate the return list with values from the .ext file
    modelState <- list(
      theta = THETA,
      thetaFix = THETAFIX,
      omegaMatrix = OM,
      numTheta = numTheta,
      numOmega = numOmega,
      numParCov = numParCov
    )
    
  } else {
    # Parsing of the FREM model if .ext file does not exist
    if (file.exists(strFREMModel)) {
      mod <- scan(strFREMModel, what = "character", sep = "\n", quiet = TRUE)
      osTheta <- mod[grep("THETA\\([0-9]+\\)", mod)] # Returns positions of every THETA(NUMBER)
      
      ### Get the maximum THETA number
      for (str in osTheta) {
        tmp <- gsub(".*THETA\\(([0-9]+)\\).*", "\\1", str)
        if (as.numeric(tmp) > numTheta) {
          numTheta <- as.numeric(tmp)
        }
      }
      
      ## Note, only accepts ETA with space before
      osOmega <- mod[grep("[ ]ETA\\([0-9]+\\)", mod)] # Returns positions of every ETA(NUMBER)
      
      ## Get the maximum OMEGA number
      for (str in osOmega) {
        tmp <- gsub(".*[ ]ETA\\(([0-9]+)\\).*", "\\1", str)
        if (as.numeric(tmp) > numOmega) {
          numOmega <- as.numeric(tmp)
        }
      }
      
      if (is.null(numParCov)) {
        stop("If no *.ext file exist, the number of parameters (numParCov) needs to be specified!")
      }
      
      # Populate the return list with parsed values (THETA/OMEGA values are not available)
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