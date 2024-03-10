#' Calculate the corresponding FFEM charactersitics from a FREM model-
#'
#' Computes the corresponding FFEM from a FREM model. Can handle missing
#' covariates.
#'
#' `calcFFEM()`` computes the corresponding FFEM characteristics for a covariate
#' model consisting of the specified covariates (`availCov`). The covariate
#' names specified by `availCov` should be among the names listed in `covNames`.
#'
#' The computed characteristics are the covariate coefficients, the FFEM
#' expressions, the omega prims and the full omega matrix to be used in the FFEM
#' model. Optionally, it also computes the corresponding eta_prims for a subject
#' with the specified set of covariates.
#'
#' If `fremETA` is non-`NULL`, ETA prims will be calculated. In this case,
#' `fremETA` should be a numeric vector of individual ETAs from the FREM model.
#' Only one set of ETA prims will be computed (for one individual). To compute
#' the ETA prims for all subjects at the same ime, please use [calcEtas()].
#'
#' @param dfext A data frame of the ext file for the FREM model. In case of
#'   multiple $EST, the dfext should contain the estimates of the *one* relevant
#'   $EST.
#' @param numNonFREMThetas Number of thetas in the FREM model that are not FREM
#'   covariates. These need to come before the FREM covariate thetas in the FREM
#'   model file.
#' @param numSkipOm Number of diagonal omegas that should not be part of the
#'   FREM calculations. Such omegas has to come before the large FREM omega
#'   block in the FREM model file.
#' @param numFREMThetas  The number thetas associated with FREM covariate
#'   effects.
#' @param numSigmas Number of sigma parameters in the ext file model from the
#'   FREM model (including zero sigma covariance elements).
#' @param numParCov Number of parameters for which covariate relations are
#'   sought (often the same as numNonFREMThetas). default (NULL) indicates that
#'   this is guessed by the function by assuming that each covariate effect is
#'   modeled as one theta and one eta.
#' @param parNames Names of the parameters. Only used in the STDOUT when `quiet
#'   =FALSE` or in `eqFile` if it is non-blank. Defaults to `Par` folowed by a
#'   number.
#' @param covNames Names of the covariates in the FREM model. Defaults to `Cov`
#'   followed by a number.
#' @param availCov Names of the covariates to use in the calculation of the FFEM
#'   model. Should be covariate names used in the FREM model.
#' @param quiet If FALSE, will print the FFEM model + associated $OMEGA BLOCK to
#'   STDOUT.
#' @param fremEta NULL or a vector of individual ETAs from the FREM model.
#' @param eqFile File name to save the FFEM equations in.
#' @param omFile File name to save the omega prim matrix in.
#'
#'
#' @return A list with components Coefficients, Vars and Expr:
#'
#'   Coeffients: The numNonFREMThetas x availCov matrix with FFEM model
#'   coefficients
#'
#'   Vars:       The numNonFREMThetas x numNonFREMThetas matrix with the FFEM
#'   variances (OMEGA)
#'
#'   Expr:       The vector of FFEM expressions, one for each base model
#'   parameter.
#'
#'   Fullvars:   The full FFEM variance-covariance matrix of all OMEGAs in the
#'   model.
#'
#'   Uppervars:  The variance-covariance matrix of the skipped OMEGAs in the
#'   model.
#'
#'   Eta_prim:  The matrix of individual etas re-scaled to FFEM etas.
#' @export
#'
#' @examples
#'
#' extFile         <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
#' dfExt           <- getExt(extFile = extFile)
#' calcFFEMtestout <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = T)
#'
#' ## Use fremETA to also compute the ETA prim
#' phiFile         <- system.file("extdata/SimNeb/run31.phi", package = "PMXFrem")
#' dfPhi           <- getPhi(phiPhile) %>% select(starts_with("ETA"))
#' calcFFEMtestout <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = T,
#'   etaFREM = as.numeric(dfPhi[1, ]))
#'
calcFFEM <- function(dfext,
                     numNonFREMThetas,
                     numSkipOm     = 0,
                     numFREMThetas = length(grep("THETA", names(dfext))) - numNonFREMThetas,
                     numSigmas     = length(grep("SIGMA", names(dfext))),
                     numParCov     = NULL,
                     parNames      = paste("Par", 1:numParCov, sep = ""),
                     covNames      = paste("Cov", 1:numFREMThetas, sep = ""),
                     availCov      = covNames,
                     quiet         = FALSE,
                     fremETA       = NULL,
                     eqFile        = "",
                     omFile        = "",
                     ...) {
  # Calculate the number of parameters to include covariates on
  if (is.null(numParCov)) {
    numParCov <- calcNumParCov(dfext, numNonFREMThetas, numSkipOm)
  }

  iNumFREMOM <- (numFREMThetas + numParCov) * (numFREMThetas + numParCov + 1) / 2
  if (nrow(dfext) > 1) dfext  <- dfext[dfext$ITERATION == -1000000000, ]

  df_th     <- as.numeric(dfext[, 2:(numNonFREMThetas + 1)])
  df_thm    <- as.numeric(dfext[, (numNonFREMThetas + 2):(numNonFREMThetas + 1 + numFREMThetas)])
  df_om     <- as.numeric(dfext[, (numNonFREMThetas + numSigmas + 2 + numFREMThetas):(ncol(dfext) - 1)])
  num_om    <- -1 / 2 + sqrt(1 / 4 + 2 * iNumFREMOM) + numSkipOm # The col/row size of the full OM matrix (including all blocks)
  om_matrix <- as.numeric(df_om)

  # Get the om-matrix
  OM                              <- matrix(0, nrow = num_om, ncol = num_om) # Define an empty matrix
  OM[upper.tri(OM, diag = TRUE)]  <- om_matrix # Assign upper triangular + diag
  tOM                             <- t(OM) # Get a transposed matrix
  OM[lower.tri(OM, diag = FALSE)] <- tOM[lower.tri(tOM, diag = FALSE)] # Assign the lower triangular except diag
  OMFULL                          <- OM

  if (numSkipOm != 0) OM <- OM[-(1:numSkipOm), -(1:numSkipOm)] # Remove upper block

  OM_PAR     <- OM[1:numParCov, 1:numParCov] # The parameter covariance matrix
  OM_COV     <- OM[(numParCov + 1):(numParCov + numFREMThetas), (numParCov + 1):(numParCov + numFREMThetas)] # The covariates covariance matrix
  OM_PAR_COV <- OM[1:numParCov, (numParCov + 1):(numParCov + numFREMThetas)] # The covariance between covariates and parameters matrix

  if (length(availCov) != 0 & length(c(1:length(covNames))[!(covNames %in% availCov)]) != 0) {
    missCov    <- c(1:length(covNames))[!(covNames %in% availCov)]

    OM_COV <- OM_COV[-missCov, -missCov]
    inv    <- solve(OM_COV)

    if (ncol(as.matrix(OM_PAR_COV)) == 1) {
      OM_PAR_COV <- t(as.matrix(OM_PAR_COV))[, -missCov]
      if (!is.matrix(OM_PAR_COV)) OM_PAR_COV <- t(as.matrix(OM_PAR_COV))
    } else {
      OM_PAR_COV <- as.matrix(OM_PAR_COV[, -missCov])
    }

    ## Fix the covariate names and means
    covNames   <- covNames[-missCov]
    df_thm     <- df_thm[-missCov]

    COEFF     <- OM_PAR_COV %*% inv # The parameter-covariate coefficients
    COEFF_VAR <- OM_PAR - OM_PAR_COV %*% inv %*% t(OM_PAR_COV) # The parameter variances

  } else if (length(c(1:length(covNames))[!(covNames %in% availCov)]) == 0) {

    if (ncol(as.matrix(OM_PAR_COV)) == 1) {
      OM_PAR_COV <- t(as.matrix(OM_PAR_COV))
    } else {
      OM_PAR_COV <- as.matrix(OM_PAR_COV)
    }

    inv       <- solve(OM_COV)
    COEFF     <- OM_PAR_COV %*% inv # The parameter-covariate coefficients
    COEFF_VAR <- OM_PAR - OM_PAR_COV %*% inv %*% t(OM_PAR_COV) # The parameter variances
  } else {
    COEFF     <- t(as.matrix(OM_PAR_COV))
    COEFF_VAR <- OM_PAR
  }

  if (length(availCov) == 0) COEFF <- t(COEFF)

  ## Print the FFEM for inspection
  if (!quiet) {
    for (p in 1:nrow(COEFF)) {
      for (c in 1:ncol(COEFF)) {
        if (c == 1 & p == 1) cat(parNames[p], "", sep = "", file = eqFile)
        if (c == 1 & p != 1) cat(parNames[p], "", sep = "", file = eqFile, append = TRUE)
        if (length(availCov) == 0) {
          cat(paste("0", "*", "(", covNames[c], "-", paste(round(df_thm[c], 3), ")", sep = ""), sep = ""), file = eqFile, append = TRUE)
        } else {
          cat(paste(round(COEFF[p, c], 3), "*", "(", covNames[c], "-", paste(round(df_thm[c], 3), ")", sep = ""), sep = ""), file = eqFile, append = TRUE)
        }
        if (c != ncol(COEFF)) cat("+", file = eqFile, append = TRUE)
        if (c == ncol(COEFF)) cat("\n", file = eqFile, append = TRUE)
      }
    }
  }

  ## Create evaluable expression
  myExpr <- c()
  for (p in 1:nrow(COEFF)) {
    myExpr[p] <- ""
    for (c in 1:ncol(COEFF)) {
      if (length(availCov) == 0) {
        myExpr[p] <- paste(myExpr[p], "0", "*", "(data$", covNames[c], "-", df_thm[c], ")", sep = "")
      } else {
        myExpr[p] <- paste(myExpr[p], COEFF[p, c], "*", "(data$", covNames[c], "-", df_thm[c], ")", sep = "")
      }
      if (c != ncol(COEFF)) myExpr[p] <- paste(myExpr[p], "+")
    }
  }

  if (!quiet) {
    cat(paste("\n$OMEGA BLOCK(", nrow(COEFF_VAR), ")", sep = ""), "\n", file = omFile)

    for (v in 1:nrow(COEFF_VAR)) {
      for (v2 in 1:v) {
        cat(round(COEFF_VAR[v, v2], 5), file = omFile, append = TRUE)
        if (v2 == v) cat("\n", append = TRUE, file = omFile)
      }
    }
  }

  if (numSkipOm == 0) { # If no upper block
    FULLVARS <- COEFF_VAR
    UPPERVARS <- numeric(0)
  } else {
    FULLVARS <- matrix(0, ncol = numSkipOm + ncol(COEFF_VAR), nrow = numSkipOm + ncol(COEFF_VAR))
    FULLVARS[(numSkipOm + 1):ncol(FULLVARS), (numSkipOm + 1):ncol(FULLVARS)] <- COEFF_VAR
    FULLVARS[1:numSkipOm, 1:numSkipOm] <- OMFULL[1:numSkipOm, 1:numSkipOm]
    UPPERVARS <- OMFULL[1:numSkipOm, 1:numSkipOm]
  }

  ## Calculate eta_prim
  if (!is.null(fremETA)) {
    if (numSkipOm == 0) {
      if (!exists("missCov")) {
        eta_prim <- fremETA[1:numParCov] - COEFF %*% as.numeric(fremETA[(numParCov + 1):(numParCov + numFREMThetas)])
      } else {
        eta_prim <- fremETA[1:numParCov] - COEFF %*% as.numeric(fremETA[(numParCov + 1):(numParCov + numFREMThetas)][-missCov])
      }
    } else {
      if (!exists("missCov")) {
        # eta_prim <- c(fremETA[1:numSkipOm], fremETA[(numSkipOm + 1):(numParCov + numSkipOm)] - COEFF %*% as.numeric(fremETA[(numSkipOm + numParCov + 1):(numSkipOm + numParCov + numFREMThetas)]))
        eta_prim <- c(fremETA[1:numSkipOm], fremETA[(numSkipOm + 1):(numParCov + numSkipOm)] - COEFF %*% as.matrix(fremETA[(numSkipOm + numParCov + 1):(numSkipOm + numParCov + numFREMThetas)]))
      } else {
        eta_prim <- c(fremETA[1:numSkipOm], fremETA[(numSkipOm + 1):(numParCov + numSkipOm)] - COEFF %*% as.numeric(fremETA[(numSkipOm + numParCov + 1):(numSkipOm + numParCov + numFREMThetas)][-missCov]))
      }
    }
  } else {
    eta_prim <- NULL
  }

  return(
    invisible(
      list(Coefficients = COEFF,
        Vars            = COEFF_VAR,
        Expr            = myExpr,
        FullVars        = FULLVARS,
        UpperVars       = UPPERVARS,
        Eta_prim        = eta_prim
      )
    )
  )
}
