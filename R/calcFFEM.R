#' Calculate the FFEM corresponding to a FREM model
#'
#' Computes the FFEM for the full covariate model of a FREM model, or for any
#' subset of its covariates (`availCov`).
#'
#' `calcFFEM()` computes the FFEM for a covariate model made of the covariates
#' in `availCov`, which must be among the names in `covNames`. Leaving a
#' covariate out of `availCov` gives the FFEM of a model without that
#' covariate: this is how a FREM model handles a covariate that is not
#' available.
#'
#' `calcFFEM()` takes no covariate values, so it cannot see a missing value
#' in the data such as -99. Called once for all subjects, it uses the same
#' covariates for every subject, and a -99 is then used as a number when the
#' FFEM expressions are evaluated. For FFEM values that leave out each
#' subject's own missing covariates, use [createFFEMdata()], which calls
#' `calcFFEM()` per subject with the covariates that subject has (`missVal`).
#'
#' The results are the covariate coefficients, the FFEM expressions, the omega
#' prims and the full omega matrix to be used in the FFEM model. Optionally, it
#' also computes the corresponding eta_prims for a subject with the specified
#' set of covariates.
#'
#' **The FFEM expressions are on the scale of the ETA.** For parameter *k*, the
#' expression is the covariate effect added to that parameter's ETA, as
#' [createFFEMmodel()] writes it: in a model with `CL = TVCL * EXP(ETA(3))`,
#' the FFEM is `CL = TVCL * EXP(<expression> + ETA(3))`, not
#' `CL = TVCL + <expression>`. The same holds for the equations written to
#' `eqFile`.
#'
#' If `fremETA` is non-`NULL`, ETA prims will be calculated. In this case,
#' `fremETA` should be a numeric vector of individual ETAs from the FREM model.
#' Only one set of ETA prims will be computed (for one individual). To compute
#' the ETA prims for all subjects at the same time, please use [calcEtas()].
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
#'   =FALSE` or in `eqFile` if it is non-blank. Defaults to `Par` followed by a
#'   number.
#' @param covNames Names of the covariates in the FREM model. Defaults to `Cov`
#'   followed by a number.
#' @param availCov Names of the covariates in the FFEM: all of `covNames` (the
#'   default) or a subset. The covariates left out are treated as not
#'   available, for every subject alike. Missing values in the data are not
#'   handled here; see [createFFEMdata()].
#' @param quiet If FALSE, will print the FFEM model + associated $OMEGA BLOCK to
#'   STDOUT, or to `eqFile` and `omFile`.
#' @param fremETA NULL or a vector of individual ETAs from the FREM model.
#' @param eqFile File name to save the FFEM equations in, one line per
#'   parameter: `CL:0.00591*(WT-86.863)+...`. Each line is the covariate effect
#'   added to the parameter's ETA (see Details). Numbers have 3 decimals, or 3
#'   significant digits when they need more decimals. Written only when
#'   `quiet = FALSE`.
#' @param omFile File name to save the omega prim matrix in. Written only when
#'   `quiet = FALSE`.
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
#'   parameter: the covariate effect added to that parameter's ETA, written
#'   for evaluation with the covariates in a data frame called `data`.
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
#' library(dplyr)
#' extFile <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
#' dfExt <- getExt(extFile = extFile)
#' calcFFEMtestout <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = TRUE)
#'
#' ## Use fremETA to also compute the ETA prim
#' phiFile <- system.file("extdata/SimNeb/run31.phi", package = "PMXFrem")
#' dfPhi <- getPhi(phiFile) %>% select(starts_with("ETA"))
#' calcFFEMtestout <- calcFFEM(dfExt,
#'   numNonFREMThetas = 7, numSkipOm = 2, quiet = TRUE,
#'   fremETA = as.numeric(dfPhi[1, ])
#' )
#'
#' @family FFEM Conversion
#' @concept ffem_conversion
calcFFEM <- function(dfext,
                     numNonFREMThetas,
                     numSkipOm = 0,
                     numFREMThetas = length(grep("THETA", names(dfext))) - numNonFREMThetas,
                     numSigmas = length(grep("SIGMA", names(dfext))),
                     numParCov = NULL,
                     parNames = NULL, # paste("Par", 1:numParCov, sep = ""),
                     covNames = paste("Cov", 1:numFREMThetas, sep = ""),
                     availCov = covNames,
                     quiet = FALSE,
                     fremETA = NULL,
                     eqFile = "",
                     omFile = "") {
  # Calculate the number of parameters to include covariates on
  if (is.null(numParCov)) {
    numParCov <- calcNumParCov(dfext, numNonFREMThetas, numSkipOm)
  }

  # Add default logic for parNames inside the function
  if (is.null(parNames)) {
    parNames <- paste("Par", 1:numParCov, sep = "")
  }

  if (length(parNames) != numParCov) {
    stop(sprintf(
      "Validation Error: Length of `parNames` (%d) must exactly match the number of structural parameters affected by covariates `numParCov` (%d).",
      length(parNames), numParCov
    ), call. = FALSE)
  }

  if (length(covNames) != numFREMThetas) {
    stop(sprintf(
      "Validation Error: Length of `covNames` (%d) must exactly match the number of FREM covariates `numFREMThetas` (%d).",
      length(covNames), numFREMThetas
    ), call. = FALSE)
  }

  iNumFREMOM <- (numFREMThetas + numParCov) * (numFREMThetas + numParCov + 1) / 2
  if (nrow(dfext) > 1) dfext <- dfext[dfext$ITERATION == -1000000000, ]

  df_th <- as.numeric(dfext[, 2:(numNonFREMThetas + 1)])
  df_thm <- as.numeric(dfext[, (numNonFREMThetas + 2):(numNonFREMThetas + 1 + numFREMThetas)])
  df_om <- as.numeric(dfext[, (numNonFREMThetas + numSigmas + 2 + numFREMThetas):(ncol(dfext) - 1)])
  num_om <- -1 / 2 + sqrt(1 / 4 + 2 * iNumFREMOM) + numSkipOm # The col/row size of the full OM matrix (including all blocks)
  om_matrix <- as.numeric(df_om)

  # Get the om-matrix
  OM <- matrix(0, nrow = num_om, ncol = num_om) # Define an empty matrix

  # <<< START OF BUG FIX for replacement length warning >>>
  # Ensure om_matrix has the correct number of elements to fill the OM matrix
  # This respects the user-provided numParCov argument. Should generate the same results as the original line (below) for cases when numParCov > 1.
  num_elements_needed <- sum(upper.tri(OM, diag = TRUE))
  om_matrix_subset <- om_matrix[1:num_elements_needed]
  OM[upper.tri(OM, diag = TRUE)] <- om_matrix_subset
  # <<< END OF BUG FIX >>>

  # OM[upper.tri(OM, diag = TRUE)]  <- om_matrix # Assign upper triangular + diag
  tOM <- t(OM) # Get a transposed matrix
  OM[lower.tri(OM, diag = FALSE)] <- tOM[lower.tri(tOM, diag = FALSE)] # Assign the lower triangular except diag
  OMFULL <- OM

  if (numSkipOm != 0) OM <- OM[-(1:numSkipOm), -(1:numSkipOm)] # Remove upper block

  OM_PAR <- OM[1:numParCov, 1:numParCov] # The parameter covariance matrix
  OM_COV <- OM[(numParCov + 1):(numParCov + numFREMThetas), (numParCov + 1):(numParCov + numFREMThetas)] # The covariates covariance matrix
  OM_PAR_COV <- OM[1:numParCov, (numParCov + 1):(numParCov + numFREMThetas)] # The covariance between covariates and parameters matrix

  if (length(availCov) != 0 & length(c(seq_along(covNames))[!(covNames %in% availCov)]) != 0) {
    missCov <- c(seq_along(covNames))[!(covNames %in% availCov)]

    OM_COV <- OM_COV[-missCov, -missCov]
    inv <- solve(OM_COV)

    if (ncol(as.matrix(OM_PAR_COV)) == 1) {
      OM_PAR_COV <- t(as.matrix(OM_PAR_COV))[, -missCov]
      if (!is.matrix(OM_PAR_COV)) OM_PAR_COV <- t(as.matrix(OM_PAR_COV))
    } else {
      OM_PAR_COV <- as.matrix(OM_PAR_COV[, -missCov])
    }

    ## Fix the covariate names and means
    covNames <- covNames[-missCov]
    df_thm <- df_thm[-missCov]

    COEFF <- OM_PAR_COV %*% inv # The parameter-covariate coefficients
    COEFF_VAR <- OM_PAR - OM_PAR_COV %*% inv %*% t(OM_PAR_COV) # The parameter variances
  } else if (length(c(seq_along(covNames))[!(covNames %in% availCov)]) == 0) {
    if (ncol(as.matrix(OM_PAR_COV)) == 1) {
      OM_PAR_COV <- t(as.matrix(OM_PAR_COV))
    } else {
      OM_PAR_COV <- as.matrix(OM_PAR_COV)
    }

    inv <- solve(OM_COV)
    COEFF <- OM_PAR_COV %*% inv # The parameter-covariate coefficients
    COEFF_VAR <- OM_PAR - OM_PAR_COV %*% inv %*% t(OM_PAR_COV) # The parameter variances
  } else {
    COEFF <- t(as.matrix(OM_PAR_COV))
    COEFF_VAR <- OM_PAR
  }

  if (length(availCov) == 0) COEFF <- t(COEFF)

  ## Print the FFEM for inspection
  if (!quiet) {
    for (p in seq_len(nrow(COEFF))) {
      for (c in seq_len(ncol(COEFF))) {
        if (c == 1 & p == 1) cat(parNames[p], ":", sep = "", file = eqFile)
        if (c == 1 & p != 1) cat(parNames[p], ":", sep = "", file = eqFile, append = TRUE)
        if (length(availCov) == 0) {
          cat(paste("0", "*", "(", covNames[c], "-", paste(.fremEqRound(df_thm[c]), ")", sep = ""), sep = ""), file = eqFile, append = TRUE)
        } else {
          cat(paste(.fremEqRound(COEFF[p, c]), "*", "(", covNames[c], "-", paste(.fremEqRound(df_thm[c]), ")", sep = ""), sep = ""), file = eqFile, append = TRUE)
        }
        if (c != ncol(COEFF)) cat("+", file = eqFile, append = TRUE)
        if (c == ncol(COEFF)) cat("\n", file = eqFile, append = TRUE)
      }
    }
  }

  ## Create evaluable expression
  myExpr <- c()
  for (p in seq_len(nrow(COEFF))) {
    myExpr[p] <- ""
    for (c in seq_len(ncol(COEFF))) {
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

    for (v in seq_len(nrow(COEFF_VAR))) {
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
      list(
        Coefficients = COEFF,
        Vars = COEFF_VAR,
        Expr = myExpr,
        FullVars = FULLVARS,
        UpperVars = UPPERVARS,
        Eta_prim = eta_prim
      )
    )
  )
}

#' A number as the FFEM equations write it
#'
#' Three decimals, or three significant digits when a number needs more
#' decimals than that, so that a small coefficient is not written as 0.
#'
#' @param x A numeric vector.
#' @return `x`, rounded.
#' @keywords internal
#' @noRd
.fremEqRound <- function(x) {
  digits <- ifelse(x == 0 | !is.finite(x), 3, pmax(3, 2 - floor(log10(abs(x)))))
  round(x, digits)
}
