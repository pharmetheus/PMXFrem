#' Generate a FREM Model File
#'
#' Generates the new NONMEM model code based on the final parameter state and
#' writes it to a file.
#'
#' @param final_df The finalized FREM dataset, used for the $INPUT record.
#' @param modelState A list containing the model's parameter state.
#' @param ... (Additional parameters are omitted for brevity, but would include
#'   covList, addedList, covnames, strFREMModel, bWriteMod, etc.)
#' @return A character vector of the new model file lines.
#'
generateFremModel <- function(final_df,
                              modelState,
                              covList,
                              addedList,
                              covnames,
                              strFREMModel,
                              strNewFREMData,
                              bWriteMod,
                              bWriteFIX,
                              noBaseThetas,
                              numSkipOm,
                              numParCov,
                              covEpsNum,
                              basenames_th,
                              basenames_om,
                              dDefaultCovValue,
                              strUpdateType) {
  
  # Helper function to safely call findrecord and avoid overwriting with NULL
  safeFindRecord <- function(current_lines, record, replace) {
    result <- findrecord(current_lines, record = record, replace = replace, quiet = TRUE)
    if (is.null(result)) {
      # If record was not found, return the original lines unmodified
      return(current_lines)
    }
    return(result)
  }
  
  strNewCovNames <- c(covnames$covNames, addedList)
  
  line <- readLines(strFREMModel)
  
  # Generate MU_/COV block
  strinput <- c()
  # FIX: Add a condition to prevent the loop from running with a reverse sequence (e.g., 2:1)
  loop_start <- noBaseThetas + 1
  loop_end <- length(strNewCovNames) + noBaseThetas
  if (loop_end >= loop_start) {
    for (i in loop_start:loop_end) {
      mu_count <- i - noBaseThetas + numSkipOm + numParCov
      strinput <- c(strinput, paste0("      MU_", mu_count, " = ", "THETA(", i, ")"))
      strinput <- c(strinput, paste0("      COV", mu_count, " = MU_", mu_count, " + ETA(", mu_count, ")"))
    }
  }
  
  tmpl <- grep(pattern = "COV\\d+ = MU\\_\\d+", x = line)
  if (length(tmpl) == 0 && length(strinput) > 0) warning("can't find MU/COV block to replace")
  if (length(tmpl) > 0) {
    # This manual replacement is brittle, but retained to match original behavior
    line <- c(line[1:(min(tmpl) - 2)], strinput, line[(max(tmpl) + 1):length(line)])
  }
  
  # Generate FREM code block
  iFremTypeIncrease <- 100
  fremTypes <- seq(from = iFremTypeIncrease, by = iFremTypeIncrease, length.out = length(strNewCovNames))
  strinput_frem <- c(";;;FREM CODE BEGIN COMPACT")
  if (length(strNewCovNames) > 0) {
    for (i in 1:length(strNewCovNames)) {
      strinput_frem <- c(strinput_frem, paste0("      IF(FREMTYPE.EQ.", fremTypes[i], ") THEN"))
      strinput_frem <- c(strinput_frem, paste0(";       ", strNewCovNames[i]))
      strinput_frem <- c(strinput_frem, paste0("        Y = COV", i + numSkipOm + numParCov, " + EPS(", covEpsNum, ")"))
      strinput_frem <- c(strinput_frem, paste0("        IPRED = COV", i + numSkipOm + numParCov))
      strinput_frem <- c(strinput_frem, paste0("      ENDIF"))
    }
  }
  strinput_frem <- c(strinput_frem, ";;;FREM CODE END COMPACT")
  # FIX: Use the safe wrapper for all findrecord calls
  line <- safeFindRecord(line, record = ";;;FREM CODE BEGIN COMPACT", replace = strinput_frem)
  
  # Unpack model state
  THETA <- modelState$theta; OM <- modelState$omegaMatrix; THETAFIX <- modelState$thetaFix
  iNumTHETA <- modelState$numTheta; iNumOM <- modelState$numOmega
  
  if (is.null(basenames_th)) basenames_th <- paste0("BASE", 1:noBaseThetas)
  if (is.null(basenames_om)) basenames_om <- paste0("BASE", 1:(numSkipOm + numParCov))
  
  # Update parameters for new covariates
  if (!is.null(addedList) & length(addedList) > 0) {
    if (is.null(OM)) stop("OM missing, must provide .ext file when adding covariates")
    OMNEW <- matrix(dDefaultCovValue, ncol(OM) + length(addedList), nrow(OM) + length(addedList))
    OMNEW[1:ncol(OM), 1:nrow(OM)] <- OM
    OM <- OMNEW
  }
  
  theta_comment <- paste0(" ; ", 1:iNumTHETA, " TV_", c(basenames_th, covnames$covNames))
  om_comment <- paste0(" ; ", 1:iNumOM, " BSV_", c(basenames_om, covnames$covNames))
  
  if (!is.null(addedList)) {
    for (i in 1:length(addedList)) {
      strcov <- addedList[i]
      l <- covList[[strcov]]
      THETA <- c(THETA, l[["Mean"]]); THETAFIX <- c(THETAFIX, 0)
      theta_comment <- c(theta_comment, paste0(" ; ", iNumTHETA + 1, " TV_", l[["Name"]]))
      iNumTHETA <- iNumTHETA + 1
      OM[iNumOM + 1, iNumOM + 1] <- l[["Var"]]
      om_comment <- c(om_comment, paste0(" ; ", iNumOM + 1, " BSV_", l[["Name"]]))
      iNumOM <- iNumOM + 1
    }
  }
  
  # Generate and replace $THETA
  strinput_theta <- c()
  if (iNumTHETA > 0) {
    for (i in 1:iNumTHETA) {
      strFIX <- if (!is.na(THETAFIX[i]) && THETAFIX[i] == 1 && bWriteFIX) " FIX" else ""
      strinput_theta <- c(strinput_theta, paste0("$THETA ", THETA[i], strFIX, " ", theta_comment[i]))
    }
  }
  line <- safeFindRecord(line, record = "\\$THETA", replace = strinput_theta)
  
  # Generate and replace $OMEGA
  newommatrix <- buildmatrix(as.matrix(OM))
  if (length(newommatrix) > 0) {
    j <- length(newommatrix)
    for (i in length(om_comment):(1 + numSkipOm)) {
      if (j > 0) {
        newommatrix[j] <- paste0(newommatrix[j], " ", om_comment[i])
        j <- j - 1
      }
    }
  }
  line <- safeFindRecord(line, record = "\\$OMEGA", replace = newommatrix)
  
  # Replace $DATA and $INPUT
  if (strUpdateType != "NoData") {
    line <- safeFindRecord(line, record = "\\$DATA", replace = paste0("$DATA ", strNewFREMData, " IGNORE=@"))
    if (!is.null(final_df)) {
      line <- safeFindRecord(line, record = "\\$INPUT", replace = paste0("$INPUT ", paste0(names(final_df), collapse = " ")))
    }
  }
  
  # Write file if requested
  if (bWriteMod) {
    strNewModelFileName <- paste0(tools::file_path_sans_ext(strFREMModel), "_new.mod")
    writeLines(line, strNewModelFileName)
  }
  
  return(line)
}