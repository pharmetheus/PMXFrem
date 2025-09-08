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
  
  safeFindRecord <- function(current_lines, record, replace) {
    result <- findrecord(current_lines, record = record, replace = replace, quiet = TRUE)
    if (is.null(result)) { return(current_lines) }
    return(result)
  }
  
  strNewCovNames <- c(covnames$covNames, addedList)
  line <- readLines(strFREMModel)
  
  strinput <- c()
  loop_start <- noBaseThetas + 1
  loop_end <- length(strNewCovNames) + noBaseThetas
  if (loop_end >= loop_start) {
    for (i in loop_start:loop_end) {
      mu_count <- i - noBaseThetas + numSkipOm + numParCov
      strinput <- c(strinput, paste0("      MU_", mu_count, " = ", "THETA(", i, ")"))
      strinput <- c(strinput, paste0("      COV", mu_count, " = MU_", mu_count, " + ETA(", mu_count, ")"))
    }
  }
  
  mu_indices <- grep(pattern = "MU_\\d+ = THETA", x = line)
  cov_indices <- grep(pattern = "COV\\d+ = MU_", x = line)
  if (length(mu_indices) > 0 && length(cov_indices) > 0) {
    start_line <- min(mu_indices)
    end_line <- max(cov_indices)
    line <- c(
      if (start_line > 1) line[1:(start_line - 1)] else NULL,
      strinput,
      if (end_line < length(line)) line[(end_line + 1):length(line)] else NULL
    )
  } else if (length(strinput) > 0) {
    warning("can't find MU/COV block to replace; new block not inserted")
  }
  
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
  line <- safeFindRecord(line, record = ";;;FREM CODE BEGIN COMPACT", replace = strinput_frem)
  
  THETA <- modelState$theta; OM <- modelState$omegaMatrix; THETAFIX <- modelState$thetaFix
  iNumTHETA <- modelState$numTheta; iNumOM <- modelState$numOmega
  
  # DEFINITIVE FIX: Handle the case where THETAFIX is NULL (no .ext file)
  if (is.null(THETAFIX)) {
    THETAFIX <- rep(0, iNumTHETA)
  }
  
  if (is.null(basenames_th)) basenames_th <- paste0("BASE", 1:noBaseThetas)
  if (is.null(basenames_om)) basenames_om <- paste0("BASE", 1:(numSkipOm + numParCov))
  
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
  
  strinput_theta <- c()
  if (iNumTHETA > 0) {
    for (i in 1:iNumTHETA) {
      strFIX <- if (!is.na(THETAFIX[i]) && THETAFIX[i] == 1 && bWriteFIX) " FIX" else ""
      strinput_theta <- c(strinput_theta, paste0("$THETA ", THETA[i], strFIX, " ", theta_comment[i]))
    }
  }
  line <- safeFindRecord(line, record = "\\$THETA", replace = strinput_theta)
  
  newommatrix <- buildmatrix(as.matrix(OM))
  if (length(newommatrix) > 0) {
    j <- length(newommatrix)
    for (i in length(om_comment):(1 + numSkipOm)) {
      if (j > 0) { newommatrix[j] <- paste0(newommatrix[j], " ", om_comment[i]); j <- j - 1 }
    }
  }
  line <- safeFindRecord(line, record = "\\$OMEGA", replace = newommatrix)
  
  if (strUpdateType != "NoData") {
    line <- safeFindRecord(line, record = "\\$DATA", replace = paste0("$DATA ", strNewFREMData, " IGNORE=@"))
    if (!is.null(final_df)) {
      line <- safeFindRecord(line, record = "\\$INPUT", replace = paste0("$INPUT ", paste0(names(final_df), collapse = " ")))
    }
  }
  
  if (bWriteMod) {
    strNewModelFileName <- paste0(tools::file_path_sans_ext(strFREMModel), "_new.mod")
    writeLines(line, strNewModelFileName)
  }
  
  return(line)
}