#' Generate a FREM Model File
#'
#' Generates the new NONMEM model code based on the final parameter state, 
#' injects the consolidated Full Random Effects Model (FREM) covariate block, 
#' and optionally writes the updated model to disk. It features smart comment 
#' extraction to preserve parameter labels and enforces positive-definite 
#' covariance matrices by replacing structural zeros.
#'
#' @param final_df A data.frame containing the finalized FREM dataset, used to dynamically construct the `$INPUT` record.
#' @param modelState A list containing the model's current parameter state (including `theta`, `omegaMatrix`, `thetaFix`, `numTheta`, `numOmega`).
#' @param covList A list of lists containing prepared statistical information for new covariates (`Name`, `Mean`, `Var`, `Fremtype`).
#' @param addedList A character vector of new covariate names added during this update step.
#' @param covnames A list containing parsed existing covariate names (`covNames`, `polyCatCovs`, `orgCovNames`).
#' @param strFREMModel A character string specifying the file path to the base FREM model (`.mod`) file.
#' @param strNewFREMData A character string specifying the file path for the newly generated FREM dataset.
#' @param bWriteMod Logical. If TRUE, writes the updated model code to disk with a `_new.mod` suffix.
#' @param bWriteFIX Logical. If TRUE, appends `FIX` to the `$THETA` records for fixed parameters.
#' @param noBaseThetas Integer. The number of structural (non-FREM) `$THETA` parameters in the base model.
#' @param numSkipOm Integer. The number of independent `$OMEGA` parameters to skip before the main FREM block.
#' @param numParCov Integer. The number of structural parameters exhibiting covariate effects.
#' @param covEpsNum Integer. The index of the `$SIGMA` parameter (epsilon) used for the FREM covariates.
#' @param basenames_th A character vector of names for the structural `$THETA` parameters. If NULL, parsed dynamically from the base model.
#' @param basenames_om A character vector of names for the structural `$OMEGA` parameters. If NULL, parsed dynamically from the base model.
#' @param dDefaultCovValue Numeric. The default covariance value (typically 1e-05) used to populate structural zeros, ensuring the `$OMEGA` block remains positive-definite.
#' @param strUpdateType A character string specifying the update mode (e.g., "DataAndModel", "NoData").
#'
#' @return A character vector containing the lines of the newly generated FREM model file.
#' @keywords internal
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
  
  THETA <- modelState$theta; OM <- modelState$omegaMatrix; THETAFIX <- modelState$thetaFix
  iNumTHETA <- modelState$numTheta; iNumOM <- modelState$numOmega
  
  if (is.null(THETAFIX)) { THETAFIX <- rep(0, iNumTHETA) }
  
  # --- Exact Comment Preservation ---
  
  th_lines <- safeFindRecord(line, "\\$THETA", replace = NULL)
  existing_th_comments <- sub("^[^;]*;\\s*", "", grep(";", th_lines, value = TRUE))
  if (length(existing_th_comments) >= iNumTHETA) {
    theta_comment <- paste0(" ; ", trimws(existing_th_comments[1:iNumTHETA]))
  } else if (!is.null(basenames_th)) {
    theta_comment <- paste0(" ; ", 1:iNumTHETA, " TV_", c(basenames_th, covnames$covNames))
  } else {
    theta_comment <- paste0(" ; ", 1:iNumTHETA, " TV_BASE", 1:iNumTHETA)
  }
  
  in_omega <- FALSE
  existing_om_comments <- c()
  for (l in line) {
    if (grepl("^\\s*\\$[A-Za-z]+", l)) in_omega <- grepl("^\\s*\\$OMEGA", l, ignore.case = TRUE)
    if (in_omega && grepl(";", l)) existing_om_comments <- c(existing_om_comments, sub("^[^;]*;\\s*", "", l))
  }
  
  if (length(existing_om_comments) >= iNumOM) {
    om_comment <- paste0(" ; ", trimws(existing_om_comments[1:iNumOM]))
  } else if (!is.null(basenames_om)) {
    om_comment <- paste0(" ; ", 1:iNumOM, " BSV_", c(basenames_om, covnames$covNames))
  } else {
    om_comment <- paste0(" ; ", 1:iNumOM, " BSV_BASE", 1:iNumOM)
  }
  
  # PK and ERROR block logic
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
  }
  
  # --- FREMTYPE Block Generation ---
  iFremTypeIncrease <- 100
  fremTypes <- seq(from = iFremTypeIncrease, by = iFremTypeIncrease, length.out = length(strNewCovNames))
  strinput_frem <- c(";;;FREM CODE BEGIN COMPACT", ";;;DO NOT MODIFY")
  
  if (length(strNewCovNames) > 0) {
    for (i in 1:length(strNewCovNames)) {
      strinput_frem <- c(strinput_frem, paste0("      IF(FREMTYPE.EQ.", fremTypes[i], ") THEN"))
      strinput_frem <- c(strinput_frem, paste0(";        ", strNewCovNames[i], " 1"))
      strinput_frem <- c(strinput_frem, paste0("         Y = COV", i + numSkipOm + numParCov, " + EPS(", covEpsNum, ")"))
      strinput_frem <- c(strinput_frem, paste0("         IPRED = COV", i + numSkipOm + numParCov))
      strinput_frem <- c(strinput_frem, paste0("      END IF"))
    }
  }
  strinput_frem <- c(strinput_frem, ";;;FREM CODE END COMPACT")
  
  line <- safeFindRecord(line, record = ";;;FREM CODE BEGIN COMPACT", replace = strinput_frem)
  
  # --- Matrix Expansion & Comment Appending ---
  if (!is.null(addedList) & length(addedList) > 0) {
    if (is.null(OM)) stop("OM missing, must provide .ext file when adding covariates")
    OMNEW <- matrix(dDefaultCovValue, ncol(OM) + length(addedList), nrow(OM) + length(addedList))
    OMNEW[1:ncol(OM), 1:nrow(OM)] <- OM
    OM <- OMNEW
  }
  
  OM[OM == 0] <- dDefaultCovValue
  
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
  
  # --- Splicing OMEGA Matrix with FIX retention ---
  skipped_omegas_lines <- c()
  if (numSkipOm > 0) {
    in_omega <- FALSE
    omega_param_lines <- c()
    for (l in line) {
      if (grepl("^\\s*\\$[A-Za-z]+", l)) in_omega <- grepl("^\\s*\\$OMEGA", l, ignore.case = TRUE)
      # Identify actual parameter definitions (lines with numbers before the semicolon)
      if (in_omega && grepl("[0-9]", sub(";.*", "", l))) omega_param_lines <- c(omega_param_lines, l)
    }
    
    for (i in 1:numSkipOm) {
      is_fixed <- if (i <= length(omega_param_lines)) grepl("FIX", omega_param_lines[i], ignore.case = TRUE) else FALSE
      fix_text <- if (is_fixed) " FIX" else ""
      skipped_omegas_lines <- c(skipped_omegas_lines, paste0("$OMEGA BLOCK(1) ", OM[i, i], fix_text, om_comment[i]))
    }
  }
  
  frem_mat_indices <- (numSkipOm + 1):nrow(OM)
  frem_matrix <- as.matrix(OM[frem_mat_indices, frem_mat_indices])
  frem_block_lines <- buildmatrix(frem_matrix, forceSingleBlock = TRUE)
  
  if (length(frem_block_lines) > 0) {
    comment_idx <- numSkipOm + 1
    for (j in 2:length(frem_block_lines)) {
      if (comment_idx <= length(om_comment)) {
        frem_block_lines[j] <- paste0(frem_block_lines[j], om_comment[comment_idx])
        comment_idx <- comment_idx + 1
      }
    }
  }
  
  newommatrix <- c(skipped_omegas_lines, frem_block_lines)
  
  strinput_theta <- c()
  for (i in 1:iNumTHETA) {
    strFIX <- if (!is.na(THETAFIX[i]) && THETAFIX[i] == 1 && bWriteFIX) " FIX" else ""
    strinput_theta <- c(strinput_theta, paste0("$THETA ", THETA[i], strFIX, theta_comment[i]))
  }
  
  line <- safeFindRecord(line, record = "\\$THETA", replace = strinput_theta)
  line <- safeFindRecord(line, record = "\\$OMEGA", replace = newommatrix)
  
  if (strUpdateType != "NoData") {
    line <- safeFindRecord(line, record = "\\$DATA", replace = paste0("$DATA ", basename(strNewFREMData), " IGNORE=@"))
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
