#' Create a Minimal FREM Model
#'
#' Injects the necessary code for a single FREM covariate into a base NONMEM
#' model to create a minimal, valid FREM model.
#'
#' @param baseModelInfo A list of structural information from `parseBaseModel`.
#' @param initialCovariateInfo A list of lists of details for the covariate(s) being added.
#' @param fremDataPath The path to the minimal FREM dataset.
#' @param fremDataHeaders The column headers of the minimal FREM dataset.
#' @param covEpsNum The EPS() number to use for the covariate error.
#' @param useMuModeling Logical flag to indicate if MU-parameterization should be used.
#'
#' @return A character vector containing the lines of the new minimal FREM model.
#'
#' @keywords internal
createMinimalFremModel <- function(baseModelInfo,
                                   initialCovariateInfo,
                                   fremDataPath,
                                   fremDataHeaders,
                                   covEpsNum,
                                   useMuModeling,
                                   dDefaultCovValue = 1E-05) {
  
  .insert_lines <- function(original_lines, lines_to_insert, insert_at) {
    if (length(lines_to_insert) == 0) return(original_lines)
    c(
      if (insert_at > 1) original_lines[1:(insert_at - 1)] else NULL,
      lines_to_insert,
      if (insert_at <= length(original_lines)) original_lines[insert_at:length(original_lines)] else NULL
    )
  }
  
  line <- baseModelInfo$modelLines
  
  newThetaLines <- c()
  muCovLines <- c()
  fremTypeBlock <- c(";;;FREM CODE BEGIN COMPACT", ";;;DO NOT MODIFY")
  
  current_theta_idx <- baseModelInfo$numThetas + 1
  current_eta_idx   <- baseModelInfo$numOmegas + 1
  num_new_covs      <- length(initialCovariateInfo)
  
  # Isolate only the final BLOCK(N) record and its values for parsing matrix math.
  omega_block_start_line <- grep("BLOCK", baseModelInfo$omegaBlock, ignore.case = TRUE)
  final_omega_block_text <- baseModelInfo$omegaBlock[omega_block_start_line:length(baseModelInfo$omegaBlock)]
  omega_matrix <- parseMatrixBlockToMatrix(final_omega_block_text)
  
  # --- Smart Comment Extraction (Phase 1) ---
  # Scan the full raw file to guarantee skipped omegas are included
  in_omega_phase1 <- FALSE
  all_omega_comments <- c()
  for (l in baseModelInfo$modelLines) {
    if (grepl("^\\s*\\$[A-Za-z]+", l)) in_omega_phase1 <- grepl("^\\s*\\$OMEGA", l, ignore.case = TRUE)
    if (in_omega_phase1 && grepl(";", l)) all_omega_comments <- c(all_omega_comments, trimws(sub("^[^;]*;\\s*", "", l)))
  }
  
  # Expand the OMEGA matrix for the new covariates' ETAs
  new_size_omega <- baseModelInfo$numParCov + num_new_covs
  expanded_omega <- matrix(dDefaultCovValue, nrow = new_size_omega, ncol = new_size_omega)
  if (nrow(omega_matrix) > 0) {
    expanded_omega[1:nrow(omega_matrix), 1:ncol(omega_matrix)] <- omega_matrix
  }
  
  for (i in seq_along(initialCovariateInfo)) {
    covInfo <- initialCovariateInfo[[i]]
    
    # THETA Generation
    theta_fix_text <- if (covInfo$shouldFixTheta) "FIX" else ""
    line_parts <- c("$THETA", covInfo$mean, theta_fix_text, paste0("; ", current_theta_idx, " TV_", covInfo$name))
    newThetaLines <- c(newThetaLines, paste(line_parts[nchar(line_parts) > 0], collapse = " "))
    
    # OMEGA Variance placement on the diagonal
    expanded_omega[baseModelInfo$numParCov + i, baseModelInfo$numParCov + i] <- covInfo$variance
    
    # PK and ERROR blocks
    if (useMuModeling) {
      muCovLines <- c(muCovLines,
                      paste0("      MU_", current_eta_idx, " = THETA(", current_theta_idx, ")"),
                      paste0("      COV", current_eta_idx, " = MU_", current_eta_idx, " + ETA(", current_eta_idx, ")"))
      fremTypeBlock <- c(fremTypeBlock,
                         paste0("      IF(FREMTYPE.EQ.", covInfo$fremType, ") THEN"),
                         paste0(";        ", covInfo$name, " 1"),
                         paste0("         Y = COV", current_eta_idx, " + EPS(", covEpsNum, ")"),
                         paste0("         IPRED = COV", current_eta_idx),
                         "      END IF")
    } else {
      fremTypeBlock <- c(fremTypeBlock,
                         paste0("      IF(FREMTYPE.EQ.", covInfo$fremType, ") THEN"),
                         paste0(";        ", covInfo$name, " 1"),
                         paste0("         Y = THETA(", current_theta_idx, ") + ETA(", current_eta_idx, ") + EPS(", covEpsNum, ")"),
                         paste0("         IPRED = THETA(", current_theta_idx, ") + ETA(", current_eta_idx, ")"),
                         "      END IF")
    }
    
    current_theta_idx <- current_theta_idx + 1
    current_eta_idx   <- current_eta_idx + 1
  }
  
  fremTypeBlock <- c(fremTypeBlock, ";;;FREM CODE END COMPACT")
  
  # --- SPLICING OMEGA Matrix with FIX detection ---
  skipped_omegas_lines <- c()
  if (baseModelInfo$numSkipOm > 0) {
    in_omega_phase1 <- FALSE
    omega_param_lines <- c()
    for (l in baseModelInfo$modelLines) {
      if (grepl("^\\s*\\$[A-Za-z]+", l)) in_omega_phase1 <- grepl("^\\s*\\$OMEGA", l, ignore.case = TRUE)
      if (in_omega_phase1 && grepl("[0-9]", sub(";.*", "", l))) omega_param_lines <- c(omega_param_lines, l)
    }
    
    for (i in 1:baseModelInfo$numSkipOm) {
      param_line <- if (i <= length(omega_param_lines)) omega_param_lines[i] else ""
      
      is_fixed <- grepl("FIX", param_line, ignore.case = TRUE)
      fix_text <- if (is_fixed) " FIX" else ""
      
      # Extract the pure numeric value from the base model line
      clean_line <- sub(";.*", "", param_line)
      clean_line <- gsub("\\$OMEGA|BLOCK\\([0-9]+\\)|FIX", "", clean_line, ignore.case = TRUE)
      val_str <- trimws(clean_line)
      if (val_str == "") val_str <- dDefaultCovValue
      
      skipped_comment <- if (i <= length(all_omega_comments)) {
        paste0(" ; ", all_omega_comments[i])
      } else {
        paste0(" ; ", i, " BSV_SKIP")
      }
      
      skipped_omegas_lines <- c(skipped_omegas_lines, 
                                paste0("$OMEGA BLOCK(1) ", val_str, fix_text, skipped_comment))
    }
  }
  
  # Rebuild the full OMEGA block
  # Force Single Block to prevent sharding when off-diagonals are zero
  new_final_omega_block <- buildmatrix(expanded_omega, strName = "$OMEGA", forceSingleBlock = TRUE)
  
  # --- Re-attach Base Comments and Append New Covariate Comments ---
  for (i in 2:length(new_final_omega_block)) {
    idx <- i - 1 
    # Use global index to offset past the skipped omegas
    global_idx <- baseModelInfo$numSkipOm + idx
    
    if (idx <= baseModelInfo$numParCov) {
      if (global_idx <= length(all_omega_comments)) {
        new_final_omega_block[i] <- paste0(new_final_omega_block[i], " ; ", all_omega_comments[global_idx])
      }
    } else {
      cov_idx <- idx - baseModelInfo$numParCov
      cov_name <- initialCovariateInfo[[cov_idx]]$name
      new_final_omega_block[i] <- paste0(new_final_omega_block[i], " ; ", global_idx, " BSV_", cov_name)
    }
  }
  
  newOmegaBlock <- c(skipped_omegas_lines, new_final_omega_block)
  
  newDataLine <- paste0("$DATA ", basename(fremDataPath), " IGNORE=@")
  newInputLine <- paste0("$INPUT ", paste0(fremDataHeaders, collapse = " "))
  
  # Assemble the model
  line <- .insert_lines(line, newThetaLines, baseModelInfo$lastThetaLine + 1)
  line <- .insert_lines(line, fremTypeBlock, baseModelInfo$fremBlockInsertLine + 1)
  line <- .insert_lines(line, muCovLines, baseModelInfo$muInsertLine + 1)
  
  line <- findrecord(line, "\\$DATA", replace = newDataLine, quiet = TRUE)
  line <- findrecord(line, "\\$INPUT", replace = newInputLine, quiet = TRUE)
  line <- findrecord(line, "\\$OMEGA", replace = newOmegaBlock, quiet = TRUE)
  
  if (!is.null(baseModelInfo$sigmaBlock)) {
    newSigmaLine <- "$SIGMA 0.0000001 FIX"
    newSigmaBlock <- c(baseModelInfo$sigmaBlock, newSigmaLine)
    line <- findrecord(line, "\\$SIGMA", replace = newSigmaBlock, quiet = TRUE)
  }
  
  # Strip out all $TABLE records
  table_start_indices <- grep("^\\s*\\$TAB", line, ignore.case = TRUE)
  if (length(table_start_indices) > 0) {
    lines_to_remove <- c()
    for (start_idx in table_start_indices) {
      end_idx <- length(line)
      next_records <- grep("^\\s*\\$", line)
      subsequent_records <- next_records[next_records > start_idx]
      
      if (length(subsequent_records) > 0) {
        end_idx <- subsequent_records[1] - 1
      }
      lines_to_remove <- c(lines_to_remove, start_idx:end_idx)
    }
    if (length(lines_to_remove) > 0) {
      line <- line[-unique(lines_to_remove)]
    }
  }
  return(line)
}
