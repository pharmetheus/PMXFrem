#' @family FREM model management Internal
#' @concept frem_model_management
#' @keywords internal
createMockExt <- function(baseExtFile, mockExtFile, initialCovariateInfo, dummySigma = 1e-7) {
  
  # 1. Read data using PMXForest
  ext_df <- PMXForest::getExt(baseExtFile, set = 1)
  
  # 2. Fix mangled column names
  lines <- readLines(baseExtFile)
  table_header_line <- lines[1] 
  raw_headers <- strsplit(trimws(lines[2]), "\\s+")[[1]]
  names(ext_df) <- raw_headers
  
  # Extract BOTH the estimates row and the fixed status row
  final_row <- ext_df[ext_df$ITERATION == -1000000000, ][1, , drop = FALSE]
  fix_row   <- ext_df[ext_df$ITERATION == -1000000006, ][1, , drop = FALSE]
  
  # Fallback just in case the base model had no fixed parameters at all
  # (R returns an NA row if the subset fails to find a match before taking [1, ])
  if (is.na(fix_row$ITERATION[1])) {
    fix_row <- final_row
    fix_row[1, ] <- 0
    fix_row$ITERATION <- -1000000006
  }
  
  # --- 3. Expand THETAs for all covariates ---
  theta_cols <- grep("THETA", names(final_row), value = TRUE)
  base_theta_count <- length(theta_cols)
  
  new_theta_cols <- character()
  new_theta_vals <- numeric()
  new_theta_fix  <- numeric()
  
  for (i in seq_along(initialCovariateInfo)) {
    covInfo <- initialCovariateInfo[[i]]
    new_theta_cols <- c(new_theta_cols, paste0("THETA", base_theta_count + i))
    new_theta_vals <- c(new_theta_vals, covInfo$mean)
    new_theta_fix  <- c(new_theta_fix, ifelse(isTRUE(covInfo$shouldFixTheta), 1, 0))
  }
  
  # --- 4. Expand SIGMAs (Only ONE dummy sigma is needed for the covariate block) ---
  sigma_cols <- grep("SIGMA", names(final_row), value = TRUE)
  max_sig <- if (length(sigma_cols) > 0) max(as.numeric(gsub("SIGMA\\(([0-9]+),[0-9]+\\)", "\\1", sigma_cols))) else 0
  new_sig_max <- max_sig + 1
  
  new_sigma_cols <- character()
  new_sigma_vals <- numeric()
  new_sigma_fix  <- numeric()
  for (i in 1:new_sig_max) {
    new_sigma_cols <- c(new_sigma_cols, paste0("SIGMA(", new_sig_max, ",", i, ")"))
    new_sigma_vals <- c(new_sigma_vals, ifelse(i == new_sig_max, dummySigma, 0))
    new_sigma_fix  <- c(new_sigma_fix, 1) # Dummy SIGMAs are fully FIXED
  }
  
  # --- 5. Expand OMEGAs (Lower Triangle) for all covariates ---
  omega_cols <- grep("OMEGA", names(final_row), value = TRUE)
  max_om <- if (length(omega_cols) > 0) max(as.numeric(gsub("OMEGA\\(([0-9]+),[0-9]+\\)", "\\1", omega_cols))) else 0
  
  new_omega_cols <- character()
  new_omega_vals <- numeric()
  new_omega_fix  <- numeric()
  
  for (i in seq_along(initialCovariateInfo)) {
    current_om <- max_om + i
    covInfo <- initialCovariateInfo[[i]]
    
    for (j in 1:current_om) {
      new_omega_cols <- c(new_omega_cols, paste0("OMEGA(", current_om, ",", j, ")"))
      # Diagonal gets variance, off-diagonals initialized to 0
      new_omega_vals <- c(new_omega_vals, ifelse(j == current_om, covInfo$variance, 0))
      new_omega_fix  <- c(new_omega_fix, 0) # Covariances are ESTIMATED
    }
  }
  
  # --- 6. Assemble the New Rows ---
  new_row_est <- data.frame(ITERATION = -1000000000, check.names = FALSE)
  new_row_fix <- data.frame(ITERATION = -1000000006, check.names = FALSE)
  
  # THETA
  for (col in theta_cols) {
    new_row_est[[col]] <- final_row[[col]]
    new_row_fix[[col]] <- fix_row[[col]]
  }
  for (i in seq_along(new_theta_cols)) {
    new_row_est[[new_theta_cols[i]]] <- new_theta_vals[i]
    new_row_fix[[new_theta_cols[i]]] <- new_theta_fix[i]
  }
  
  # SIGMA
  for (col in sigma_cols) {
    new_row_est[[col]] <- final_row[[col]]
    new_row_fix[[col]] <- fix_row[[col]]
  }
  for (i in seq_along(new_sigma_cols)) {
    new_row_est[[new_sigma_cols[i]]] <- new_sigma_vals[i]
    new_row_fix[[new_sigma_cols[i]]] <- new_sigma_fix[i]
  }
  
  # OMEGA
  for (col in omega_cols) {
    new_row_est[[col]] <- final_row[[col]]
    new_row_fix[[col]] <- fix_row[[col]]
  }
  for (i in seq_along(new_omega_cols)) {
    new_row_est[[new_omega_cols[i]]] <- new_omega_vals[i]
    new_row_fix[[new_omega_cols[i]]] <- new_omega_fix[i]
  }
  
  if ("OBJ" %in% names(final_row)) {
    new_row_est[["OBJ"]] <- final_row[["OBJ"]]
    new_row_fix[["OBJ"]] <- 0
  }
  
  # --- 7. Format and Write ---
  format_sci <- function(x) sprintf("%13.5E", as.numeric(x))
  for (col in names(new_row_est)) {
    if (col != "ITERATION") {
      new_row_est[[col]] <- format_sci(new_row_est[[col]])
      new_row_fix[[col]] <- format_sci(new_row_fix[[col]])
    }
  }
  new_row_est[["ITERATION"]] <- " -1000000000"
  new_row_fix[["ITERATION"]] <- " -1000000006"
  
  writeLines(table_header_line, mockExtFile)
  out_df <- rbind(new_row_est, new_row_fix)
  suppressWarnings(write.table(out_df, file = mockExtFile, append = TRUE, quote = FALSE, row.names = FALSE, col.names = TRUE, sep = "  "))
  
  return(mockExtFile)
}
