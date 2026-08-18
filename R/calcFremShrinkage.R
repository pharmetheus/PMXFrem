### FILE: calcFremShrinkage.R ###

#' Calculate Empirical Shrinkage for an FFEM Model
#'
#' @description 
#' Computes the empirical shrinkage of Empirical Bayes Estimates (EBEs) directly from 
#' a MAXEVAL=0 (or MAXEVAL>0) FFEM run. This function entirely replaces the need for 
#' the NONMEM .shk file, allowing diagnostics to be generated natively from the .phi 
#' and .ext files without requiring an estimation step.
#' 
#' It accurately replicates NONMEM's internal shrinkage mathematics, including the 
#' use of population variance (dividing by N rather than N-1) and the exact derivation 
#' of EBV SD shrinkage from EBV Variance shrinkage.
#'
#' @param runno A numeric or character representing the run number of the FFEM model.
#' @param modName A character string representing the model name. If \code{NULL}, 
#'   inferred from \code{runno}.
#' @param modDevDir A character string representing the directory containing the model files.
#' @param dropUninformative Logical. If \code{TRUE} (default), subjects with completely 
#'   uninformative ETAs (where all ETAs are exactly 0) are dropped from the shrinkage 
#'   calculation, mirroring NONMEM's internal handling of missing PK data.
#' @param quiet Logical. If \code{FALSE}, messages about dropped subjects will be printed.
#'
#' @return A data frame containing four types of shrinkage (in percentage) for each ETA:
#' \itemize{
#'   \item \code{Parameter}: The name of the structural ETA.
#'   \item \code{ETA_Var}: ETA Variance Shrinkage (NONMEM Type 8)
#'   \item \code{ETA_SD}: ETA Standard Deviation Shrinkage (NONMEM Type 4)
#'   \item \code{EBV_Var}: EBV Variance Shrinkage based on ETC (NONMEM Type 9)
#'   \item \code{EBV_SD}: EBV Standard Deviation Shrinkage based on ETC (NONMEM Type 6)
#' }
#' @export
#' @family Diagnostics & Plotting
#' @concept diagnostics
#' 
#' @examples
#' # Define the directory containing the model files
#' modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
#' 
#' # Calculate shrinkage directly from the FFEM model files (e.g., run 31)
#' shrinkage_res <- calcFremShrinkage(
#'   runno = "31max0",
#'   modDevDir = modDevDir,
#'   dropUninformative = TRUE,
#'   quiet = TRUE
#' )
#' 
#' print(shrinkage_res)
#' 
calcFremShrinkage <- function(runno = NULL, modName = NULL, modDevDir = NULL, 
                              dropUninformative = TRUE, quiet = TRUE) {
  
  fileNames <- getFileNames(runno = runno, modName = modName, modDevDir = modDevDir)
  
  extFile <- paste0(tools::file_path_sans_ext(fileNames$mod), ".ext")
  phiFile <- paste0(tools::file_path_sans_ext(fileNames$mod), ".phi")
  
  if (!file.exists(extFile)) stop(sprintf("Cannot find .ext file at %s", extFile), call. = FALSE)
  if (!file.exists(phiFile)) stop(sprintf("Cannot find .phi file at %s", phiFile), call. = FALSE)
  
  # --- 1. Extract Population OMEGAs from the .ext file ---
  ext_data <- getExt(extFile)
  
  iter_col <- grep("ITERATION", names(ext_data), ignore.case = TRUE, value = TRUE)
  if (length(iter_col) > 0) {
    final_est <- ext_data[ext_data[[iter_col[1]]] == -1000000000, ]
    if (nrow(final_est) == 0) final_est <- ext_data[ext_data[[iter_col[1]]] == 0, ]
    if (nrow(final_est) == 0) final_est <- tail(ext_data, 1)
    final_est <- tail(final_est, 1)
  } else {
    final_est <- tail(ext_data, 1)
  }
  
  omega_diag <- numeric(0)
  for (i in 1:100) { 
    regex <- paste0("^\\s*OMEGA[^0-9]*", i, "[^0-9]+", i, "[^0-9]*$")
    col_name <- grep(regex, names(final_est), ignore.case = TRUE, value = TRUE)
    
    if (length(col_name) >= 1) {
      omega_diag[i] <- as.numeric(as.character(final_est[[col_name[1]]]))
    } else {
      omega_diag[i] <- NA_real_
    }
  }
  
  while(length(omega_diag) > 0 && is.na(omega_diag[length(omega_diag)])) {
    omega_diag <- omega_diag[-length(omega_diag)]
  }
  
  if(length(omega_diag) == 0) {
    stop("No diagonal OMEGAs could be parsed from the .ext file. Column names: ", 
         paste(names(final_est), collapse = ", "), call. = FALSE)
  }
  
  # --- 2. Extract ETAs and ETCs from the .phi file ---
  phi_data <- getPhi(phiFile)
  
  eta_cols <- grep("^\\s*ETA[^0-9]*[0-9]+[^0-9]*$", names(phi_data), ignore.case = TRUE, value = TRUE)
  eta_indices <- as.numeric(gsub("[^0-9]", "", eta_cols))
  
  if (length(eta_indices) == 0) {
    stop("No ETA columns found in the .phi file.", call. = FALSE)
  }
  
  valid_etas <- sort(unique(eta_indices))
  
  if (dropUninformative) {
    actual_eta_cols <- grep("^\\s*ETA", names(phi_data), ignore.case = TRUE, value = TRUE)
    is_zero <- rowSums(phi_data[, actual_eta_cols, drop = FALSE] == 0) == length(actual_eta_cols)
    if (any(is_zero)) {
      if (!quiet) message(sprintf("Dropping %d uninformative subjects (all ETAs = 0) from Shrinkage calculation.", sum(is_zero)))
      phi_data <- phi_data[!is_zero, ]
    }
  }
  
  # --- 3. Calculate Shrinkages perfectly mirroring NONMEM ---
  res_list <- list(
    Parameter = character(),
    ETA_Var   = numeric(),
    ETA_SD    = numeric(),
    EBV_Var   = numeric(),
    EBV_SD    = numeric()
  )
  
  for (i in valid_etas) {
    if (i > length(omega_diag)) next
    om_val <- omega_diag[i]
    
    if (is.na(om_val) || om_val <= 0) {
      if(!quiet) warning(sprintf("OMEGA variance for ETA %d is NA or <= 0. Skipping.", i))
      next
    }
    
    regex_eta <- paste0("^\\s*ETA[^0-9]*", i, "[^0-9]*$")
    eta_col <- grep(regex_eta, names(phi_data), ignore.case = TRUE, value = TRUE)
    if (length(eta_col) == 0) next
    
    eta_vec <- phi_data[[eta_col[1]]]
    
    regex_etc <- paste0("^\\s*ETC[^0-9]*", i, "[^0-9]+", i, "[^0-9]*$")
    etc_col <- grep(regex_etc, names(phi_data), ignore.case = TRUE, value = TRUE)
    if (length(etc_col) >= 1) {
      etc_vec <- phi_data[[etc_col[1]]]
    } else {
      etc_vec <- rep(NA_real_, length(eta_vec))
    }
    
    eta_clean <- na.omit(eta_vec)
    n <- length(eta_clean)
    
    var_eta <- if (n > 1) var(eta_clean) * ((n - 1) / n) else 0
    mean_etc <- if (all(is.na(etc_vec))) NA_real_ else mean(etc_vec, na.rm = TRUE)
    
    shk8 <- (1 - (var_eta / om_val)) * 100
    shk4 <- (1 - sqrt(var_eta / om_val)) * 100
    
    if (!is.na(mean_etc)) {
      shk9 <- (mean_etc / om_val) * 100
      val_for_sqrt <- 1 - (shk9 / 100)
      shk6 <- if (val_for_sqrt >= 0) (1 - sqrt(val_for_sqrt)) * 100 else NA_real_
    } else {
      shk9 <- NA_real_
      shk6 <- NA_real_
    }
    
    # Clamp negative shrinkages to 0 to mimic NONMEM behavior
    shk8 <- pmax(0, shk8, na.rm = FALSE)
    shk4 <- pmax(0, shk4, na.rm = FALSE)
    shk9 <- pmax(0, shk9, na.rm = FALSE)
    shk6 <- pmax(0, shk6, na.rm = FALSE)
    
    res_list$Parameter <- c(res_list$Parameter, paste0("ETA", i))
    res_list$ETA_Var   <- c(res_list$ETA_Var, round(shk8, 4))
    res_list$ETA_SD    <- c(res_list$ETA_SD, round(shk4, 4))
    res_list$EBV_Var   <- c(res_list$EBV_Var, round(shk9, 4))
    res_list$EBV_SD    <- c(res_list$EBV_SD, round(shk6, 4))
  }
  
  res_df <- as.data.frame(res_list, stringsAsFactors = FALSE)
  return(res_df)
}
