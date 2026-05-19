library(dplyr)
library(PMXFrem)

# --- 1. Define Paths & Data ---
modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
fremMod   <- "run31"
ffemMod   <- "run31max1-2"
testData  <- read.csv(file.path(modDevDir, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")) %>% filter(BLQ != 1)

valid_ids <- testData %>% filter(EVID == 0) %>% pull(ID) %>% unique()

# --- 2. Extract Population OMEGAs (Denominators) ---
N_base <- 5
covNames <- getCovNames(file.path(modDevDir, paste0(fremMod, ".mod")))$covNames

ext_data_31 <- getExt(file.path(modDevDir, paste0(fremMod, ".ext")))
final_est_31 <- tail(ext_data_31[ext_data_31$ITERATION == -1000000000, ], 1)

pop_ffem <- calcFFEM(dfext = final_est_31, numNonFREMThetas = 7, numSkipOm = 2, covNames = covNames, availCov = covNames, fremETA = NULL)
var_obj_name <- grep("FULLVAR", names(pop_ffem), ignore.case = TRUE, value = TRUE)
omega_diag_frem <- diag(pop_ffem[[var_obj_name]])[1:N_base]

ext_data_max1 <- getExt(file.path(modDevDir, paste0(ffemMod, ".ext")))
final_est_max1 <- tail(ext_data_max1[ext_data_max1$ITERATION == -1000000000, ], 1)
omega_diag_max1 <- numeric(N_base)
for(i in 1:N_base) {
  col_name <- grep(paste0("OMEGA[^0-9]*", i, "[^0-9]+", i, "[^0-9]*$"), names(final_est_max1), value = TRUE)
  if(length(col_name) == 1) omega_diag_max1[i] <- as.numeric(final_est_max1[[col_name]])
}

# --- 3. Compute T_mat for ETC Projection ---
om_cols_31 <- grep("OMEGA", names(final_est_31), value = TRUE)
N_eta_frem <- max(as.numeric(sub(".*OMEGA[^0-9]*([0-9]+).*", "\\1", om_cols_31)), na.rm = TRUE)

Omega_31 <- matrix(0, N_eta_frem, N_eta_frem)
for (i in 1:N_eta_frem) {
  for (j in 1:i) {
    col_name <- grep(paste0("OMEGA[^0-9]*", i, "[^0-9]+", j, "[^0-9]*$"), names(final_est_31), value = TRUE)
    if (length(col_name) == 1) {
      val <- as.numeric(final_est_31[[col_name]])
      Omega_31[i, j] <- val; Omega_31[j, i] <- val
    }
  }
}
Om_str_cov <- Omega_31[1:N_base, (N_base+1):N_eta_frem]
Om_cov_cov <- Omega_31[(N_base+1):N_eta_frem, (N_base+1):N_eta_frem]
C_mat <- Om_str_cov %*% solve(Om_cov_cov)
T_mat <- cbind(diag(N_base), -C_mat)

# --- 4. Extract ETAs and ETCs ---
phi_31_raw <- getPhi(file.path(modDevDir, paste0(fremMod, ".phi")))
phi_31_clean <- phi_31_raw
names(phi_31_clean) <- gsub("\\.", "", names(phi_31_clean)) 
eta_cols <- paste0("ETA", 1:N_eta_frem)

# calcFFEM Storage
ffem_etas <- data.frame(ID = valid_ids)
for(i in 1:N_base) {
  ffem_etas[[paste0("ETA", i)]] <- NA
  ffem_etas[[paste0("ETC_diag", i)]] <- NA
}

for(r in 1:nrow(ffem_etas)) {
  subj_id <- valid_ids[r]
  
  # 1. ETA projection
  row_data_clean <- phi_31_clean[phi_31_clean$ID == subj_id, ]
  res <- calcFFEM(dfext = final_est_31, numNonFREMThetas = 7, numSkipOm = 2, covNames = covNames, availCov = covNames, fremETA = as.numeric(row_data_clean[, eta_cols]))
  eta_obj_name <- grep("eta_prim", names(res), ignore.case = TRUE, value = TRUE)
  ffem_etas[r, paste0("ETA", 1:N_base)] <- res[[eta_obj_name]][1:N_base]
  
  # 2. ETC projection
  row_data_raw <- phi_31_raw[phi_31_raw$ID == subj_id, ]
  ETC_mat <- matrix(0, N_eta_frem, N_eta_frem)
  for(i in 1:N_eta_frem) {
    for(j in 1:i) {
      col_pattern <- paste0("^ETC[^0-9]*", i, "[^0-9]+", j, "[^0-9]*$")
      col_name <- grep(col_pattern, names(row_data_raw), value = TRUE)
      if(length(col_name) == 1) {
        val <- as.numeric(row_data_raw[[col_name]])
        ETC_mat[i, j] <- val; ETC_mat[j, i] <- val
      }
    }
  }
  ETC_prim <- T_mat %*% ETC_mat %*% t(T_mat)
  ffem_etas[r, paste0("ETC_diag", 1:N_base)] <- diag(ETC_prim)[1:N_base]
}

# calcEtas Storage
FFEMData <- createFFEMdata(runno = 31, modDevDir = modDevDir, numNonFREMThetas = 7, numSkipOm = 2, dataFile = testData, parNames = c("CL", "V", "MAT"), quiet = TRUE)
pkg_etas <- calcEtas(runno = 31, modDevDir = modDevDir, numNonFREMThetas = 7, numSkipOm = 2, FFEMData = FFEMData, quiet = TRUE) %>% filter(ID %in% valid_ids)

# MAX1 Storage
phi_max1_raw <- getPhi(file.path(modDevDir, paste0(ffemMod, ".phi")))
max1_etas <- data.frame(ID = valid_ids)
for(i in 1:N_base) {
  max1_etas[[paste0("ETA", i)]] <- NA
  max1_etas[[paste0("ETC_diag", i)]] <- NA
}

for(r in 1:nrow(max1_etas)) {
  subj_id <- valid_ids[r]
  row_data <- phi_max1_raw[phi_max1_raw$ID == subj_id, ]
  for(i in 1:N_base) {
    col_name_eta <- grep(paste0("^ETA[^0-9]*", i, "[^0-9]*$"), names(row_data), value=TRUE)
    if(length(col_name_eta) == 1) max1_etas[r, paste0("ETA", i)] <- as.numeric(row_data[[col_name_eta]])
    
    col_name_etc <- grep(paste0("^ETC[^0-9]*", i, "[^0-9]+", i, "[^0-9]*$"), names(row_data), value=TRUE)
    if(length(col_name_etc) == 1) max1_etas[r, paste0("ETC_diag", i)] <- as.numeric(row_data[[col_name_etc]])
  }
}

# --- 5. Extract MAX1_NM from .shk file ---
shk_file <- file.path(modDevDir, paste0(ffemMod, ".shk"))
max1_nm_shk <- list()

if(file.exists(shk_file)) {
  shk_data <- read.table(shk_file, skip = 1, header = TRUE, fill = TRUE)
  shk_eta_cols <- grep("^ETA", names(shk_data), value = TRUE)
  for(t in c(8, 4, 9, 6)) {
    row_data <- shk_data[shk_data$TYPE == t, ]
    if(nrow(row_data) > 0) {
      max1_nm_shk[[paste0("TYPE_", t)]] <- as.numeric(row_data[1, shk_eta_cols[1:N_base]])
    } else {
      max1_nm_shk[[paste0("TYPE_", t)]] <- rep(NA, N_base)
    }
  }
} else {
  for(t in c(8, 4, 9, 6)) max1_nm_shk[[paste0("TYPE_", t)]] <- rep(NA, N_base)
}

# --- 6. HELPER: Exact NONMEM Shrinkage Logic (NO FLOORING) ---
calc_nonmem_shk <- function(eta_vec, etc_vec, omega) {
  eta_clean <- na.omit(eta_vec)
  n <- length(eta_clean)
  
  # 1. NONMEM uses Population Variance (N denominator instead of N-1)
  var_eta <- if(n > 1) var(eta_clean) * ((n - 1) / n) else 0
  mean_etc <- if(all(is.na(etc_vec))) NA else mean(etc_vec, na.rm = TRUE)
  
  # 2. Mathematical derivation av Shrinkage (Inga max(0)-spärrar)
  shk8 <- 1 - (var_eta / omega)
  shk4 <- 1 - sqrt(var_eta / omega)
  
  if(!is.na(mean_etc)) {
    shk9 <- mean_etc / omega
    # Förhindra NaN i R om mean_etc mot förmodan > omega, annars rå siffra
    val_for_sqrt <- 1 - shk9
    shk6 <- if (val_for_sqrt >= 0) 1 - sqrt(val_for_sqrt) else NA
  } else {
    shk9 <- NA; shk6 <- NA
  }
  
  res <- c(shk8, shk4, shk9, shk6) * 100
  # GOLVET ÄR BORTTAGET HÄR. Negativa värden returneras som de är.
  return(res)
}


# --- 7. Compute and Format Output ---
cat("\n")
for(i in 1:N_base) {
  om_frem <- omega_diag_frem[i]
  om_max1 <- omega_diag_max1[i]
  
  tf_shk <- calc_nonmem_shk(ffem_etas[[paste0("ETA", i)]], ffem_etas[[paste0("ETC_diag", i)]], om_frem)
  ce_shk <- calc_nonmem_shk(pkg_etas[[paste0("ETA", i, "_PRIM")]], NA, om_frem)
  mx_shk <- calc_nonmem_shk(max1_etas[[paste0("ETA", i)]], max1_etas[[paste0("ETC_diag", i)]], om_max1)
  
  df_out <- data.frame(
    Shrinkage_Type = c("TYPE 8 (ETA Var)", "TYPE 4 (ETA SD)", "TYPE 9 (EBV Var)", "TYPE 6 (EBV SD)"),
    calcFFEM       = tf_shk,
    calcEtas       = ce_shk,
    MAX1_R         = mx_shk,
    MAX1_NM        = c(max1_nm_shk$TYPE_8[i], max1_nm_shk$TYPE_4[i], max1_nm_shk$TYPE_9[i], max1_nm_shk$TYPE_6[i])
  )
  
  cat(sprintf("--- OM%d:%d ---\n", i, i))
  print(df_out, row.names = FALSE, digits = 5) 
  cat("\n")
}