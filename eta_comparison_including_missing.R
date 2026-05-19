library(dplyr)
library(PMXFrem)

# --- 1. Define Paths & Data ---
modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
fremMod   <- "run31"
ffemMod   <- "run31max1-2"
testData  <- read.csv(file.path(modDevDir, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv")) %>% filter(BLQ != 1)

# Filter for Informative Subjects
valid_ids <- testData %>% filter(EVID == 0) %>% pull(ID) %>% unique()

cat("\n=====================================================================\n")
cat(sprintf("   3-WAY ETA COMPARISON (Filtered to %d Informative Subjects)\n", length(valid_ids)))
cat("   Now using PMXFrem::calcFFEM() for Bayesian Projection\n")
cat("=====================================================================\n\n")

# --- 2. METHOD A: calcFFEM() Projection (Bayesian) ---
ext_data_31  <- getExt(file.path(modDevDir, paste0(fremMod, ".ext")))
final_est_31 <- tail(ext_data_31[ext_data_31$ITERATION == -1000000000, ], 1)

covNames <- getCovNames(file.path(modDevDir, paste0(fremMod, ".mod")))$covNames

phi_31 <- getPhi(file.path(modDevDir, paste0(fremMod, ".phi")))
names(phi_31) <- gsub("\\.", "", names(phi_31))

# Identify correct ETA columns in order
eta_cols <- paste0("ETA", 1:max(as.numeric(sub("ETA", "", grep("^ETA[0-9]+$", names(phi_31), value=TRUE)))))
N_base <- 5

# Initialize dataframe
bayesian_etas <- data.frame(ID = phi_31$ID)
for(i in 1:N_base) bayesian_etas[[paste0("ETA", i, "_FFEM")]] <- NA

# Process each subject through calcFFEM
for(r in 1:nrow(phi_31)) {
  frem_eta_vec <- as.numeric(phi_31[r, eta_cols])
  
  res <- calcFFEM(
    dfext = final_est_31,
    numNonFREMThetas = 7,
    numSkipOm = 2,
    covNames = covNames,
    availCov = covNames, 
    fremETA = frem_eta_vec
  )
  
  # Bulletproof dynamic extraction of the return object name
  eta_obj_name <- grep("eta_prim", names(res), ignore.case = TRUE, value = TRUE)
  
  # Ensure we only grab the structural ETAs (in case it returns covariates too)
  bayesian_etas[r, paste0("ETA", 1:N_base, "_FFEM")] <- res[[eta_obj_name]][1:N_base]
}

bayesian_etas <- bayesian_etas %>% filter(ID %in% valid_ids)


# --- 3. METHOD B: Package calcEtas() (Data-Driven Linear) ---
FFEMData <- createFFEMdata(
  runno = 31, modDevDir = modDevDir, numNonFREMThetas = 7, numSkipOm = 2, 
  dataFile = testData, parNames = c("CL", "V", "MAT"), quiet = TRUE
)
pkg_etas <- calcEtas(
  runno = 31, modDevDir = modDevDir, numNonFREMThetas = 7, numSkipOm = 2, 
  FFEMData = FFEMData, quiet = TRUE
)
pkg_etas_clean <- pkg_etas %>% 
  select(ID, all_of(paste0("ETA", 1:N_base, "_PRIM"))) %>%
  rename_with(~ sub("_PRIM", "_CALC", .), everything()) %>%
  filter(ID %in% valid_ids)


# --- 4. METHOD C: MAX1 Reference (Data-Driven Non-Linear) ---
phi_max1 <- getPhi(file.path(modDevDir, paste0(ffemMod, ".phi")))
names(phi_max1) <- gsub("\\.", "", names(phi_max1))
phi_max1_clean <- phi_max1 %>% 
  select(ID, all_of(paste0("ETA", 1:N_base))) %>%
  rename_with(~ paste0(., "_MAX1"), starts_with("ETA")) %>%
  filter(ID %in% valid_ids)


# --- 5. Merge and Compare ---
comp_df <- bayesian_etas %>%
  inner_join(pkg_etas_clean, by = "ID") %>%
  inner_join(phi_max1_clean, by = "ID")

res_calc_vs_ffem <- data.frame()
res_calc_vs_max  <- data.frame()

for(i in 1:N_base) {
  col_ffem <- paste0("ETA", i, "_FFEM")
  col_calc <- paste0("ETA", i, "_CALC")
  col_max  <- paste0("ETA", i, "_MAX1")
  
  # calcEtas vs calcFFEM
  diff_cf <- comp_df[[col_calc]] - comp_df[[col_ffem]]
  res_calc_vs_ffem <- rbind(res_calc_vs_ffem, data.frame(
    Parameter = paste0("ETA", i),
    Correlation = cor(comp_df[[col_calc]], comp_df[[col_ffem]]),
    Mean_Abs_Error = mean(abs(diff_cf)),
    Max_Abs_Diff = max(abs(diff_cf))
  ))
  
  # calcEtas vs MAX1
  diff_cx <- comp_df[[col_calc]] - comp_df[[col_max]]
  res_calc_vs_max <- rbind(res_calc_vs_max, data.frame(
    Parameter = paste0("ETA", i),
    Correlation = cor(comp_df[[col_calc]], comp_df[[col_max]]),
    Mean_Abs_Error = mean(abs(diff_cx)),
    Max_Abs_Diff = max(abs(diff_cx))
  ))
}

cat("--- COMPARISON 1: calcEtas (Data-Driven) vs calcFFEM (Bayesian Projection) ---\n")
print(res_calc_vs_ffem, digits = 5)

cat("\n--- COMPARISON 2: calcEtas (Linear) vs MAX1 (Non-Linear Re-estimation) ---\n")
print(res_calc_vs_max, digits = 5)
cat("\n=====================================================================\n")
