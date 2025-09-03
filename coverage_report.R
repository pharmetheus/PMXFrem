# --- coverage_report.R (DIAGNOSTIC VERSION) ---

# 1. Get all source files, excluding the one for visual tests
all_source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
source_files_to_check <- all_source_files[!grepl("plotExplainedVar\\.R$", all_source_files)]

# Initialize a list to store individual coverage objects
coverage_list <- list()
coverage_sources <- c()

message("Calculating true file-specific coverage (Diagnostic Mode)...")

# 2. Loop through each source file
for (source_file in source_files_to_check) {
  test_file <- file.path("tests", "testthat", paste0("test-", basename(source_file)))

  if (file.exists(test_file)) {
    message(paste(" -> Analyzing:", basename(source_file)))

    file_cov <- try(
      covr::file_coverage(
        source_files = source_file,
        test_files = test_file
      ),
      silent = TRUE
    )

    if (inherits(file_cov, "coverage")) {
      coverage_list <- c(coverage_list, list(file_cov))
      coverage_sources <- c(coverage_sources, source_file)
    } else {
      warning(paste("Failed to calculate coverage for:", basename(source_file)))
    }

  } else {
    warning(paste("No corresponding test file found for:", basename(source_file)))
  }
}

# 3. Combine the objects one-by-one to find the failure
if (length(coverage_list) > 1) {
  message("\n--- Combining coverage objects to find the issue ---")

  # Start with the first valid object
  combined_coverage <- coverage_list[[1]]
  message(paste("Successfully initialized with:", basename(coverage_sources[1])))

  # Loop through the rest of the objects
  for (i in 2:length(coverage_list)) {
    message(paste("Adding coverage from:", basename(coverage_sources[i])))

    # This is the step that is failing
    combined_coverage <- combined_coverage + coverage_list[[i]]

    message("... Success!")
  }

  # If the loop finishes, we can generate the report
  message("\nDone. Opening the final report...")
  covr::report(combined_coverage)

} else {
  message("Not enough valid coverage objects to combine.")
}
