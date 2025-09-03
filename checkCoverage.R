library(testthat)
# 1. Get all source R files from the R/ directory
# We continue to exclude plotExplainedVar.R as it's measured separately by vdiffr
all_source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
# source_files <- all_source_files[!grepl("plotExplainedVar\\.R$", all_source_files)]
source_files <- all_source_files

# 2. Construct the conventional name for the corresponding test files
test_files <- file.path("tests", "testthat", paste0("test-", basename(source_files)))

# 3. (Optional but recommended) Check which test files actually exist
# This makes the command more robust if a test file is missing
existing_tests_mask <- file.exists(test_files)
if (!all(existing_tests_mask)) {
  missing_tests <- basename(test_files[!existing_tests_mask])
  warning("Could not find the following test files:\n", paste(missing_tests, collapse = "\n"))
}

# 4. Generate the report for the files where a corresponding test exists
covr::report(
  covr::file_coverage(
    source_files = source_files[existing_tests_mask],
    test_files = test_files[existing_tests_mask]
  )
)


covr::report(
  covr::file_coverage(
    source_files = "R/createFREMdata.R",
    test_files = "tests/testthat/test-createFREMData.R"
  )
)



 cov <- covr::package_coverage(quiet=FALSE)
 covr::report(cov)

 Sys.setenv(NOT_CRAN = "true")
 cov2 <- covr::package_coverage(quiet=FALSE)
 covr::report(cov2)

 devtools::test()

 covr::file_coverage(
   source_files = "R/plotExplainedVar.R",
   test_files = "tests/testthat/test-plotExplainedVar.R"
 )
