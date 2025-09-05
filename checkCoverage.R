 ## It is necessary to set NO_CRAN="true" to get all code paths tested
 Sys.setenv(NOT_CRAN = "true")
 cov2 <- covr::package_coverage(quiet=FALSE)
 covr::report(cov2)

 covr::file_coverage(
   source_files = "R/calcFFEM.R",
   test_files = "tests/testthat/test-calcFFEM.R"
 )
