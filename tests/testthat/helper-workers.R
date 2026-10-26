#' Is this session running an installed PMXFrem, not the source tree?
#'
#' An installed package has `Meta/package.rds` in its folder; the source tree
#' that `devtools::load_all()` / `testthat::test_local()` runs does not. R CMD
#' check and an installed-package test run are the installed case.
pmxfremIsInstalled <- function() {
  file.exists(file.path(getNamespaceInfo("PMXFrem", "path"), "Meta", "package.rds"))
}

#' Skip a test whose parallel workers would load a different PMXFrem
#'
#' Where doParallel cannot fork (Windows) `.fremStartWorkers()` starts fresh R
#' sessions, and those load PMXFrem from the library path. If this session is
#' running the source tree through `load_all()`, they get the installed copy,
#' whose version differs, and `.fremStartWorkers()` stops on the mismatch - as
#' it should. That says nothing about the code under test, so the test is
#' skipped.
#'
#' It skips only in that case. Where workers are forked (Linux, macOS) they are
#' copies of this session and share its loaded PMXFrem, and under R CMD check
#' the session runs the installed PMXFrem the workers load: both run the test.
#' The default for `fresh` is read from `.fremStartWorkers()` itself, so the
#' two cannot disagree about which platforms start fresh sessions.
skip_if_workers_load_source_tree <- function() {
  fresh <- eval(formals(.fremStartWorkers)$fresh)
  testthat::skip_if(
    isTRUE(fresh) && !pmxfremIsInstalled(),
    "parallel workers would load the installed PMXFrem, not the source tree under test"
  )
}
