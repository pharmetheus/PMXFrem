#' Start the parallel workers for a `%dopar%` loop
#'
#' PMXForest's `startParallelWorkers()`, with PMXFrem as the package the
#' workers must load and PMXFrem's own option for `fresh`. On Windows, where
#' doParallel cannot fork, the workers are fresh R sessions; they get the
#' folders this session loaded its packages from first on their library path,
#' load `pkgs`, and stop the run if any package's version differs from the
#' session's. See `?PMXForest::startParallelWorkers`.
#'
#' @param ncores Number of workers.
#' @param pkgs Packages the loop needs on the workers.
#' @param fresh Start fresh worker sessions instead of forking. Defaults to
#'   `TRUE` on Windows, where doParallel cannot fork. The
#'   `PMXFrem.freshWorkers` option sets it elsewhere, for testing.
#'
#' @return A function that stops the workers, to be called on exit. Its
#'   `cluster` attribute holds the workers, for tests.
#' @noRd
.fremStartWorkers <- function(ncores, pkgs = "PMXFrem",
                              fresh = getOption("PMXFrem.freshWorkers", .Platform$OS.type == "windows")) {
  PMXForest::startParallelWorkers(ncores, pkgs = pkgs, fresh = fresh)
}
