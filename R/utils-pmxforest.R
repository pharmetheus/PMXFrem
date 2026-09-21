## Refuse a PMXForest that is too old, by version rather than by symbol
##
## The parser functions this package builds on have been exported since the
## 1.2.15.900x series, but their arguments changed up to 1.3.0: nmParsePK()
## only takes `keep` from 1.3.0 on. A guard that asks whether nmParsePK()
## *exists* therefore passes on an older install, and the call then fails with
## "unused argument (keep = secReads)", which says nothing about what to do.
##
## `installed` exists so the check itself can be tested; leave it NULL to read
## the installed version.
##
## @noRd
.fremRequirePMXForest <- function(fn, minVersion = "1.3.0", installed = NULL) {
  howTo <- "Update it with remotes::install_github(\"pharmetheus/PMXForest\")."

  if (is.null(installed)) {
    if (!requireNamespace("PMXForest", quietly = TRUE)) {
      stop(fn, "() needs the PMXForest package (>= ", minVersion,
        "), which is not installed. ", howTo,
        call. = FALSE
      )
    }
    installed <- utils::packageVersion("PMXForest")
  }

  if (utils::compareVersion(as.character(installed), minVersion) < 0) {
    stop(fn, "() needs PMXForest >= ", minVersion, "; you have ",
      as.character(installed), ". ", howTo,
      call. = FALSE
    )
  }

  invisible(TRUE)
}
