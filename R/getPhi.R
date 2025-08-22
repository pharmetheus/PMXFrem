#' Read the information in a NONMEM phi file
#'
#' @description
#' Extracts the NONMEM iteration information from a NONMEM .phi file.
#'
#' If more that one $ESTIMATION is present in a NONMEM run, then each one will
#' give rise to a "TABLE". The `set` argument can specify which if these tables
#' to extract the information from. The default is to extract the information
#' from the last TABLE.
#'
#' @param phiFile The name of the .phi file.
#' @param set The TABLE in the file the iteration information should be
#'   extracted from. Will use the last TABLE if set to \code{NULL}. There is one
#'   TABLE for each \code{$ESTIMATION} in the \code{NONMEM} model file.
#' @param warn Logical, if a warning should be issued if PHI is part of the
#'   column names in the phi-file.
#'
#' @return A data.frame with the data from the specified TABLE (or the last if
#'   not specified).
#' @export
#'
#' @examples
#' ## Read the complete information from the last TABLE in a .phi file.
#' phiFile <- system.file("extData/SimNeb/run30.phi", package = "PMXFrem")
#' dfPhi <- getPhi(phiFile = phiFile)
#'
getPhi <- function(phiFile, set = NULL, warn=T) {

  tmp   <- scan(phiFile, what = "character", sep = "\n", quiet = TRUE)
  tabs  <- grep("TABLE", tmp)
  if (is.null(set)) set <- length(tabs)

  if (set == 1 & length(tabs) == 1) { # Only one set of results
    myphi <- read.table(phiFile, skip = 1, h = T)
  } else if (set == 1 & length(tabs) > 1) {
    myphi <- read.table(phiFile, skip = 1, nrows = tabs[2] - 3, h = T)
  } else if (set == 2 & length(tabs) == 2) {
    myphi <- read.table(phiFile, skip = tabs[2], h = T)
  } else if (set == 2 & length(tabs) == 3) {
    myphi <- read.table(phiFile, skip = tabs[2], nrows = length(tmp) - tabs[2] - (length(tmp) - tabs[3]) - 2, h = T)
  } else if (set == 3 & length(tabs) == 3) {
    myphi <- read.table(phiFile, skip = tabs[3], h = T)
  } else if (set == 4 & length(tabs) == 4) {
    myphi <- read.table(phiFile, skip = tabs[4], h = T)
  }

  ## Check to see if PHITYPE=1 has been used.
  if(warn) {
    if(any(!is.na(stringr::str_match(names(myphi),"PHI"))))  {
      warning("The phi-file appears to have been generated without PHITYPE=1.")
    }
  }

  return(myphi)
}
