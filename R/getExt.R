#' Read the information in a NONMEM ext file
#'
#' @description
#' Extracts the NONMEM iteration information from a NONMEM .ext file.
#'
#' If more that one $ESTIMATION is present in a NONMEM run, then each one will
#' give rise to a "TABLE". The `set` argument can specify which of these tables
#' to extract the information from. The default is to extract the information

#' from the last TABLE.
#'
#' @param extFile The name of the .ext file.
#' @param set The TABLE in the file the iteration information should be
#'   extracted from. Will use the last TABLE if set to \code{NULL}. There is one
#'   TABLE for each \code{$ESTIMATION} in the \code{NONMEM} model file.
#'
#' @return A data.frame with the data from the specified TABLE (or the last if
#'   not specified).
#' @export
#'
#' @examples
#'
#' ## Read the complete information from the last TABLE in an .ext file.
#' extFile <- system.file("extData/SimNeb/run30.ext", package = "PMXFrem")
#' dfExt <- getExt(extFile = extFile)
#'
#' ## Extract the final parameter estimates
#' subset(getExt(extFile = extFile), ITERATION == "-1000000000")
#'
getExt <- function(extFile, set = NULL) {

  tmp   <- scan(extFile, what = "character", sep = "\n", quiet = TRUE)
  tabs  <- grep("TABLE", tmp)
  if (is.null(set)) set <- length(tabs)

  if (set == 1 & length(tabs) == 1) { # Only one set of results
    myext <- read.table(extFile, skip = 1, h = T)
  } else if (set == 1 & length(tabs) > 1) {
    myext <- read.table(extFile, skip = 1, nrows = tabs[2] - 3, h = T)
  } else if (set == 2 & length(tabs) == 2) {
    myext <- read.table(extFile, skip = tabs[2], h = T)
  } else if (set == 2 & length(tabs) == 3) {
    myext <- read.table(extFile, skip = tabs[2], nrows = length(tmp) - tabs[2] - (length(tmp) - tabs[3]) - 2, h = T)
  } else if (set == 3 & length(tabs) == 3) {
    myext <- read.table(extFile, skip = tabs[3], h = T)
  } else if (set == 4 & length(tabs) == 4) {
    myext <- read.table(extFile, skip = tabs[4], h = T)
  }

  return(myext)
}
