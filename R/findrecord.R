#' Find/replace a record in a NONMEM model file.
#'
#' @param input The filename of the NONMEM model file or a character vector of
#'   with lines of a model file.
#' @param record The NONMEM record to replace/find, note all of the records
#'   starting with "record" will be replaced/found
#' @param replace string vector to replace `record` with (default=NULL, no
#'   replacement)
#' @param quiet verbose (quiet=F) or not (quiet=T, default)
#'
#' @return The new model file including the replacement (if replace!=NULL),
#'   otherwise the found record(s)
#' @export
#'
#' @examples
#'
#' modFile <- system.file("extData/SimNeb/run30.mod", package = "PMXFrem")
#'
#' ## Return the $PROBLEM record
#' tmp <- findrecord(modFile, record = "\\$PROBLEM", quiet = TRUE)
#'
#' ## Return the new model with $PROBLEM set to FFEM model
#' tmp <- findrecord(modFile, record = "\\$PROBLEM", replace = "$PROBLEM FFEM model", quiet = TRUE)
#'
findrecord <- function(input,
                       record  = "\\$OMEGA",
                       replace = NULL,
                       quiet   = TRUE) {

  if (length(input) == 1) {
    con  <- file(input, open = "r")
    line <- readLines(con)
  } else {
    line <- input
  }

  start <- NULL
  stop  <- NULL

  for (i in 1:length(line)) {
    tmp <- grep(paste0("^", record, ".*"), line[i])
    if (!is.null(start)) {
      if (length(tmp) == 0) {
        tmp1 <- grep(paste0("^\\$.*"), line[i])
        if (length(tmp1) != 0) {
          stop <- i - 1
          break
        }
      }
    } else {
      if (length(tmp) > 0) {
        start <- i
      }
    }
  }

  if (is.null(stop)) stop <- length(line)
  if (!quiet && is.null(start) == FALSE && is.null(stop) == FALSE) print(line[start:stop])

  # Replace some text
  if (!is.null(replace) && is.null(start) == FALSE && is.null(stop) == FALSE) {
    newtext <- c()
    if (start > 1) newtext <- line[1:(start - 1)]
    newtext <- c(newtext, replace)
    if (stop < length(line)) newtext <- c(newtext, line[(stop + 1):length(line)])
    if (!quiet) print(newtext)
    if (length(input) == 1) close(con)
    return(newtext)
  }
  if (length(input) == 1) close(con)
  if (is.null(start) == FALSE && is.null(stop) == FALSE) {
    return(line[start:stop])
  }
}
