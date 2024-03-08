#' Collect filenames that are associated with a NONMEM run
#'
#' Assembles the model, ext, phi and lst file names for a run based on either
#' run number or model name (the part of the file name to the left of the
#' filename extension).
#'
#' runno and modName can not both be NULL. If both are provided, modName is
#' used.
#'
#' modDevDir can not be NULL.
#'
#' @param runno The run number. Must be provided if `modName` is NULL. Will be
#'   used to create NONMEM model and results file names  with the structure
#'   run#.mod and run#. NONMEM ext, phi and cov files will be assumed to be
#'   named run# followed by the appropriate suffix (where # is `runno`).
#' @param modName The model name. Will be used together with `modExt`, `lstExt`,
#'   ext, phi and cov to form the NONMEM file names, for example
#'   `modName`.mod for the model file.
#' @param modDevDir The directory where the files reside. Default is the current
#'   directory, i.e. ".".
#' @param modExt The model file name extension. Default is ".mod".
#' @param lstExt The NONMEM out file name extension. Default is ".lst".
#'
#' @return A list with components "mod", "ext", "phi" and "lst" specifying the
#'   path names of the corresponding files.
#' @export
#'
#' @examples
#' getFileNames(runno = 30, modDevDir = system.file("extData/SimNeb/", package = "PMXFrem"))
#'
getFileNames <- function(runno     = NULL,
                         modName   = NULL,
                         modDevDir = ".",
                         modExt    = ".mod",
                         lstExt    = ".lst") {

  if (is.null(runno) & is.null(modName)) stop("One of runno and modName needs to have a non-null value.")
  if (is.null(modDevDir)) stop("modDevDir must be a non-NULL value.")

  if (is.null(modName)) {
    modFile <- file.path(modDevDir, paste0("run", runno, modExt))
    extFile <- file.path(modDevDir, paste0("run", runno, ".ext"))
    phiFile <- file.path(modDevDir, paste0("run", runno, ".phi"))
    covFile <- file.path(modDevDir, paste0("run", runno, ".cov"))
    lstFile <- file.path(modDevDir, paste0("run", runno, lstExt))
  } else {
    modFile <- file.path(modDevDir, paste0(modName, modExt))
    extFile <- file.path(modDevDir, paste0(modName, ".ext"))
    phiFile <- file.path(modDevDir, paste0(modName, ".phi"))
    covFile <- file.path(modDevDir, paste0(modName, ".cov"))
    lstFile <- file.path(modDevDir, paste0(modName, lstExt))
  }

  retList <- list(mod = modFile, ext = extFile, phi = phiFile, lst = lstFile, cov = covFile)

  return(retList)
}
