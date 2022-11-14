#' getFileNames
#'
#' @description Assembles the model, ext, phi and lst file names for a run based on either run number or model name (the part of the file name to the left of the filename extension).
#' @param runno The run number.
#' @param modName The model name. Default is the current directory, i.e. ".".
#' @param modDevDir The directory where the files reside.
#' @param modExt The model file name extension. Default is ".mod".
#' @param lstExt The NONMEM out file name extension. Default is ".lst".
#' @details 
#' runno and modName can not both be NULL. If both are provided, modName is used.
#' 
#' modDevDir can not be NULL. 
#' @return
#' A list with components "mod", "ext", "phi" and "lst" specifying the path names of the corresponding files.
#' @export
#'
#' @examples
#' getFilenNames(modName=9)
#' 
getFileNames <- function(runno=NULL,modName=NULL,modDevDir=".",modExt = ".mod",lstExt = ".lst") {
  
  if(is.null(runno) & is.null(modName)) stop("One of runno and modName needs to have a non-null value.")
  if(is.null(modDevDir)) stop("modDevDir must be a non-NULL value.")
  
    if(is.null(modName)) {
      modFile    <- file.path(modDevDir,paste0("run",runno,modExt))
      extFile    <- file.path(modDevDir,paste0("run",runno,".ext"))
      phiFile    <- file.path(modDevDir,paste0("run",runno,".phi"))
      lstFile    <- file.path(modDevDir,paste0("run",runno,lstExt))
    } else {
      modFile    <- file.path(modDevDir,paste0(modName,modExt))
      extFile    <- file.path(modDevDir,paste0(modName,".ext"))
      phiFile    <- file.path(modDevDir,paste0(modName,".phi"))
      lstFile    <- file.path(modDevDir,paste0(modName,lstExt))
    }
  
  
  retList <- list(mod=modFile,ext=extFile,phi=phiFile,lst=lstFile)
  
  return(retList)
}