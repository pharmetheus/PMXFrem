#' getPhi
#'
#' @description Extracts the  information from a NONMEM .ext file.
#' @param phiFile The name of the .phi file.
#' @inheritParams getExt
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' dfPhi    <- getPhi(phiFile = "run10.phi")
#' }
getPhi <- function(phiFile,set=NULL) {
  
  tmp   <- scan(phiFile,what="character",sep="\n",quiet=TRUE)
  tabs  <- grep("TABLE",tmp)
  if(is.null(set)) set <- length(tabs)
  
  if(set==1 & length(tabs)==1) { # Only one set of results
    myphi <- read.table(phiFile,skip=1,h=T)
  } else if(set== 1 & length(tabs)>1) {
    myphi <- read.table(phiFile,skip=1,nrows=tabs[2]-3,h=T)
  } else if(set==2 & length(tabs)==2) {
    myphi <- read.table(phiFile,skip=tabs[2],h=T)
  } else if(set==2 & length(tabs)==3) {
    myphi <- read.table(phiFile,skip=tabs[2],nrows=length(tmp)-tabs[2]-(length(tmp)-tabs[3])-2,h=T)
  } else if(set==3 & length(tabs)==3) {
    myphi <- read.table(phiFile,skip=tabs[3],h=T)
  } else if(set==4 & length(tabs)==4) {
    myphi <- read.table(phiFile,skip=tabs[4],h=T)
  }
  
  return(myphi)
}