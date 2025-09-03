#' Create traceplots of OFV, tehtas and omegas from a NONMEM run.
#'
#' Use to check for a stable convergence of NONMEM runs, typically for runs
#' implemented with EM algorithms.
#'
#' @inheritParams getFileNames
#' @inheritParams getExt
#' @param startIter The iteration to start the traceplot from. Default is 10.
#' (The first iterations are typically not particularly informative.)
#' @param main The title to use in the generated plots. Default is NULL.
#' @param includeOFV Logical (default is \code{TRUE}). Should the traceplot for OFV be included?
#' @param includeTheta Logical (default is \code{TRUE}). Should the traceplot for the \code{THETA}s be included?
#' @param includeOmega Logical (default is \code{TRUE}). Should the traceplot for the \code{OMEGA}s be included?
#' @param thetaNum The number of the \code{THETA}s to include in the plot. Default is NULL, which means that all \code{THETA}s should be included.
#' @param omegaNum The number of the \code{OMEGA}s to include in the plot. Default is NULL, which means that all \code{OMEGA}s should be included.
#'
#' @return A list of ggplot objects, one for each of an object of OFV, \code{THETA}s and code{OMEGA}s.
#' @details Each of the parameters and OFV are plotted versus the iteration number.
#' The value on the y-axis is normalised to the last iteration.
#' @export
#'
#' @examples
#' \dontrun{
#'  traceplot(30,modDevDir=system.file("extdata","SimNeb/",package="PMXFrem"))
#' }
traceplot <- function(runno        = NULL,
                      modName      = NULL,
                      modDevDir    = NULL,
                      extFileName  = NULL,
                      set          = 1,
                      startIter    = 10,
                      main         = NULL,
                      includeOFV   = TRUE,
                      includeTheta = TRUE,
                      includeOmega = TRUE,
                      thetaNum     = NULL,
                      omegaNum     = NULL) {

  if (is.null(extFileName)) {
    fileNames   <- getFileNames(runno = runno, modName = modName, modDevDir = modDevDir)
    extFileName <- fileNames$ext
  }

  if (set == "last") {
    tmp  <- scan(extFileName, what = "character", sep = "\n", quiet = TRUE)
    tabs <- grep("TABLE", tmp)
    set  <- length(tabs)
  }

  myext <- getExt(extFile = extFileName, set = set)

  ## Get the starting parameters
  Startpar <- subset(myext,ITERATION==0)

  # Get the finals
  Finpar <- subset(myext,ITERATION==-1000000000)

  # Get the intermediate
  myext  <- subset(myext,ITERATION>0)

  myTrash <- c()
  if (ncol(myext) > 2) {
    for(i in 2:(ncol(myext)-1)) {
      if(sum(myext[,i])==0) myTrash <- c(myTrash,i)
    }
  }

  # Select the parameters
  if(!is.null(myTrash) && length(myTrash) > 0) {
    myext <- myext[,-myTrash]
    Finpar <- Finpar[,-myTrash]
    Startpar <- Startpar[,-myTrash]
  }

  myext2 <- myext
  if (length(Finpar) > 1) {
    for (i in 2:(length(Finpar)-1)) {
      myext2[,i] <- myext[,i]/as.numeric(Finpar[i])
    }
  }

  if("SAEMOBJ" %in% names(myext2)) myext2 <- myext2 %>% dplyr::rename(OBJ =SAEMOBJ)

  myextlong <- myext2 %>% tidyr::gather("Parameter","Value",-ITERATION)

  thData <- subset(myextlong,Parameter!="OBJ" & ITERATION > startIter & grepl("THETA",myextlong$Parameter))
  if(!is.null(thetaNum)) thData <- subset(thData,Parameter %in%paste("THETA",thetaNum,sep=""))

  p1 <- ggplot2::ggplot(thData,ggplot2::aes(ITERATION,Value,color=Parameter,group=Parameter)) +
    ggplot2::geom_line(show.legend=FALSE) +
    ggplot2::geom_hline(yintercept = 1) +
    ggplot2::facet_wrap(~Parameter) +
    ggplot2::ggtitle(main)

  omData <- subset(myextlong,Parameter!="OBJ" & ITERATION > startIter & grepl("OMEGA",myextlong$Parameter))
  if(!is.null(omegaNum)) omData <- subset(omData,Parameter %in%paste0("OMEGA.",omegaNum,".",omegaNum,"."))

  p1a <- ggplot2::ggplot(omData,ggplot2::aes(ITERATION,Value,color=Parameter,group=Parameter)) +
    ggplot2::geom_line(show.legend=FALSE) +
    ggplot2::facet_wrap(~Parameter)

  p2 <- ggplot2::ggplot(subset(myextlong,Parameter=="OBJ" & ITERATION > startIter ),ggplot2::aes(ITERATION,Value,color=Parameter,group=Parameter)) +
    ggplot2::geom_line(show.legend=FALSE) +
    ggplot2::geom_point(show.legend=FALSE) +
    ggplot2::ylab("OFV")+
    ggplot2::ggtitle(main)

  retList <- list()
  if(includeOFV) retList[["OFV"]]     <- p2
  if(includeTheta) retList[["Theta"]] <- p1
  if(includeOmega) retList[["Omegas"]]  <- p1a
  return(retList)
}
