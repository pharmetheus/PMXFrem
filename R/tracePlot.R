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
#' @return A list of ggplot objects, one for each of OFV, \code{THETA}s and code{OMEGA}s.
#' @details Each of the parameters and OFV are plotted versus the iteration number.
#' The value on the y-axis is normalised to the last iteration.
#' @export
#'
#' @examples
#' \dontrun
#'  traceplot(30,modDevDir=system.file("extdata","SimNeb/",package="PMXFrem"))
#'  }
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

  fileNames   <- getFileNames(runno = runno, modName = modName, modDevDir = modDevDir)
  extFileName <- fileNames$ext

  tmp   <- scan(extFileName,what="character",sep="\n")
  tabs <- grep("TABLE",tmp)

  if(set=="last") set <- length(tabs)

  if(set==1 & length(tabs)==1) { # Only one set of results
    myext <- read.table(extFileName,skip=1,h=T)
  } else if(set== 1 & length(tabs)>1) {
    myext <- read.table(extFileName,skip=1,nrows=tabs[2]-3,h=T)
  } else if(set==2 & length(tabs)==2) {
    myext <- read.table(extFileName,skip=tabs[2],h=T)
  } else if(set==2 & length(tabs)==3) {
    myext <- read.table(extFileName,skip=tabs[2],nrows=length(tmp)-tabs[2]-(length(tmp)-tabs[3])-2,h=T)
  } else if(set==3 & length(tabs)==4) {
    myext <- read.table(extFileName,skip=tabs[3],nrows=length(tmp)-tabs[3]-(length(tmp)-tabs[4])-2,h=T)
  } else if(set==4 & length(tabs)==4) {
    myext <- read.table(extFileName,skip=tabs[4],h=T)
  }

  #browser()

  ## Get the starting parameters
  Startpar <- subset(myext,ITERATION==0)

  # Get the finals
  Finpar <- subset(myext,ITERATION==-1000000000)

  # Get the intermediate
  myext  <- subset(myext,ITERATION>0)

  myTrash <- c()
  for(i in 2:(ncol(myext)-1)) {
    if(sum(myext[,i])==0) myTrash <- c(myTrash,i)
  }

  # Select the parameters
  if(!is.null(myTrash)) myext <- myext[,-myTrash]
  if(!is.null(myTrash))  Finpar <- Finpar[,-myTrash]
  if(!is.null(myTrash))  Startpar <- Startpar[,-myTrash]


  myext2 <- myext
  for (i in 2:(length(Finpar)-1)) {
    myext2[,i] <- myext[,i]/as.numeric(Finpar[i])
    #myext2[,i] <- myext[,i]/as.numeric(Startpar[i])
  }

  if("SAEMOBJ" %in% names(myext2)) myext2 <- myext2 %>% rename(OBJ =SAEMOBJ)

  myextlong <- myext2 %>% gather("Parameter","Value",-ITERATION)

  thData <- subset(myextlong,Parameter!="OBJ" & ITERATION > startIter & grepl("THETA",myextlong$Parameter))
  if(!is.null(thetaNum)) thData <- subset(thData,Parameter %in%paste("THETA",thetaNum,sep=""))

  p1 <- ggplot(thData,aes(ITERATION,Value,color=Parameter,group=Parameter))
  p1 <- p1 + geom_line(show.legend=FALSE)
  p1 <- p1 + geom_hline(yintercept = 1)
  p1 <- p1 + facet_wrap(~Parameter) + ggtitle(main)

  omData <- subset(myextlong,Parameter!="OBJ" & ITERATION > startIter & grepl("OMEGA",myextlong$Parameter))
  if(!is.null(omegaNum)) omData <- subset(omData,Parameter %in%paste("OMEGA.",omegaNum,".",omegaNum,".",sep=""))

  p1a <- ggplot(omData,aes(ITERATION,Value,color=Parameter,group=Parameter))
  p1a <- p1a + geom_line(show.legend=FALSE)
  p1a <- p1a + facet_wrap(~Parameter)

  p2 <- ggplot(subset(myextlong,Parameter=="OBJ" & ITERATION > startIter ),aes(ITERATION,Value,color=Parameter,group=Parameter))
  p2 <- p2 + geom_line(show.legend=FALSE) + geom_point(show.legend=FALSE)
  p2 <- p2 + ylab("OFV")+ ggtitle(main)

  retList <- list()
  if(includeOFV) retList[["OFV"]]     <- p2
  if(includeTheta) retList[["Theta"]] <- p1
  if(includeOmega) retList[["Omegas"]]  <- p1a
  return(retList)

}
