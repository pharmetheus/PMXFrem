#' Create traceplots of OFV, thetas and omegas from a NONMEM run.
#'
#' Use to check for a stable convergence of NONMEM runs, typically for runs
#' implemented with EM algorithms.
#'
#' @inheritParams getFileNames
#' @inheritParams getExt
#' @param startIter The iteration to start the traceplot from. Default is 10.
#' @param main The title to use in the generated plots. Default is NULL.
#' @param includeOFV Logical (default is \code{TRUE}). Should the traceplot for OFV be included?
#' @param includeTheta Logical (default is \code{TRUE}). Should the traceplot for the \code{THETA}s be included?
#' @param includeOmega Logical (default is \code{TRUE}). Should the traceplot for the \code{OMEGA}s be included?
#' @param thetaNum The number of the \code{THETA}s to include in the plot. Default is NULL, which means that all \code{THETA}s should be included.
#' @param omegaNum The number of the \code{OMEGA}s to include in the plot. Default is NULL, which means that all \code{OMEGA}s should be included.
#' @param includeShapedOFV Logical (default is \code{TRUE}). If TRUE, adds a shaded
#'   acceptance region to the OFV plot based on the chi-squared distribution to
#'   help visualize convergence stability.
#' @param pvalue The p-value (default is 0.05) for the chi-squared distribution
#'   used to define the height of the shaded region. Must be between 0 and 1.
#' @param df The degrees of freedom (default is 1) for the chi-squared distribution.
#' @param meanShapeLastIter The number of final iterations (default is 30) used to
#'   calculate the mean OFV, which centers the shaded region.
#'
#' @return A list of ggplot objects, one for each of an object of OFV, \code{THETA}s and \code{OMEGA}s.
#' @details Each of the parameters and OFV are plotted versus the iteration number.
#' The value on the y-axis is normalised to the last iteration. The shaded region
#' on the OFV plot represents the expected range of variability for the OFV at
#' the end of a run.
#' @export
#'
#' @examples
#' model_dir <- system.file("extdata/SimNeb/", package = "PMXFrem")
#'
#' # Generate plot with the default shaded OFV region
#' trace_plots <- traceplot(
#'   runno = 31,
#'   modDevDir = model_dir
#' )
#'
#' names(trace_plots)
#'
#' if (interactive()) {
#'   trace_plots$OFV
#' }
#'
#' # Generate plot without the shaded OFV region
#' trace_plots_no_shape <- traceplot(
#'   runno = 31,
#'   modDevDir = model_dir,
#'   includeShapedOFV = FALSE
#' )
#'
#' if (interactive()) {
#'   trace_plots_no_shape$OFV
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
                      omegaNum     = NULL,
                      includeShapedOFV = TRUE,
                      pvalue = 0.05,
                      df = 1,
                      meanShapeLastIter=30) {

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
  #Error checks for dOFV shape arguments
  if (includeShapedOFV) {
    if (pvalue<0 || pvalue>1) stop("Chi-square p-value (pvalue) should be in the interval [0-1]")
    if (df<0) stop("Chi-square degreess of freedom should >0")
    if (meanShapeLastIter<1) stop("The number of last iterations used for the mean OFV calculations > 0")
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

  dfofv<-subset(myextlong,Parameter=="OBJ" & ITERATION > startIter )
  p2 <- ggplot2::ggplot(dfofv,ggplot2::aes(ITERATION,Value,color=Parameter,group=Parameter))
  if (includeShapedOFV==TRUE) {
    meanobj<-mean(dfofv[max(1,nrow(dfofv)-meanShapeLastIter+1):nrow(dfofv),]$Value)
    chisq<-qchisq(df = df,p=pvalue,lower.tail = FALSE)
    p2<-p2+ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymax=meanobj+chisq/2,ymin=meanobj-chisq/2,alpha=0.2,color="lightgrey") 
  }
    
  p2<-p2+ggplot2::geom_line(show.legend=FALSE) +
  ggplot2::geom_point(show.legend=FALSE) +
  ggplot2::ylab("OFV")+
  ggplot2::ggtitle(main)

  
  retList <- list()
  if(includeOFV) retList[["OFV"]]     <- p2
  if(includeTheta) retList[["Theta"]] <- p1
  if(includeOmega) retList[["Omegas"]]  <- p1a
  return(retList)
}
