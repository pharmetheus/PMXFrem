createFFEMmodel2<-function(
    runno         =NULL,
    numNonFREMThetas,
    modName       = NULL,
    modExt        = ".mod",
    lstExt        = ".lst",
    numFREMThetas = length(grep("THETA",names(dfext)))-numNonFREMThetas,
    covSuffix     = "FREMCOV",
    parNames      = NULL,
    numParCov     = NULL,
    numSkipOm     = 0,
    dataFile,
    newDataFile   = paste("vpcData",runno,".csv",sep=""),
    availCov      = "all",
    idvar         = "ID",
    modDevDir     = NULL,
    quiet         = FALSE,
    cores         = 1,
    dfext         = NULL,
    baserunno     = NULL,
    baseModName   = NULL,
    baseModDevDir = modDevDir,
    ffemTabName   = "ffemtab",
    ffemModName   = NULL,
    omegaToData   = FALSE,
    ...) {
  
  ## Check input
  if(is.null(runno) & is.null(modName)) stop("Either runno or modName has to be specified")
  if(is.null(baserunno) & is.null(baseModName)) stop("Either baserunno or baseModName has to be specified")
  
  baseModNames <- getFileNames(runno=baserunno,modName=baseModName,modDevDir=baseModDevDir)
  basemodel    <- baseModNames$mod
  
  fremModNames <- getFileNames(runno=runno,modName=modName,modDevDir=modDevDir)
  extFile      <- fremModNames$ext
  
  # Only read it from file if it isn't passed via dfext
  if(is.null(dfext)) {
    dfExt <- getExt(extFile)
  } else {
    dfExt <- dfext
  }
  
  if(is.null(parNames)) stop("parNames should specify a vector of names for the parameters related to frem covariates.")
  if (is.null(numParCov)) {
    numParCov <- calcNumParCov(dfExt,numNonFREMThetas, numSkipOm)
  }
  
  if(numParCov != length(parNames)) stop("parNames should have the same length as numParCov")
  
  FFEMdata <- createFFEMdata2(runno          = runno,
                              numNonFREMThetas,
                              modName        = modName,
                              numFREMThetas  = numFREMThetas,
                              covSuffix      = covSuffix,
                              parNames       = parNames,
                              numParCov      = numParCov,
                              numSkipOm      = numSkipOm,
                              dataFile,
                              newDataFile    = newDataFile,
                              availCov       = availCov,
                              idvar          = idvar,
                              modDevDir      = modDevDir,
                              quiet          = quiet,
                              cores          = cores,
                              dfext          = dfext,
                              omegaToData    = omegaToData,
                              ...)
  
  ## Start processing the model
  
  ## Replace $PROBLEM
  tmp <- findrecord(basemodel,record="\\$PROBLEM",replace="$PROBLEM FFEM model",quiet=T)
  
  ## Replace $INPUT
  strInput <- findrecord(basemodel,record="\\$INPUT",quiet=T)
  strInput <- c(strInput, paste0("         ", paste(FFEMdata$indCovEff, collapse=" ")))
  tmp <- findrecord(tmp,record="\\$INPUT",replace=strInput,quiet=T)
  
  ## Replace $DATA
  strData <- findrecord(basemodel, record = "\\$DATA", quiet = TRUE)
  if (grepl("^(\\$DATA )(.*)(\\s+.+)$", strData[1]) == FALSE) { 
    strData[1] <- gsub("^(\\$DATA )(.*)$", paste0("\\1", newDataFile, "\\3"), strData[1])
  } else if(grepl("IGNORE",strData[1]) == TRUE)  { 
    strData[1] <- gsub("^(\\$DATA )(.*)(\\s+.+)$", paste0("\\1", newDataFile, "\\3"), strData[1])
  } else { 
    strData[1] <- gsub("^(\\$DATA )(.*)(\\s+.+)$", paste0("\\1", newDataFile, "\\2"), strData[1])
  }
  tmp <- findrecord(tmp, record = "\\$DATA", replace = strData, quiet = TRUE)
  ## Replace $OMEGA
  if (!omegaToData) {
    tmp <- findrecord(tmp,record="\\$OMEGA",replace=buildmatrix(FFEMdata$FullVars),quiet=TRUE)
  } else {
    n_ffem_par <- nrow(FFEMdata$Coefficients)
    omg_lines <- c()
    browser()
    # Write the skipped OMEGA lines
    if (numSkipOm > 0) {
      omg_lines<-c(omg_lines,buildmatrix(FFEMdata$FullVars[1:numSkipOm,1:numSkipOm]))
    }
    
    # Build the Cholesky block (Identitiy matrix)
    omg_lines <- c(omg_lines, paste0("$OMEGA BLOCK(", n_ffem_par, ") FIX"))
    for (i in 1:n_ffem_par) {
      row_vals <- rep("0.0", i)
      row_vals[i] <- "1.0"
      omg_lines <- c(omg_lines, paste(row_vals, collapse = " "))
    }
    
    tmp <- findrecord(tmp,record="\\$OMEGA",replace=omg_lines,quiet=TRUE)
  }
  
  ## Replace $THETA
  thvalues <- dfExt[dfExt$ITERATION==-1000000000,names(dfExt)[grepl("THETA.*",names(dfExt))]][1:numNonFREMThetas]
  tmp      <- findrecord(tmp,record="\\$THETA",replace=paste0("$THETA"," ",thvalues, " ; TH",1:numNonFREMThetas))
  
  ## Replace $SIGMA
  nosigma    <- length(dfExt[dfExt$ITERATION == -1000000000, names(dfExt)[grepl("SIGMA.*", names(dfExt))]])
  df_sig     <- as.numeric(dfExt[dfExt$ITERATION == -1000000000, names(dfExt)[grepl("SIGMA.*", names(dfExt))]])
  num_sig    <- -1 / 2 + sqrt(1 / 4 + 2 * nosigma) 
  sig_matrix <- as.numeric(df_sig)
  
  SIG                              <- matrix(0, nrow=num_sig, ncol=num_sig) 
  SIG[upper.tri(SIG,diag = TRUE)]  <- sig_matrix 
  tSIG                             <- t(SIG) 
  SIG[lower.tri(SIG,diag = FALSE)] <- tSIG[lower.tri(tSIG,diag = FALSE)] 
  SIGFULL                          <- SIG
  SIGFULL <- SIGFULL[-nrow(SIGFULL),-ncol(SIGFULL)]
  tmp <- findrecord(tmp,record="\\$SIGMA",replace=buildmatrix(as.matrix(SIGFULL),strName = "$SIGMA"),quiet=T)
  
  ## Replace ETA with Coefficients
  for (i in 1:nrow(FFEMdata$Coefficients)) {
    eta_idx <- i + numSkipOm
    if (!omegaToData) {
      tmp <- gsub(pattern = paste0("^(.*)(([^TH]|\\s*)\\bETA\\(",eta_idx,"\\))(.*)$"),
                  replace = paste0("\\1(ETA(",eta_idx,")+",FFEMdata$indCovEff[i],")\\4"),
                  x = tmp)
    } else {
      tmp <- gsub(pattern = paste0("^(.*)(([^TH]|\\s*)\\bETA\\(", eta_idx, "\\))(.*)$"),
                  replace = paste0("\\1(MYETA", eta_idx, " + ", FFEMdata$indCovEff[i], ")\\4"),
                  x = tmp)
    }
  }
  
  ## Inject Cholesky Code
  if (omegaToData) {
    n_ffem_par <- nrow(FFEMdata$Coefficients) 
    cholesky_lines <- generate_cholesky_lines(n_ffem_par, eta_offset = numSkipOm)
    
    pk_idx <- grep("\\$PK", tmp)
    if(length(pk_idx) > 0) {
      tmp <- c(tmp[1:pk_idx], cholesky_lines, tmp[(pk_idx+1):length(tmp)])
    } else { #If not PK, check for PRED
      pred_idx <- grep("\\$PRED", tmp)
      if(length(pred_idx) > 0) {
        tmp <- c(tmp[1:pred_idx], cholesky_lines, tmp[(pred_idx+1):length(tmp)])
      }
    }
  }
  
  ## Replace $EST
  tmp <- findrecord(tmp,record="\\$EST",replace="$ESTIMATION METHOD=1 INTER MAX=0")
  
  ## Change table file name
  tabString <- findrecord(tmp,record="\\$TAB")
  if(length(tabString)!=0) {
    tabString <- gsub(x=tabString,pattern = "FILE=.*",replace=paste0("FILE=",ffemTabName))
    tmp <- findrecord(tmp,record="\\$TAB",replace=tabString)
  }
  
  if(!is.null(ffemModName)) {
    writeLines(tmp,ffemModName)
  }
  
  if(quiet) {
    return(invisible(tmp))
  } else{
    return(tmp)
  }
}