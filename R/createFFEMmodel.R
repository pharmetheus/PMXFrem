#'Create an FFEM version of a FREM model together with an appropriate data file.
#'
#'Create FFEM versions of a FREM model and data set to be used for generating
#'GOF plots and VPC analyses.
#'
#'To create GOF plots and VPC analysies for a FREM model it is necessary to
#'convert it to a FFEM model, with the appropriate covariate effects and omega
#'prim.
#'
#'There are two ways the FREM covariate effects can be included in the FFEM
#'model. The first is to amend the FFEM code with the full covariate model
#'expressions (these expressions can be obtained from the [createFFEMdata()]
#'function. This may be practically possible for one or a few covariates, but
#'will quickly run into NONMEM related issues regarding the number of allowed
#'constants, and similar. The second and recommended approach, is to precompute
#'the total impact of the covariates for each individual, and to add these
#'*individual covariate coefficients* to the original data set. They can then be
#'added as constants in the the FFEM code additatively to the corresponding ETA:
#'
#'Change the FFEM code from:
#'
#'`PAR = TVPAR * EXP(ETA)`
#'
#'to
#'
#'`PAR = TVPAR * EXP(ETA + PARCOV)`
#'
#'where PARCOV is the individual covariate coefficient. (The PARCOV term
#'
#'The addition of the individual covariate coefficients to the original data set
#'is done by´ the [createFFEMdata()] function. However, `createFFEMdata()` is
#'called by `createFFEMmodel()` so there is no need to do this as a separate
#'step.
#'
#'The are also other changes to the original model file to be suitable for
#'generating GOF diagnostics for the FREM model:
#' * Replace the data file name on $DATA with name of the file with the individual covariate coefficients.
#' * Append column names to $INPUT to match the new data file.
#' * Update the initial estimates for $THETA and $SIGMA with the corresponding estimates from the FREM run.
#' * Replace the initial estimates of the OMEGAs with the adjusted OMEGA ("Omega prim") from the FREM model.
#' * Add the individual covariate coefficients to the corresponding ETAs (see above).
#' * Replace the options on $EST with MAX=1 METHOD=1 INTER. If more that one $EST is present in the file, the second, third, etc are removed.
#' * Update any table file name with `ffemTabName`
#'
#'`createFFEMmodel()` implements these changes to the model automatically.
#'
#'The individual covariate coefficients will take missing covariates into
#'account while the omega prim will assume that all covariates are non-missing.
#'
#'The resulting model is not intended for estimation of the population
#'parameters, only for EBE estimation and generation of predictions in a
#'MAXEVAL=0 run. The the tabulated output is suitable for the generation of
#'regular GOF plots. The model file can easily be modified to perform
#'simulations for a VPC analysis.
#'
#'
#'@inheritParams getFileNames
#'@inheritParams createFFEMdata
#'@inheritParams calcFFEM
#'@param baserunno The run number of the base model.
#'@param baseModName The name of the base model. If NULL (default) no model file
#'  will be printed to file.
#'@param baseModDevDir The directory name in which the base model files can be
#'  found. Default the same as modDevDir.
#'@param ffemTabName The table file name to insert into the FFEM model. Default
#'  is 'ffemtab'.
#'@param ffemModName The name of the file to write the ffem model to, or NULL.
#'  NULL is the default
#'@param ... Arguments passed to `createFFEMdata`
#'
#'@return A character vector with the code for the FFEM model.
#'
#'@section Side effects: The FFEM model will be written to a file with the name
#'  `ffemModName` if it is non-`NULL`.
#'
#'@seealso [getFileName()] [createFFEMdata()] [calcFFEM()]]
#'@export
#'
#' @examples
#' \dontrun{
#' ffemMod <- createFFEMmodel(runno            = 31,
#'                            modDevDir        = modDevDir,
#'                            numNonFREMThetas = 7,
#'                            numSkipOm        = 2,
#'                            parNames         = c("CL","V","MAT"),
#'                            dataFile         = "DAT-2-MI-PMX-2-onlyTYPE2-new.csv",
#'                            newDataFile      = "testFileName.csv",
#'                            quiet            = FALSE,
#'                            baserunno        = 30)
#' }
createFFEMmodel <- function(
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
    ...) {

  ## Check input
  # if(is.null(newDataFile)) stop("newDataFile must be a character string.")
  if(is.null(runno) & is.null(modName)) stop("Either runno or modName has to be specified")
  if(is.null(baserunno) & is.null(baseModName)) stop("Either baserunno or baseModName has to be specified")

  baseModNames <- getFileNames(runno=baserunno,modName=baseModName,modDevDir=baseModDevDir,...)
  basemodel    <- baseModNames$mod

  fremModNames <- getFileNames(runno=runno,modName=modName,modDevDir=modDevDir,...)
  extFile      <- fremModNames$ext

  # Only read it from file if it isn't passed via dfext
  if(is.null(dfext)) {
    dfExt <- getExt(extFile)
  } else {
    dfExt <- dfext
  }

  ## Check the parNames argument
  if(is.null(parNames)) stop("parNames should specify a vector of names for the parameters related to frem covariates.")
  if (is.null(numParCov)) {
    numParCov <- calcNumParCov(dfExt,numNonFREMThetas, numSkipOm)
  }

  if(numParCov != length(parNames)) stop("parNames should have the same length as numParCov")

  FFEMdata <- createFFEMdata(runno         = runno,
                             numNonFREMThetas,
                             modName       = modName,
                             numFREMThetas = numFREMThetas,
                             covSuffix     = covSuffix,
                             parNames      = parNames,
                             numParCov     = numParCov,
                             numSkipOm     = numSkipOm,
                             dataFile,
                             newDataFile   = newDataFile,
                             availCov      = availCov,
                             idvar         = idvar,
                             modDevDir     = modDevDir,
                             quiet         = quiet,
                             cores         = cores,
                             dfext         = dfext,
                             ...)

  ## Start processing the model

  ## Replace $PROBLEM
  tmp <- findrecord(basemodel,record="\\$PROBLEM",replace="$PROBLEM FFEM model",quiet=T)

  ## Replace $INPUT
  strInput <- findrecord(basemodel,record="\\$INPUT",quiet=T)
  strInput <- c(strInput,paste0("         ",paste(FFEMdata$indCovEff,collapse=" ")))
  tmp      <- findrecord(tmp,record="\\$INPUT",replace=strInput,quiet=T)

  ## Replace $DATA
  strData <- findrecord(basemodel, record = "\\$DATA", quiet = T)

  if (grepl("^(\\$DATA )(.*)(\\s+.+)$", strData[1]) == FALSE) { # Only filename
    strData[1] <- gsub("^(\\$DATA )(.*)$", paste0("\\1", newDataFile, "\\3"), strData[1])
  } else {
    strData[1] <- gsub("^(\\$DATA )(.*)(\\s+.+)$", paste0("\\1", newDataFile, "\\3"), strData[1])
  }
  tmp <- findrecord(tmp, record = "\\$DATA", replace = strData, quiet = T)

  ## Replace $OMEGA
  tmp <- findrecord(tmp,record="\\$OMEGA",replace=buildmatrix(FFEMdata$FullVars),quiet=T)

  ## Replace $THETA
  thvalues <- dfExt[dfExt$ITERATION==-1000000000,names(dfExt)[grepl("THETA.*",names(dfExt))]][1:numNonFREMThetas]
  tmp      <- findrecord(tmp,record="\\$THETA",replace=paste0("$THETA"," ",thvalues, " ; TH",1:numNonFREMThetas))

  ## Replace $SIGMA
  nosigma    <- length(dfExt[dfExt$ITERATION == -1000000000, names(dfExt)[grepl("SIGMA.*", names(dfExt))]])
  df_sig     <- as.numeric(dfExt[dfExt$ITERATION == -1000000000, names(dfExt)[grepl("SIGMA.*", names(dfExt))]])
  num_sig    <- -1 / 2 + sqrt(1 / 4 + 2 * nosigma) # The col/row size of the full SIG matrix (including all blocks), remove last sigma
  sig_matrix <- as.numeric(df_sig)

  #Get the sig-matrix
  SIG                              <- matrix(0, nrow=num_sig, ncol=num_sig) #Define an empty matrix
  SIG[upper.tri(SIG,diag = TRUE)]  <- sig_matrix #Assign upper triangular + diag
  tSIG                             <- t(SIG) #Get a transposed matrix
  SIG[lower.tri(SIG,diag = FALSE)] <- tSIG[lower.tri(tSIG,diag = FALSE)] #Assign the lower triangular except diag
  SIGFULL                          <- SIG

  # remove last sigma (FREM sigma)
  SIGFULL <- SIGFULL[-nrow(SIGFULL),-ncol(SIGFULL)]

  # Replace $SIGMA
  tmp <- findrecord(tmp,record="\\$SIGMA",replace=buildmatrix(as.matrix(SIGFULL),strName = "$SIGMA"),quiet=T)

  ## Replace FREM eta with ETA+Coefficients
  for (i in 1:nrow(FFEMdata$Coefficients)) {
    tmp  <- gsub(pattern = paste0("^(.*)([^TH]ETA\\(",i+numSkipOm,"\\))(.*)$"),
                 replace = paste0("\\1(ETA(",i+numSkipOm,")+",FFEMdata$indCovEff[i],")\\3"),
                 x = tmp)
  }

  ## Replace $EST
  tmp <- findrecord(tmp,record="\\$EST",replace="$ESTIMATION METHOD=1 INTER MAX=0")

  ## Change table file name
  tabString <- findrecord(tmp,record="\\$TAB")

  ## If there is no $TAB in the base model file, skip to the next step
  if(length(tabString)!=0) {

    ## Change the table file name to ffemtab
    tabString <- gsub(x=tabString,pattern = "FILE=.*",replace=paste0("FILE=",ffemTabName))
    tmp <- findrecord(tmp,record="\\$TAB",replace=tabString)
  }

  ## Write the ffem model to disk unless ffemModName is NULL
  if(!is.null(ffemModName)) {
    writeLines(tmp,ffemModName)
  }

  if(quiet) {
    return(invisible(tmp))
  } else{
    return(tmp)
  }
}

