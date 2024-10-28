#' Create a new FFEM data set with the FREM covariat effect appended as columns
#'
#'
#' Add combined FFEM coefficients to the data set for the specified covariates.
#'
#' This function computes a combined term of all (FREM) covariate effects for
#' each parameter in the FFEM model (individual covariate coefficients) and
#' creates a new data file with these combined effects appended. The individual
#' covariate coefficients are added to the FFEM data set with names that appends
#' COV to the parameter name, e.g. the column with the combined covariate
#' effects for parameter PAR will be called PARCOV. All rows for an individual
#' in the data set will have the same value for PARCOV.
#'
#' The function  [calcFFEM()] is used to calculate the individual covariate
#' coefficients. It is called once per individual in the data set and takes any
#' missing covariates into account. This means that two subjects with identical
#' covariate values, except that one subject has a missing value for one of the
#' covariate, will have different covariate coefficients for the non-missing
#' covariates.
#'
#' If the original NONMEM model file use IGNORE statements to subset the data
#' file, it is necessary to provide a filter expression as a character string to
#' obtain an FFEM data set that only include the rows used in the original
#' model. This is done with the `filterString` argument, e.g. `STUDYID==7` to
#' only include STUDYID 7 (this is the same as saying `IGNORE.NE.7` in the
#' NM-TRAN model file.) If IGNORE statements are used in original model file and
#' `filterString` is not specified, then the resulting FFEM data set will have
#' the same number of rows as the the original data set.
#'
#' @inheritParams calcFFEM
#' @inheritParams getFileNames
#' @param dataFile The name of the data file used in the base model, i.e. the
#'   original data file, or a data.frame with the same data.
#' @param newDataFile The name of a new data file with the FFEM columns added.
#'   Default is vpcData{runno}.csv. If NULL, will return a data frame with the
#'   data instead of writing it to disk.
#' @param idvar The name of the ID column,
#' @param cores How many cores to use in the calculations of the FFEM
#'   expressions.
#' @param covSuffix Use to create the column name fo rthe covariate effects
#'   columns. The column names will be the parameter name followed by this
#'   string.
#' @param filterString A character string with a filter expression to subset the
#'   amended FFEM data set so that the number of rows matches the original data
#'   file. Useful if IGNORE statements were used in the original model file.
#'
#' @seealso [createFFEMmodel()]
#' @return A list with objects:
#'
#' * Omega: The omega prim matrix to be used in the model file. The upper triangle is set to NAs.
#' * Coefficients: An nPar x nCov matrix with covariate coefficients used to compute the individual covariate effects.
#' * indCovEff: A character vector with the labels for the individual covariate effects. Used for the new columns in the new data set.
#' * newData: A data.frame with the new data set, i.e. the same as dataFile with individual covariate effects appended.
#'
#' @section Side effetcs:
#'
#'   If `newDataFile` is not NULL, `newData` will be printed as a csv to disk
#'   with the name `newDataFile`.
#'
#'   If quiet is FALSE, the FFEM expressions and new OMEGA matrix is printed in
#'   the console.
#'
#' @export
#'
#' @examples
#'
#' data <- read_csv(system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"), show_col_types = FALSE) %>%
#'   filter(BLQ != 1)
#'
#' ## Check with specified parameter names
#' ffemData <- createFFEMdata(
#'   modName          = "run31",
#'   modDevDir        = system.file("extdata/SimNeb/", package = "PMXFrem"),
#'   parNames         = c("CL", "V", "MAT"),
#'   numNonFREMThetas = 7,
#'   numSkipOm        = 2,
#'   dataFile         = data,
#'   newDataFile      = NULL,
#'   quiet            = TRUE)
#'
createFFEMdata <- function(runno = NULL,
                           numNonFREMThetas,
                           modName       = NULL,
                           numFREMThetas = length(grep("THETA", names(dfext))) - numNonFREMThetas,
                           covSuffix     = "FREMCOV",
                           parNames      = paste("Par", 1:numParCov, sep = ""),
                           numParCov     = NULL,
                           numSkipOm     = 0,
                           dataFile,
                           newDataFile   = paste("vpcData", runno, ".csv", sep = ""),
                           filterString  = NULL,
                           availCov      = "all",
                           idvar         = "ID",
                           modDevDir     = NULL,
                           quiet         = FALSE,
                           cores         = 1,
                           dfext         = NULL,
                           ...) {


  retList <- list()
  retList$newDataFileName <- newDataFile

  fileNames <- getFileNames(runno = runno, modName = modName, modDevDir = modDevDir, ...)
  modFile   <- fileNames$mod
  extFile   <- fileNames$ext
  phiFile   <- fileNames$phi

  covNames <- getCovNames(modFile)$covNames
  fremCovs <- getCovNames(modFile)$polyCatCovs
  orgCovs  <- getCovNames(modFile)$orgCovNames

  # It is much faster to send in extdf than to create it fo reach ID.
  # Only read it from file if it isn't passed via dfext
  if (is.null(dfext)) {
    dfext <- getExt(extFile)
  } else {
    dfext <- dfext
  }

  # dfPhi   <- getPhi(phiFile)

  if (is.null(numParCov)) {
    numParCov <- calcNumParCov(dfext, numNonFREMThetas, numSkipOm)
  }


  ## Run this to get the omega matrix to use in the vpc
  if (length(availCov) == 1 && availCov == "all") availCov    <- covNames

  tmp <- calcFFEM(dfext = dfext, numNonFREMThetas, covNames = covNames, parNames = parNames, availCov = availCov, quiet = quiet, numSkipOm = numSkipOm, ...)

  ## Create the omega matrix information to put in the return value. This function
  ## takes the full omega, removes any skipped omega and sets the upper triangle to NA.
  makeMat <- function(myMat, skip = numSkipOm) {

    myMat[upper.tri(myMat)] <- NA
    if (skip != 0) {
      retVal <- myMat[-(1:skip), -(1:skip)]
    } else {
      retVal <- myMat
    }

    return(retVal)
  }


  ## Collect Coefficients and omegaprim to return object
  retList$Omega        <- makeMat(tmp$FullVars)
  retList$Coefficients <- tmp$Coefficients
  retList$FullVars     <- tmp$FullVars
  retList$Expr         <- tmp$Expr

  ## Create a data set with all the original covariates + the frem-specific ones
  # Read the FFEM data set and rename the id column to ID (to simplify the coding below. The id column will get its original name in the new data file.)
  if (!is.data.frame(dataFile)) {
    data <- fread(dataFile, h = T, data.table = FALSE, showProgress = FALSE) %>%
      rename("ID" = idvar)
  } else { ## The dataFile was supplied as a data frame and not a name
    data <- dataFile %>%
      rename("ID" = idvar)
  }
  ## Add the FREM covariates to the data file
  data <- addFREMcovariates(dfFFEM = data, modFile)

  dataI <- data %>% distinct(ID, .keep_all = TRUE)
  dataI <- dataI[, c("ID", orgCovs, covNames)]

  ## Go through the individuals to make sure that missing values for polycats are coded properly

  ## Register to allow for parallel computing
  if (cores > 1) registerDoParallel(cores = cores)

  mapFun <- function(data, cov, orgCovs) {
    for (cov in orgCovs) {
      if (data[1, cov] == -99 & length(grepl(cov, names(data))) > 1) {
        data[1, grepl(cov, names(data))] <- -99
      }
    }
    return(data)
  }
  #
  #   dataI <- foreach(k = 1:nrow(dataI)) %dopar% {
  #     mapFun(data=dataI[k,],cov=cov,orgCovs=orgCovs)
  #   }
  #
  #
  if (cores > 1) {
    dataI <- foreach(k = 1:nrow(dataI)) %dopar% {
      mapFun(data = dataI[k, ], cov = cov, orgCovs = orgCovs)
    }
    dataI <- data.frame(rbindlist(dataI))
  } else {
    dataI2 <- data.frame()
    for (k in 1:nrow(dataI)) dataI2 <- rbind(dataI2, mapFun(data = dataI[k, ], cov = cov, orgCovs = orgCovs))
    dataI <- dataI2
  }


  dataI <- dataI[, c("ID", covNames)]
  dataMap <- dataI[]
  dataMap[, covNames] <- TRUE

  for (c in covNames) {
    dataMap[, c] <- ifelse(dataI[, c] == -99, FALSE, TRUE)
  }

  ## Loop over each individual to compute their covariate contributions ##
  myFun <- function(data, parNames, dataMap = dataMap, availCov = NULL, covSuffix) {
    ID <- data$ID
    if (is.null(availCov)) {
      availCov  <- covNames[as.logical(dataMap[dataMap$ID == ID, -1])]
    } else {
      myCov    <- covNames[as.logical(dataMap[dataMap$ID == ID, -1])]
      availCov <- myCov[myCov %in% availCov]
    }
    ffemObj <- calcFFEM(dfext = dfext, numNonFREMThetas, covNames = covNames, availCov = availCov, quiet = TRUE, parNames = parNames, numSkipOm = numSkipOm, ...)

    retDf <- data.frame(ID = ID)
    for (i in 1:length(parNames)) {
      colName <- paste0(parNames[i], covSuffix)
      retDf[[colName]] <- as.numeric(eval(parse(text = ffemObj$Expr[i])))
      # retDf[[parNames[i]]] <- as.numeric(eval(parse(text=ffemObj$Expr[i])))
    }

    return(retDf)
  }

  dataOne <- data %>% distinct(ID, .keep_all = TRUE)



  if (cores > 1) {
    covEff <- foreach(k = 1:nrow(dataOne)) %dopar% {
      myFun(data = dataOne[k, ], parNames, dataMap = dataMap, availCov = availCov, covSuffix)
    }
    covEff <- data.frame(rbindlist(covEff))
  } else {
    covEff2 <- data.frame()
    for (k in 1:nrow(dataOne)) covEff2 <- bind_rows(covEff2, myFun(data = dataOne[k, ], parNames, dataMap = dataMap, availCov = availCov, covSuffix))
    covEff <- covEff2
  }


  ## Add the individual covariateCoefficients to the return object
  retList$indCovEff <- names(covEff)[-1]

  data2 <- left_join(data, covEff, by = "ID")

  # Remove the frem covariates
  data3 <- data2[, !(names(data2) %in% fremCovs)]

  ## Give the ID column back its original name
  data3 <- data3 %>% rename(!!idvar := "ID")

  ## Add the modified data set to the return object
  if (!is.null(filterString)) {
    retList$newData <- data3 %>% filter(!!rlang::parse_expr(filterString))
  } else {
    retList$newData <- data3
  }

  # Write the data to file
  if (!is.null(newDataFile)) {
    write.csv(retList$newData, file = newDataFile, quote = FALSE, row.names = FALSE)
  }

  if (cores > 1) stopImplicitCluster()
  invisible(retList)
}
