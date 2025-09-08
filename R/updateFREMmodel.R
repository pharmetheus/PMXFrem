#' Modify an exiting FREM model and data set
#'
#' Add or remove covariates and/or data to a FREM model and data set.
#'
#' @inheritParams createFREMData
#' @inheritParams calcFFEM
#' @param strFREMModel File name of the FREM-model to add/remove covariates
#'   to/from.
#' @param strFREMData Name of FREM-dataset (file or data.frame) to add/remove
#'   covariates to/from (not used with strUpdateType "NoData").
#' @param strFFEMData Name of FFEM-dataset (normal dataset, (file or
#'   data.frame)) that will be used to append the FREM-dataset, (not used with
#'   strUpdateType "NoData").
#' @param cstrContCovsToAdd A vector of continuous covariate names to add,
#'   default = NULL, (not used with strUpdateType "NoData").
#' @param cstrCatCovsToAdd A vector of categorical covariate names to add,
#'   default = NULL, (not used with strUpdateType "NoData").
#' @param cstrCatCovsToAdd A vector of categorical covariate names to add,
#'   default = NULL, (not used with strUpdateType "NoData").
#' @param cstrCovsToAddOrder A vector of the order of the covariate names to
#'   add, default = NULL (i.e. alphabetic order will be used), (not used with
#'   strUpdateType "NoData"). Note: if used, this should contain all covariates
#'   in cstrCatCovsToAdd as well as cstrContCovsToAdd.
#' @param strNewFREMData Name of the new dataset,
#'   default=paste0(strFREMData_without_extension,"new",".",extension), (not
#'   used with strUpdateType "NoData").
#' @param strUpdateType Update function to run: "DataAndModel" - Create new data
#'   and add/remove variables from the model (with updated inits). "NoData" - Do
#'   not create data or add variables to model, only update the frem model in
#'   terms of new inits
#' @param basenames_th A vector of strings with the names of the base variables
#'   (used for commenting thetas), should be the same length as number of
#'   nonFREMThetas in the model, if NULL, BASE1,BASE2,etc are used as names.
#' @param basenames_om A vector of strings with the names of the base variables
#'   (used for commenting omegas), should be the same length as number of
#'   numSkipOm+numParCov in the model, if NULL, BASE1,BASE2,etc are used as
#'   names.
#' @param bWriteData If FALSE; add new variables to the model file but do not
#'   write new datasets, has no effect when "NoData" is used.
#' @param bWriteFIX If TRUE; FIX is written to the theta parameter estimates
#'   code for the covariates that were fixed in the model file, if FALSE; all
#'   theta parameters are assumed to be estimated.
#' @param bWriteMod If TRUE; write the new model file to disk with _mod appended
#'   to the file name (before the suffix).
#' @param cstrRemoveCov A vector of strings for covariates that should be
#'   remove, note that FREMTYPEs for remaining covariates might/will change. The
#'   removal of covariates are done before any adding of data and/or new
#'   covariates. Note that if this functionality is used to remove the last
#'   existing category of a categorical covariate, this should be done by
#'   removing the orginal name of the covariate and not the specific categorical
#'   covariate, i.e. "SITEID" instead of "SITEID_1" to ensure consistent
#'   renumbering of FREMTYPEs
#' @param covEpsNum The number of the epsilons parameter to be used for the
#'   covariates.
#' @param overrideExistingCheck If TRUE, the existing check will be overriden
#'   and covariates will be added even though they are present in $DATA of the
#'   modefile
#' @param sortFREMDataset The columns to sort the new data set on.
#'
#' @return An invisible list with components data and model, containing the new
#'   data set (if any, else NULL) and updated model.
#'
#' @section Side effects:
#'
#'   Will write the new fremdata set (if bWriteData is TRUE and strUpdateType is
#'   not 'NoData') and updated model file (if bwriteMod is TRUE) to disc. The
#'   model file name will be 'stem'_new.mod).
#'
#' @export
#'
#' @examples
#' ## Remove SEX from the model and data set
#' updateFREMmodel(
#'   strFREMModel      = system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"),
#'   strFREMData       = system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem"),
#'   strFFEMData       = system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"),
#'   cstrRemoveCov     = c("SEX"),
#'   cstrCatCovsToAdd  = NULL,
#'   cstrContCovsToAdd = NULL,
#'   strID             = "ID",
#'   strNewFREMData    = "frem_dataset_noSEX.csv",
#'   numNonFREMThetas  = 7,
#'   numSkipOm         = 2,
#'   bWriteData        = TRUE,
#'   quiet             = FALSE,
#'   bWriteFIX         = TRUE,
#'   sortFREMDataset  = c("ID", "TIME", "FREMTYPE"),
#'   cstrKeepCols = c("ID", "TIME", "AMT", "EVID", "RATE", "DV", "FOOD", "FREMTYPE"))
#'
#' ## Only update inits
#' updateFREMmodel(
#'   strFREMModel      = system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"),
#'   basenames_th      = c("CL","V","MAT","D1","FRELFOOD","MATFOOD"),
#'   basenames_om      = c("RUV","D1","CL","V","MAT"),
#'   numNonFREMThetas  = 7,
#'   numSkipOm         = 2,
#'   bWriteData        = FALSE,
#'   bWriteMod         = FALSE,
#'   bWriteFIX         = TRUE,
#'   quiet             = FALSE,
#'   strUpdateType     = "NoData")
updateFREMmodel <- function(strFREMModel,
                            strFREMData,
                            strFFEMData,
                            cstrContCovsToAdd     = NULL,
                            cstrCatCovsToAdd      = NULL,
                            cstrCovsToAddOrder    = NULL,
                            strNewFREMData        = NULL,
                            filterString          = NULL,
                            strUpdateType         = "DataAndModel",
                            quiet                 = TRUE,
                            
                            
                            strID                 = "ID",
                            basenames_th          = NULL,
                            basenames_om          = NULL,
                            numNonFREMThetas,
                            numSkipOm             = 0,
                            numParCov             = NULL,
                            cstrKeepCols          = c("ID", "TIME", "AMT", "DV","II", "EVID", "SS", "RATE","FREMTYPE"),
                            cstrSetToZero         = c("AMT", "II", "SS", "EVID", "RATE"),
                            bWriteData            = TRUE,
                            bWriteFIX             = TRUE,
                            bWriteMod             = TRUE,
                            cstrDV                = "DV",
                            cstrRemoveCov         = NULL,
                            covEpsNum             = 2,
                            overrideExistingCheck = FALSE,
                            sortFREMDataset       = c(strID, "TIME", "FREMTYPE")) {
  
  
  iFremTypeIncrease <- 100 # The FREMTYPE INCREASE TO USE
  dDefaultCovValue <- 1E-05 # The covariance value to use as initial estimate for new covariates
  
  printq <- function(str, quiet) {
    if (!quiet) print(str)
  }
  
  if (strUpdateType != "NoData") {
    
    # <<< ROBUSTNESS FIX START >>>
    # Ensure internal data objects are standard data.frames to prevent
    # class-specific syntax errors (e.g., from data.table or tibble).
    if (is.data.frame(strFFEMData)) {
      dfFFEM <- as.data.frame(strFFEMData)
    } else if (file.exists(strFFEMData)) {
      dfFFEM <- as.data.frame(data.table::fread(strFFEMData, h = TRUE, data.table = FALSE, check.names = TRUE, showProgress = !quiet))
    } else {
      stop("Cannot find FFEM dataset: ", strFFEMData)
    }
    
    if (is.data.frame(strFREMData)) {
      dfFREM <- as.data.frame(strFREMData)
    } else if (file.exists(strFREMData)) {
      dfFREM <- as.data.frame(data.table::fread(strFREMData, h = TRUE, data.table = FALSE, check.names = TRUE, showProgress = !quiet))
    } else {
      stop("Cannot find FREM dataset: ", strFREMData)
    }
    
    if (is.null(strNewFREMData)) strNewFREMData <- paste0(tools::file_path_sans_ext(strFREMData), "_new.", tools::file_ext(strFREMData))
    if (!is.data.frame(strFREMData) && (is.null(strFREMData) || strFREMData == ""))
      stop("strFREMData must be set to dataset or data.frame")
    if (!is.data.frame(strFFEMData) && (is.null(strFFEMData) || strFFEMData == ""))
      stop("strFFEMData must be set to dataset or data.frame")
  }
  
  if (strUpdateType == "NoData" || strUpdateType == "NewInits") {
    if (!file.exists(paste0(tools::file_path_sans_ext(strFREMModel), ".ext")))
      stop(paste0("NoData and NewInits demands that ", paste0(tools::file_path_sans_ext(strFREMModel), ".ext"), " exists"))
  }
  
  # --- MINIMAL REFACTORING BLOCK START ---
  # The original ~60 lines of parameter extraction are replaced with this single call,
  # and the results are unpacked into the original variable names.
  
  modelState <- initializeModelParameters(strFREMModel, numNonFREMThetas, numSkipOm, numParCov)
  
  iNumTHETA  <- modelState$numTheta
  iNumOM     <- modelState$numOmega
  THETA      <- modelState$theta
  THETAFIX   <- modelState$thetaFix
  OM         <- modelState$omegaMatrix
  numParCov  <- modelState$numParCov # This might be updated by the function
  
  # --- MINIMAL REFACTORING BLOCK END ---
  
  printq(paste0(iNumTHETA, " THETAs found in modelfile"), quiet = quiet)
  printq(paste0(iNumOM, " OMEGAs/ETAs found in modelfile"), quiet = quiet)
  
  
  dfFFEM <- NULL
  dfFREM <- NULL
  
  covnames <- getCovNames(modFile = strFREMModel)
  
  addedList <- c()
  noBaseThetas <- numNonFREMThetas
  if (strUpdateType != "NoData") {
    if (is.data.frame(strFFEMData)) {
      dfFFEM <- as.data.frame(strFFEMData)
    } else if (file.exists(strFFEMData)) {
      dfFFEM <- as.data.frame(data.table::fread(strFFEMData, h = TRUE, data.table = FALSE, check.names = TRUE, showProgress = !quiet))
    } else {
      stop("Cannot find FFEM dataset: ", strFFEMData)
    }
    
    ## Check for filter string and apply if not NULL
    if (!is.null(filterString)) {
      dfFFEM <- dfFFEM %>% filter(!!rlang::parse_expr(filterString))
    }
    
    ## Add columns for binarised covariates that may not be present in the FFEM data set
    dfFFEM <- addFREMcovariates(dfFFEM,modFile = strFREMModel)
    
    
    if (is.data.frame(strFREMData)) {
      dfFREM <- strFREMData
    } else if (file.exists(strFREMData)) {
      dfFREM <- as.data.frame(data.table::fread(strFREMData, h = TRUE, data.table = FALSE, check.names = TRUE, showProgress = !quiet))
    } else {
      stop("Cannot find FREM dataset: ", strFREMData)
    }
    
    #### Removal of covariates
    # Bundle the current state into a list
    currentState <- list(
      dfFREM = dfFREM,
      covnames = covnames,
      theta = THETA,
      omegaMatrix = OM,
      thetaFix = THETAFIX,
      numOmega = iNumOM,
      numTheta = iNumTHETA,
      numNonFREMThetas = noBaseThetas,
      numParCov = numParCov,
      numSkipOm = numSkipOm,
      iFremTypeIncrease = iFremTypeIncrease
    )
    
    # Call the new function to handle removal logic
    updatedState <- removeFremCovariates(currentState, cstrRemoveCov, quiet)
    
    # Unpack the results back into local variables for the rest of the function to use
    dfFREM     <- updatedState$dfFREM
    covnames   <- updatedState$covnames
    THETA      <- updatedState$theta
    OM         <- updatedState$omegaMatrix
    THETAFIX   <- updatedState$thetaFix
    iNumOM     <- updatedState$numOmega
    iNumTHETA  <- updatedState$numTheta
    
    # Get the next fremtype to use. Handle case where dfFREM is empty after removals.
    iFremType <- if(nrow(dfFREM) > 0) max(dfFREM$FREMTYPE) else 0
    
    # Call the new function to prepare new covariates
    prepResult <- prepareNewCovariates(
      dfFFEM = dfFFEM,
      cstrCatCovsToAdd = cstrCatCovsToAdd,
      cstrContCovsToAdd = cstrContCovsToAdd,
      cstrCovsToAddOrder = cstrCovsToAddOrder,
      existingCovNames = covnames,
      lastFremType = iFremType,
      iFremTypeIncrease = iFremTypeIncrease,
      strID = strID,
      overrideExistingCheck = overrideExistingCheck,
      quiet = quiet
    )
    
    # Unpack the results back into local variables
    covList   <- prepResult$covList
    addedList <- c(addedList, prepResult$addedList) # Append to existing addedList
    dfFFEM    <- prepResult$dfFFEM
    iFremType <- prepResult$lastFremType
    
    dataToAdd <- dfFFEM[!(dfFFEM[[strID]] %in% dfFREM[[strID]]), ]
    printq(paste0("Found ", nrow(dataToAdd[!duplicated(dataToAdd[[strID]]), ]), " individuals that should be added to the FREM dataset"), quiet = quiet)
    iFremtypeDV <- unique(dfFREM[["FREMTYPE"]][dfFREM[["FREMTYPE"]] < iFremTypeIncrease])
    iNewFremtypeDV <- NULL
    if (is.null(cstrDV)) {
      warning("No DVs recognized; no DVs added for new individuals")
      iFremtypeDV <- NULL
    } else {
      if (length(iFremtypeDV) > length(cstrDV)) {
        warning(paste0("Found more DV Fremtypes than DV variables, only adding DVs: ", paste0(cstrDV, collapse = ", ")), " to new individuals")
        iFremTypeDV <- 0:(length(cstrDV) - 1)
      }
      if (length(iFremtypeDV) < length(cstrDV)) {
        printq(paste0("Found new DVs to add: ", paste0(cstrDV[(length(iFremtypeDV) + 1):length(cstrDV)], collapse = ", ")), quiet = quiet)
        iNewFremtypeDV <- length(iFremtypeDV):(length(cstrDV) - 1)
      }
    }
    dfAddList <- list()
    if (length(iFremtypeDV) > 0) {
      for (i in 1:length(iFremtypeDV)) {
        strDV <- cstrDV[i]
        dfDVData <- dataToAdd[dataToAdd[[strDV]] != -99, unique(c(names(dataToAdd)[names(dataToAdd) %in% names(dfFREM)], strDV)), ]
        if (nrow(dfDVData) == 0) {
          printq(paste0("No observations for ", strDV, " (fremtype=", iFremtypeDV[i], "); not adding any observations!"), quiet = quiet)
        } else {
          dfDVData$DV <- dfDVData[[strDV]]
          if (!(strDV %in% names(dfFREM))) dfDVData[[strDV]] <- NULL
          dfDVData$FREMTYPE <- iFremtypeDV[i]
          if (length(names(dfDVData)) == length(names(dfFREM)) && all(sort(names(dfDVData)) == sort(names(dfFREM)))) {
            dfDVData       <- dfDVData[, names(dfFREM)]
            dfAddList[[i]] <- dfDVData
            printq(paste0("Adding ", nrow(dfDVData), " observations (", strDV, ") from ", nrow(dfDVData[!duplicated(dfDVData[[strID]]), ]), " individuals as fremtype ", iFremtypeDV[i]), quiet = quiet)
          } else {
            strMissnames <- names(dfFREM)[!(names(dfFREM) %in% names(dfDVData))]
            stop("Variables not found in FFEM dataset: ", paste0(strMissnames, collapse = ", "), "; please add column(s) and try again")
          }
        }
      }
      dfFREM <- rbind(dfFREM, as.data.frame(data.table::rbindlist(dfAddList)))
    }
    dfAddList <- list()
    if (length(iNewFremtypeDV) > 0) {
      for (i in 1:length(iNewFremtypeDV)) {
        strDV <- cstrDV[length(iFremtypeDV) + i]
        dfDVData <- dfFFEM[dfFFEM[[strDV]] != -99, unique(c(names(dfFFEM)[names(dfFFEM) %in% names(dfFREM)], strDV)), ]
        if (nrow(dfDVData) == 0) {
          printq(paste0("No observations for ", strDV, " (fremtype=", iNewFremtypeDV[i], "); not adding any observations!"), quiet = quiet)
          warning(paste0("Note that it might be inconsistencies in DV fremtypes since fremtype ", iNewFremtypeDV[i], " is not present!"))
        } else {
          dfDVData$DV <- dfDVData[[strDV]]
          if (!(strDV %in% names(dfFREM))) dfDVData[[strDV]] <- NULL
          dfDVData$FREMTYPE <- iNewFremtypeDV[i]
          if (length(names(dfDVData)) == length(names(dfFREM)) && all(sort(names(dfDVData)) == sort(names(dfFREM)))) {
            dfDVData <- dfDVData[, names(dfFREM)]
            dfAddList[[i]] <- dfDVData
            printq(paste0("Adding ", nrow(dfDVData), " observations (", strDV, ") from ", nrow(dfDVData[!duplicated(dfDVData[[strID]]), ]), " individuals as fremtype ", iNewFremtypeDV[i]), quiet = quiet)
          } else {
            strMissnames <- names(dfFREM)[!(names(dfFREM) %in% names(dfDVData))]
            stop("Variables not found in FFEM dataset: ", paste0(strMissnames, collapse = ", "), "; please add column(s) and try again")
          }
        }
      }
      dfFREM <- rbind(dfFREM, as.data.frame(data.table::rbindlist(dfAddList)))
    }
    dfAddList <- list()
    if (nrow(dataToAdd) > 0) {
      for (i in 1:length(covnames$covNames)) {
        strCov <- covnames$covNames[i]
        iFremtype <- iFremTypeIncrease * i
        strCovClean <- stringr::str_replace(strCov, "_.*", "")
        dfData <- dataToAdd[dataToAdd[[strCovClean]] != -99, unique(c(names(dataToAdd)[names(dataToAdd) %in% names(dfFREM)], strCovClean)), ]
        if (nrow(dfData) == 0) {
          printq(paste0("No observed covariate values for ", strCov, " (fremtype=", iFremtype, "); not adding any covariate values!"), quiet = quiet)
        } else {
          dfData <- dfData[!duplicated(dfData[[strID]]), ]
          dfData$FREMTYPE <- iFremtype
          if (strCov == strCovClean) {
            dfData$DV      <- dfData[[strCovClean]]
            dfData         <- dfData[, names(dfFREM)]
            dfAddList[[i]] <- dfData
            printq(paste0("Adding ", nrow(dfData), " continuous covariate values (", strCovClean, ") from ", nrow(dfData[!duplicated(dfData[[strID]]), ]), " individuals as fremtype ", iFremtype), quiet = quiet)
          } else {
            iCategory      <- gsub(".+_([0-9]+)", "\\1", strCov)
            dfData$DV      <- ifelse(dfData[[strCovClean]] == iCategory, 1, 0)
            dfData         <- dfData[, names(dfFREM)]
            dfAddList[[i]] <- dfData
            printq(paste0("Adding ", nrow(dfData), " categorical covariate values (", strCov, ") from ", nrow(dfData[!duplicated(dfData[[strID]]), ]), " individuals as fremtype ", iFremtype), quiet = quiet)
          }
        }
      }
      dfFREM <- rbind(dfFREM, as.data.frame(data.table::rbindlist(dfAddList)))
    }
    dfAddList <- list()
    dfFREMOne <- dfFREM[!duplicated(dfFREM[[strID]]), ]
    if (!is.null(addedList)) {
      for (i in 1:length(addedList)) {
        strcov               <- addedList[i]
        l                    <- covList[[strcov]]
        dftmp                <- dfFREMOne
        dfnew                <- l[["Data"]]
        dfnew$NEWVARIABLE    <- dfnew[[l[["Name"]]]]
        dfnew[[l[["Name"]]]] <- NULL
        dftmp                <- merge(dftmp, dfnew, by = strID, all.x = FALSE, all.y = FALSE)
        dftmp$DV             <- dftmp$NEWVARIABLE
        dftmp$FREMTYPE       <- l[["Fremtype"]]
        dftmp$NEWVARIABLE    <- NULL
        
        # --- THIS IS THE CORRECTED LINE - REVERTED TO ORIGINAL ---
        dftmp                <- dftmp %>% mutate_at(which(names(dftmp) %in% cstrSetToZero),function(x) return(0))
        
        dfAddList[[i]]       <- dftmp
        printq(paste0("Adding ", nrow(dftmp), " covariate values (", l[["Name"]], ") from ", nrow(dftmp[!duplicated(dftmp[[strID]]), ]), " individuals as fremtype ", l[["Fremtype"]]), quiet = quiet)
      }
    }
    dfFREM <- rbind(dfFREM, as.data.frame(data.table::rbindlist(dfAddList)))
    if (!is.null(sortFREMDataset)) {
      if (is.null(cstrKeepCols)) {
        dfFREM %>% dplyr::arrange(!!!rlang::syms(sortFREMDataset))
      } else {
        dfFREM <- dfFREM %>% dplyr::arrange(!!!rlang::syms(sortFREMDataset)) %>% dplyr::select(dplyr::one_of(cstrKeepCols))
      }
    } else {
      if (is.null(cstrKeepCols)) {
        dfFREM <- dfFREM %>% dplyr::arrange(ID, FREMTYPE)
      } else {
        dfFREM <- dfFREM %>% dplyr::arrange(ID, FREMTYPE) %>% dplyr::select(dplyr::one_of(cstrKeepCols))
      }
    }
    if (bWriteData) {
      write.csv(dfFREM, file = strNewFREMData, row.names = FALSE, quote = FALSE)
      if (!("FREMTYPE" %in% names(dfFREM))) warning("No FREMTYPE available in dataset, add in cstrKeepCols and rerun updateFREM")
    }
  }
  strNewCovNames <- c(covnames$covNames, addedList)
  con <- file(strFREMModel, open = "r")
  line <- readLines(con)
  close(con)
  strinput <- ""
  for (i in (noBaseThetas + 1):(length(strNewCovNames) + noBaseThetas)) {
    mu_count <- i - noBaseThetas + numSkipOm + numParCov
    strinput <- c(strinput, paste0("      MU_", mu_count, " = ", "THETA(", i, ")"))
    strinput <- c(strinput, paste0("      COV", mu_count, " = MU_", mu_count, " + ETA(", mu_count, ")"))
  }
  tmpl <- grep(pattern = "COV\\d+ = MU\\_\\d+", x = line)
  if (is.null(tmpl)) warning("can't replace MU and COV lines")
  if (!is.null(tmpl)) {
    line <- c(line[1:(min(tmpl - 2))], strinput, line[(max(tmpl) + 1):length(line)])
  }
  fremTypes <- seq(from = iFremTypeIncrease, by = iFremTypeIncrease, length.out = length(strNewCovNames))
  strinput <- ""
  strinput <- c(strinput, paste0(";;;FREM CODE BEGIN COMPACT"))
  for (i in 1:length(strNewCovNames)) {
    strinput <- c(strinput, paste0("      IF(FREMTYPE.EQ.", fremTypes[i], ") THEN"))
    strinput <- c(strinput, paste0(";       ", strNewCovNames[i]))
    strinput <- c(strinput, paste0("        Y = COV", i + numSkipOm + numParCov, " + EPS(", covEpsNum, ")"))
    strinput <- c(strinput, paste0("        IPRED = COV", i + numSkipOm + numParCov))
    strinput <- c(strinput, paste0("      ENDIF"))
  }
  strinput <- c(strinput, paste0(";;;FREM CODE END COMPACT"))
  line <- findrecord(line, record = ";;;FREM CODE BEGIN COMPACT", replace = strinput, quiet = TRUE)
  if (is.null(basenames_th)) basenames_th <- paste0("BASE", 1:numNonFREMThetas)
  if (is.null(basenames_om)) basenames_om <- paste0("BASE", 1:(numSkipOm + numParCov))
  if (!is.null(addedList) & length(addedList) > 0) {
    if (exists("OM") == FALSE || is.null(OM)) stop("OM missing, make sure ext file is provided when adding covariates")
    OMNEW <- matrix(dDefaultCovValue, ncol(OM) + length(addedList), nrow(OM) + length(addedList))
    OMNEW[1:ncol(OM), 1:nrow(OM)] <- OM
    OM <- OMNEW
  }
  theta_comment <- paste0(" ; ", 1:iNumTHETA, " TV_", c(basenames_th, covnames$covNames))
  om_comment <- paste0(" ; ", 1:iNumOM, " BSV_", c(basenames_om, covnames$covNames))
  if (!is.null(addedList)) {
    for (i in 1:length(addedList)) {
      strcov                     <- addedList[i]
      l                          <- covList[[strcov]]
      THETA                      <- c(THETA, l[["Mean"]])
      THETAFIX                   <- c(THETAFIX, 0)
      theta_comment              <- c(theta_comment, paste0(" ; ", iNumTHETA + 1, " TV_", l[["Name"]]))
      iNumTHETA                  <- iNumTHETA + 1
      OM[iNumOM + 1, iNumOM + 1] <- l[["Var"]]
      om_comment                 <- c(om_comment, paste0(" ; ", iNumOM + 1, " BSV_", l[["Name"]]))
      iNumOM                     <- iNumOM + 1
    }
  }
  strinput <- ""
  for (i in 1:iNumTHETA) {
    strFIX <- ""
    if (THETAFIX[i] == 1 && bWriteFIX) strFIX <- " FIX"
    strinput <- c(strinput, paste0("$THETA ", THETA[i], strFIX, " ", theta_comment[i]))
  }
  line <- findrecord(line, record = "\\$THETA", replace = strinput, quiet = TRUE)
  newommatrix <- buildmatrix(as.matrix(OM))
  j <- length(newommatrix)
  for (i in length(om_comment):(1 + numSkipOm)) {
    newommatrix[j] <- paste0(newommatrix[j], " ", om_comment[i])
    j <- j - 1
  }
  line <- findrecord(line, record = "\\$OMEGA", replace = newommatrix, quiet = TRUE)
  if(strUpdateType != "NoData") {
    line <- findrecord(line, record = "\\$DATA", replace = paste0("$DATA ", strNewFREMData, " IGNORE=@"), quiet = TRUE)
    if (!is.null(dfFREM)) {
      line <- findrecord(line, record = "\\$INPUT", replace = paste0("$INPUT ", paste0(names(dfFREM), collapse = " ")), quiet = TRUE)
    }
  }
  if (bWriteMod) {
    strNewModelFileName <- paste0(tools::file_path_sans_ext(strFREMModel), "_new.mod")
    con <- file(strNewModelFileName, open = "w")
    writeLines(line, con)
    close(con)
  }
  
  retList <- list(data = dfFREM, model = line)
  return(invisible(retList))
}