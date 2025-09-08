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
  
  # --- 1. Initial Setup ---
  if (strUpdateType != "NoData") {
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
    
    if (is.null(strNewFREMData)) {
      if (!is.data.frame(strFREMData)) {
        # Original behavior: create a name based on the input file path
        strNewFREMData <- paste0(tools::file_path_sans_ext(strFREMData), "_new.", tools::file_ext(strFREMData))
      } else {
        # New behavior: stop if data is a data.frame but no output name is given
        stop("When 'strFREMData' is provided as a data frame, 'strNewFREMData' must be specified as a file name for the output dataset.", 
             call. = FALSE)
      }
    }
  } else {
    dfFREM         <- NULL
    dfFFEM         <- NULL
    strNewFREMData <- ""
  }
  
  if ((strUpdateType == "NoData" || strUpdateType == "NewInits") && !file.exists(paste0(tools::file_path_sans_ext(strFREMModel), ".ext"))) {
    stop(paste0("NoData and NewInits demands that ", paste0(tools::file_path_sans_ext(strFREMModel), ".ext"), " exists"))
  }
  
  if (strUpdateType != "NoData" && is.null(sortFREMDataset)) {
    stop("'sortFREMDataset' must be a character vector of column names when processing data.", call. = FALSE)
  }
  
  # --- 2. Initialize Model Parameters ---
  modelState <- initializeModelParameters(strFREMModel, numNonFREMThetas, numSkipOm, numParCov)
  numParCov  <- modelState$numParCov
  
  covList   <- list()
  addedList <- c()
  
  # --- 3. Update Data and Covariates (if applicable) ---
  if (strUpdateType != "NoData") {
    covnames <- getCovNames(modFile = strFREMModel)
    if (!is.null(filterString)) { dfFFEM <- dfFFEM %>% filter(!!rlang::parse_expr(filterString)) }
    dfFFEM <- addFREMcovariates(dfFFEM, modFile = strFREMModel)
    
    # 3a. Remove Covariates
    currentState <- list(
      dfFREM            = dfFREM, 
      covnames          = covnames, 
      theta             = modelState$theta, 
      omegaMatrix       = modelState$omegaMatrix, 
      thetaFix          = modelState$thetaFix, 
      numOmega          = modelState$numOmega, 
      numTheta          = modelState$numTheta, 
      numNonFREMThetas  = numNonFREMThetas, 
      numParCov         = numParCov, 
      numSkipOm         = numSkipOm, 
      iFremTypeIncrease = 100
    )
    updatedState           <- removeFremCovariates(currentState, cstrRemoveCov, quiet)
    dfFREM                 <- updatedState$dfFREM
    covnames               <- updatedState$covnames
    modelState$theta       <- updatedState$theta
    modelState$omegaMatrix <- updatedState$omegaMatrix
    modelState$thetaFix    <- updatedState$thetaFix
    modelState$numOmega    <- updatedState$numOmega
    modelState$numTheta    <- updatedState$numTheta
    
    # 3b. Prepare New Covariates
    iFremType    <- if(nrow(dfFREM) > 0) max(dfFREM$FREMTYPE) else 0
    prepResult   <- prepareNewCovariates(dfFFEM, cstrCatCovsToAdd, cstrContCovsToAdd, cstrCovsToAddOrder, covnames, iFremType, 100, strID, overrideExistingCheck, quiet)
    covList      <- prepResult$covList
    addedList    <- prepResult$addedList
    dfFFEM       <- prepResult$dfFFEM
    
    # 3c. Augment FREM Data
    dfFREM <- augmentFremData(dfFREM, dfFFEM, covList, addedList, covnames, cstrDV, strID, 100, cstrSetToZero, quiet)
  } else {
    covnames <- getCovNames(modFile = strFREMModel)
  }
  
  # --- 4. Finalize Data and Generate Model ---
  final_df <- finalizeFremData(dfFREM, sortFREMDataset, cstrKeepCols, bWriteData, strNewFREMData)
  
  final_model_lines <- generateFremModel(
    final_df            = final_df, 
    modelState          = modelState, 
    covList             = covList, 
    addedList           = addedList, 
    covnames            = covnames,
    strFREMModel        = strFREMModel, 
    strNewFREMData      = strNewFREMData, 
    bWriteMod           = bWriteMod, 
    bWriteFIX           = bWriteFIX,
    noBaseThetas        = numNonFREMThetas, 
    numSkipOm           = numSkipOm, 
    numParCov           = numParCov, 
    covEpsNum           = covEpsNum,
    basenames_th        = basenames_th, 
    basenames_om        = basenames_om, 
    dDefaultCovValue    = 1E-05, 
    strUpdateType       = strUpdateType
  )
  
  # --- 5. Return Final Objects ---
  return(invisible(list(data = final_df, model = final_model_lines)))
}