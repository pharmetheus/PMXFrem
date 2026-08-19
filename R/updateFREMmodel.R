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
#' @param missVal Numeric. The value representing missing data in the covariates. Defaults to `-99`.
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
#' @param bRecodeDichotomous Logical. Should dichotomous covariates be automatically 
#'   recoded to 0/1? Defaults to FALSE. If FALSE, inputs must be strictly 0/1.
#' @param allowNon01 Logical. If TRUE, allows non-0/1 dichotomous covariates (like 1/2) 
#'   to pass through untouched for PsN compatibility. Defaults to FALSE.
#' @param keepDoseOnlySubjects Logical. If \code{FALSE} (default), subjects without any valid PK observations (e.g., 
#' only dosing records) are completely excluded from the generated dataset. If \code{TRUE}, 
#' these subjects are retained and their covariates are included as observations.
#' @param roundMeanTo Numeric. The number of decimal places to round the calculated baseline covariate means. Defaults to 2.
#' @param fixTheta Logical. Should initial THETA estimates for covariates without missing values be fixed? Defaults to TRUE.
#' @param sortFREMDataset Deprecated.
#' @return An invisible list with components data and model, containing the new
#'   data set (if any, else NULL) and updated model.
#'
#' @section Side effects:
#'
#'   Will write the new fremdata set (if bWriteData is TRUE and strUpdateType is
#'   not 'NoData') and updated model file (if bwriteMod is TRUE) to disc. The
#'   model file name will be 'stem'_new.mod).
#'
#'@examples
#' 
#' ## Example 1: Remove Covariates from a Model
#' # This example removes "SEX" from the model and data set.
#' td <- tempdir() # Create a temporary directory for output files
#' 
#' updateFREMmodel(
#'   strFREMModel      = system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"),
#'   strFREMData       = system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem"),
#'   strFFEMData       = system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem"),
#'   cstrRemoveCov     = c("SEX"),
#'   strNewFREMData    = file.path(td, "frem_dataset_noSEX.csv"),
#'   numNonFREMThetas  = 7,
#'   numSkipOm         = 2,
#'   bWriteData        = TRUE,
#'   bWriteMod         = TRUE,
#'   quiet             = FALSE,
#'   bWriteFIX         = TRUE,
#'   cstrKeepCols      = c("ID", "TIME", "AMT", "EVID", "RATE", "DV", "FOOD", "FREMTYPE")
#' )
#'
#' ## Example 2: Add Covariates to a Model
#' # This example adds a new categorical covariate, "SITE", to the model.
#' td <- tempdir()
#' 
#' # First, create a temporary FFEM dataset that includes the new covariate
#' ffem_df <- as.data.frame(data.table::fread(
#'   system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")
#' ))
#' ffem_df$SITE <- rep(c(101, 102, 103), length.out = nrow(ffem_df)) # Add 3-level SITE covariate
#' 
#' # Define paths to original model and FREM data
#' model_path <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
#' frem_data_path <- system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem")
#' 
#' # Copy the .ext file to the temp directory so the function can find it
#' file.copy(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"), td)
#' file.copy(model_path, td)
#' model_in_td <- file.path(td, "run31.mod")
#' 
#' updateFREMmodel(
#'   strFREMModel      = model_in_td,
#'   strFREMData       = frem_data_path,
#'   strFFEMData       = ffem_df, # Use the modified data frame with SITE
#'   strNewFREMData    = file.path(td, "frem_data_with_SITE.csv"),
#'   cstrCatCovsToAdd  = "SITE",  # Add the new SITE covariate
#'   numNonFREMThetas  = 7,
#'   numSkipOm         = 2,
#'   bWriteData        = TRUE,
#'   bWriteMod         = TRUE,
#'   quiet             = FALSE
#' )
#' 
#' ## Example 3: Add New Individuals to an Existing FREM Dataset
#' td <- tempdir()
#' 
#' # Load the original data as data frames
#' frem_df <- as.data.frame(data.table::fread(
#'   system.file("extdata/SimNeb/frem_dataset.dta", package = "PMXFrem")
#' )) %>% rename(LNDV=ODV)
#' ffem_df <- as.data.frame(data.table::fread(
#'   system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv", package = "PMXFrem")
#' )) 
#' 
#' # Simulate new individuals by taking 50 rows and giving them new IDs
#' ffem_new_ids <- ffem_df[1:50, ]
#' ffem_new_ids$ID <- ffem_new_ids$ID + max(frem_df$ID)
#' 
#' # Use the original model
#' model_path <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
#' 
#' result <- updateFREMmodel(
#'   strFREMModel      = model_path,
#'   strFREMData       = frem_df,      # Pass original data as a data frame
#'   strFFEMData       = ffem_new_ids, # Pass the new individuals' data
#'   strNewFREMData    = file.path(td, "frem_data_with_new_ids.csv"),
#'   numNonFREMThetas  = 7,
#'   numSkipOm         = 2,
#'   bWriteData        = TRUE,
#'   bWriteMod         = FALSE, # Model structure doesn't change
#'   quiet             = FALSE
#' )
#' 
#' # The returned data frame now contains the new IDs
#' print(tail(result$data))
#'
#' ## Example 4: Only Update Initial Estimates from an .ext file
#' td <- tempdir()
#' model_path <- system.file("extdata/SimNeb/run31.mod", package = "PMXFrem")
#' 
#' # Copy model and ext file to a temporary directory
#' file.copy(model_path, td)
#' file.copy(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"), td)
#' model_in_td <- file.path(td, "run31.mod")
#' 
#' updateFREMmodel(
#'   strFREMModel       = model_in_td,
#'   strUpdateType      = "NoData",
#'   basenames_th       = c("CL","V","MAT","D1","FRELFOOD","MATFOOD"),
#'   basenames_om       = c("RUV","D1","CL","V","MAT"),
#'   numNonFREMThetas   = 7,
#'   numSkipOm          = 2,
#'   bWriteData         = FALSE,
#'   bWriteMod          = TRUE,
#'   bWriteFIX          = TRUE,
#'   bRecodeDichotomous = TRUE,
#'   quiet              = FALSE
#' )
#' 
#' @export
#' @family FREM model management
#' @concept frem_model_management
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
                            missVal               = -99,
                            overrideExistingCheck = FALSE,
                            bRecodeDichotomous    = FALSE,
                            allowNon01            = FALSE,
                            keepDoseOnlySubjects  = FALSE,
                            roundMeanTo           = 2,
                            fixTheta              = TRUE,
                            sortFREMDataset       = NULL) {
  
  if (!is.null(sortFREMDataset)) {
    warning(
      "The `sortFREMDataset` argument is deprecated. ",
      "PMXFrem v2 and above now automatically enforces stable, intra-subject dataset sorting. ",
      "This argument is safely ignored.", 
      call. = FALSE
    )
  }
  
  # --- 1. Initial Setup ---
  if (strUpdateType != "NoData") {
    
    # 1A. Load and filter FFEM Data
    if (is.data.frame(strFFEMData)) { 
      dfFFEM <- as.data.frame(strFFEMData) 
    } else if (file.exists(strFFEMData)) { 
      dfFFEM <- as.data.frame(data.table::fread(strFFEMData, h = TRUE, data.table = FALSE, check.names = TRUE, showProgress = !quiet)) 
    } else { 
      stop("Cannot find FFEM dataset: ", strFFEMData) 
    }
    
    # Apply IGNORE filters so dfFFEM perfectly matches the base data cohort rules
    if (exists("filterDataFromModel")) {
      dfFFEM <- filterDataFromModel(baseModelFile = strFREMModel, wideData = dfFFEM, quiet = quiet)
    }
    
    if (!keepDoseOnlySubjects && "EVID" %in% names(dfFFEM) && strID %in% names(dfFFEM)) {
      valid_ids <- unique(dfFFEM[[strID]][dfFFEM$EVID == 0])
      dfFFEM <- dfFFEM[dfFFEM[[strID]] %in% valid_ids, , drop = FALSE]
    }
    
    # 1B. Load FREM Data
    if (is.data.frame(strFREMData)) { 
      dfFREM <- as.data.frame(strFREMData) 
    } else if (file.exists(strFREMData)) { 
      dfFREM <- as.data.frame(data.table::fread(strFREMData, h = TRUE, data.table = FALSE, check.names = TRUE, showProgress = !quiet)) 
    } else { 
      stop("Cannot find FREM dataset: ", strFREMData) 
    }
    
    # 1C. Capture the incoming pristine data for the Phase 4.5 Validator
    dfFREM_input <- dfFREM
    
  } else {
    # Ensure variables exist even if we are skipping data generation
    dfFFEM <- NULL
    dfFREM <- NULL
    dfFREM_input <- NULL
  }
  
  if ((strUpdateType == "NoData" || strUpdateType == "NewInits") && !file.exists(paste0(tools::file_path_sans_ext(strFREMModel), ".ext"))) {
    stop(paste0("NoData and NewInits demands that ", paste0(tools::file_path_sans_ext(strFREMModel), ".ext"), " exists"))
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
    prepResult   <- prepareNewCovariates(dfFFEM                = dfFFEM, 
                                         cstrCatCovsToAdd      = cstrCatCovsToAdd, 
                                         cstrContCovsToAdd     = cstrContCovsToAdd, 
                                         cstrCovsToAddOrder    = cstrCovsToAddOrder, 
                                         existingCovNames      = covnames, 
                                         lastFremType          = iFremType, 
                                         iFremTypeIncrease     = 100, 
                                         strID                 = strID, 
                                         missVal               = missVal,
                                         overrideExistingCheck = overrideExistingCheck, 
                                         bRecodeDichotomous    = bRecodeDichotomous,
                                         allowNon01            = allowNon01,
                                         roundMeanTo           = roundMeanTo,
                                         fixTheta              = fixTheta,
                                         quiet                 = quiet)
    covList      <- prepResult$covList
    addedList    <- prepResult$addedList
    dfFFEM       <- prepResult$dfFFEM
    
    # 3c. Augment FREM Data
    dfFREM <- augmentFremData(
      dfFREM            = dfFREM, 
      dfFFEM            = dfFFEM, 
      covList           = covList, 
      addedList         = addedList, 
      covnames          = covnames, 
      cstrDV            = cstrDV, 
      strID             = strID, 
      iFremTypeIncrease = 100, 
      cstrSetToZero     = cstrSetToZero, 
      missVal           = missVal,         # <-- Explicit mapping
      quiet             = quiet            # <-- Explicit mapping
    )
    
    # --- PHASE 2 KEEP_COLS HEALING ---
    # If the user requested keep_cols that were generated during Phase 2 (e.g., SEX_2),
    # we must extract them from the updated wide dataset and merge them into the long dataset.
    if (!is.null(cstrKeepCols)) {
      missing_keeps <- cstrKeepCols[!cstrKeepCols %in% names(dfFREM)]
      cols_to_pull <- missing_keeps[missing_keeps %in% names(dfFFEM)]
      
      if (length(cols_to_pull) > 0) {
        dfFREM <- merge(dfFREM, dfFFEM[, c(strID, cols_to_pull), drop = FALSE], by = strID, all.x = TRUE)
      }
    }
  } else {
    covnames <- getCovNames(modFile = strFREMModel)
  }
  
  # --- 4. Finalize Data and Generate Model ---
  final_df <- finalizeFremData(dfFREM, strID, cstrKeepCols, bWriteData, strNewFREMData)
  
  # --- 4.5 STRICT DATA VALIDATION ---
  if (strUpdateType != "NoData" && !is.null(final_df)) {
    # Because updateFREMmodel lacks the original IGNORE statements to filter dfFFEM,
    # the "Ground Truth" for existing subjects is the input dfFREM's base records.
    # The "Ground Truth" for NEW subjects is dfFFEM.
    
    base_existing <- dfFREM_input[dfFREM_input$FREMTYPE == 0, , drop = FALSE]
    new_ids <- setdiff(unique(dfFFEM[[strID]]), unique(base_existing[[strID]]))
    
    if (length(new_ids) > 0) {
      base_new <- dfFFEM[dfFFEM[[strID]] %in% new_ids, , drop = FALSE]
      
      # Safely align columns for binding
      cols_to_keep <- intersect(names(base_existing), names(base_new))
      ground_truth <- rbind(base_existing[, cols_to_keep, drop = FALSE], 
                            base_new[, cols_to_keep, drop = FALSE])
    } else {
      ground_truth <- base_existing
    }
    
    # validateFremData will apply the DV != missVal rule to the ground_truth.
    # This safely filters new subjects, while leaving existing subjects (already filtered) intact.
    validateFremData(originalData = ground_truth, fremData = final_df, cstrDV = cstrDV, strID = strID, missVal=missVal,quiet = quiet)
  }
  
  # ---> ADD THIS SAFETY NET: Ensure strNewFREMData has a string for the in-memory $DATA record
  if (missing(strNewFREMData) || is.null(strNewFREMData)) {
    strNewFREMData <- "updated_frem_data.csv"
  }
  
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
