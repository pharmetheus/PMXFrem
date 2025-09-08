#' Prepare New Covariates for Addition to a FREM Model
#'
#' This function processes lists of continuous and categorical covariates to be
#' added to a FREM model. It handles the re-classification of binary
#' categorical variables, calculates initial estimates for the mean (THETA) and
#' variance (OMEGA), assigns new FREMTYPEs, and creates binarized columns in the
#' FFEM dataset for polytomous categorical covariates.
#'
#' @param dfFFEM The FFEM data frame containing covariate information for all subjects.
#'   This data frame may be modified by adding new binarized columns.
#' @param cstrCatCovsToAdd A character vector of categorical covariate names to add.
#' @param cstrContCovsToAdd A character vector of continuous covariate names to add.
#' @param cstrCovsToAddOrder An optional character vector specifying the order of
#'   processing. If NULL, covariates are processed alphabetically.
#' @param existingCovNames A list of existing covariate names (from getCovNames)
#'   to prevent adding duplicates.
#' @param lastFremType The last (highest) FREMTYPE value currently in use.
#' @param iFremTypeIncrease The integer value by which to increment FREMTYPE for
#'   each new covariate.
#' @param strID The name of the subject identifier column.
#' @param overrideExistingCheck A logical flag to allow adding covariates that
#'   already exist.
#' @param quiet A logical flag to suppress printed messages.
#'
#' @return A list containing four elements:
#'   \item{covList}{A list where each element contains the prepared information for a new covariate (Name, Mean, Var, Fremtype, Data).}
#'   \item{addedList}{A character vector of the names of the covariates that were successfully prepared for addition (e.g., c("AGE", "SITE_2", "SITE_3")).}
#'   \item{dfFFEM}{The potentially modified dfFFEM data frame with new binarized columns.}
#'   \item{lastFremType}{The updated last (highest) FREMTYPE value.}
#'
prepareNewCovariates <- function(dfFFEM,
                                 cstrCatCovsToAdd,
                                 cstrContCovsToAdd,
                                 cstrCovsToAddOrder,
                                 existingCovNames,
                                 lastFremType,
                                 iFremTypeIncrease,
                                 strID,
                                 overrideExistingCheck,
                                 quiet) {
  
  printq <- function(str, quiet) {
    if (!quiet) print(str)
  }
  
  # --- This is the logic block moved from updateFREMmodel.R ---
  printq(paste0("Variables already in FREM model (n=", length(existingCovNames$orgCovNames), "): ", paste0(existingCovNames$orgCovNames, collapse = " ")), quiet = quiet)
  if (!is.null(cstrContCovsToAdd)) printq(paste0("Continuous covariates that will be added to FREM (n=", length(cstrContCovsToAdd), "): ", paste0(cstrContCovsToAdd, collapse = " ")), quiet = quiet)
  if (!is.null(cstrCatCovsToAdd)) printq(paste0("Categorical covariates that will be added to FREM (n=", length(cstrCatCovsToAdd), "): ", paste0(cstrCatCovsToAdd, collapse = " ")), quiet = quiet)
  
  iFremType <- lastFremType
  
  if (is.null(cstrCovsToAddOrder)) cstrCovsToAddOrder <- sort(c(cstrContCovsToAdd, cstrCatCovsToAdd))
  
  covList <- list()
  addedList <- c()
  
  for (strCov in cstrCovsToAddOrder) {
    # Re-classify binary categorical covariates as continuous
    if (strCov %in% cstrCatCovsToAdd && length(unique(dfFFEM[[strCov]][dfFFEM[[strCov]] != -99])) == 2) {
      cstrContCovsToAdd <- c(strCov, cstrContCovsToAdd)
      cstrCatCovsToAdd  <- cstrCatCovsToAdd[-which(cstrCatCovsToAdd == strCov)]
      printq(paste0("Switching: ", strCov, ", from categorical to continuous since only 2 levels"), quiet = quiet)
    }
    
    if (strCov %in% cstrContCovsToAdd) { # Add a continuous covariate
      if (!strCov %in% existingCovNames$covNames || overrideExistingCheck == TRUE) {
        tmp               <- dfFFEM[dfFFEM[[strCov]] != -99, ]
        tmp               <- tmp[!duplicated(tmp[[strID]]), c(strID, strCov)]
        iFremType         <- iFremType + iFremTypeIncrease
        l                 <- list(Name = strCov, Mean = mean(tmp[[strCov]]), Var = var(tmp[[strCov]]), Fremtype = iFremType)
        l[["Data"]]       <- tmp
        covList[[strCov]] <- l
        printq(paste0("Identifying new covariate: ", strCov, " with fremtype ", iFremType), quiet = quiet)
        addedList         <- c(addedList, strCov)
      } else {
        printq(paste0("Skipping continuous covariate: ", strCov, ", already existing as fremtype"), quiet = quiet)
      }
    } else { # Add a categorical covariate
      covValues <- sort(unique(dfFFEM[[strCov]][dfFFEM[[strCov]] != -99]))
      for (j in 2:length(covValues)) {
        strCov2 <- paste0(strCov, "_", covValues[j])
        
        if (!strCov2 %in% existingCovNames$covNames || overrideExistingCheck == TRUE) {
          dfFFEM[[strCov2]] <- dfFFEM[[strCov]]
          dfFFEM[[strCov2]][dfFFEM[[strCov]] != -99 & dfFFEM[[strCov]] == covValues[j]] <- 1
          dfFFEM[[strCov2]][dfFFEM[[strCov]] != -99 & dfFFEM[[strCov]] != covValues[j]] <- 0
          
          tmp                <- dfFFEM[dfFFEM[[strCov2]] != -99, ]
          tmp                <- tmp[!duplicated(tmp[[strID]]), c(strID, strCov2)]
          iFremType          <- iFremType + iFremTypeIncrease
          l                  <- list(Name = strCov2, Mean = mean(tmp[[strCov2]]), Var = var(tmp[[strCov2]]), Fremtype = iFremType)
          l[["Data"]]        <- tmp
          covList[[strCov2]] <- l
          printq(paste0("Identifying new covariate: ", strCov2, " with fremtype ", iFremType), quiet = quiet)
          addedList          <- c(addedList, strCov2)
        } else {
          printq(paste0("Skipping category: ", strCov2, ", already existing as fremtype"), quiet = quiet)
        }
      }
    }
  }
  # --- End of moved logic block ---
  
  return(list(
    covList = covList,
    addedList = addedList,
    dfFFEM = dfFFEM,
    lastFremType = iFremType
  ))
}