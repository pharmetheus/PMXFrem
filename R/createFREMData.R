#' Create a FREM dataset from a standard NONMEM dataset.
#'
#' Create a FREM dataset from a standard NONMEM dataset.
#'
#' If there are more than one DV variable in different columns in the FFEM data
#' set, they can be included in the DV column of the FREM data set with
#' different FREMTYPES by specifying the names of the columns in the `cstrDV`
#' argument.
#'
#' @param strFFEMData Name of FFEM-dataset (normal dataset) that will be used to
#'   create the FREM-dataset.
#' @param cstrKeepCols A vector of columns to keep in the dataset (for the
#'   updated new dataset).
#' @param cstrSetToZero A vector of variable names that should be set to zero on
#'   the covariate FREMTYPE lines. Typically columns with PK dosing information.
#' @param strFREMDataFileName Name of FREM dataset to create. Default=NULL => no
#'   dataset is written to disc
#' @param quiet If set to FALSE, the function outputs verbose information on
#'   what it is doing.
#' @param strID A string with the ID identifier column in the FFEM dataset.
#' @param cstrDV A vector of strings with DV variables that should be added,
#'   default="DV" (i.e. DV assumed fremtype=0), additional DVs are added with
#'   fremtype 1,2,3...etc.
#' @param cstrContCovs A vector of strings with continuous covariates to create.
#'   Default=NULL, i.e. no continuous covariates will be added
#' @param cstrCatCovs  A vector of strings with categorical covariates to
#'   create. Default=NULL, i.e. no categorical covariates will be added
#' @param bRecodeDichotomous Set to true if dichotomous covariates should be
#'   re-scaled to 1,0. E.g. if set to TRUE and SEX is coded as 1 or 2, a new
#'   variable SEXN_2 will be created with the values of 0 or 1. Only applicable
#'   if cstrCatCov is not zero and at least one of the cat covariates is
#'   dichotomous
#' @param cSortCols The column names to sort by (must  be present in
#'   cstrKeepCols)
#' @param cSortDirection The sort column order, ascending = 1, descending = -1,
#'   must be the same length as cSortCols
#' @param cFremtypes A vector of FREMTYPE values that each DV and covariate
#'   should use. The order should be: DV variables, continuous covariates,
#'   categorical covariates. Default=NULL, i.e.FREMTYPE of DV1=0,
#'   DV2=1,..,CONT1=100, ..CONTN=N*100, CAT1=(N+1+NumLevels1)*100,
#'   CATM=(N+M*NumLevelsM)*100
#'
#' @return A data.frame with the FREM data set.
#'
#' @section Side effetcs:
#'
#'   Will write a new FREM data set set to disc if strFREMDataFileName is not
#'   NULL.
#'
#' @export
#'
#' @examples
#' strFFEMData <- system.file("extdata/SimNeb/DAT-2-MI-PMX-2-onlyTYPE2-new.csv",
#'                             package = "PMXFrem")
#' tmp10 <- createFREMData(
#'           strFFEMData = strFFEMData,
#'           cstrCatCovs = c("SEX","SMOK","NCIL"),
#'           cstrContCovs = c("WT","BMI","AGE"))
#'
createFREMData <- function(
    strFFEMData,
    strFREMDataFileName        = NULL,
    quiet              = TRUE,
    strID              = "ID",
    cstrKeepCols       = c("ID", "TIME", "AMT", "II", "EVID", "SS", "RATE"),
    cstrSetToZero      = c("AMT", "II", "SS", "EVID", "RATE"),
    cstrDV             = "DV",
    cstrContCovs       = NULL,
    cstrCatCovs        = NULL,
    bRecodeDichotomous = TRUE,
    cSortCols          = c("ID", "TIME", "FREMTYPE"),
    cSortDirection     = c(1, 1, -1),
    cFremtypes         = NULL) {

  CovFremType       <- 100 # The Fremtype value for the next covariate
  FremtypeIncrement <- 100 # The FREMTYP incrementer for covariates
  iNewFremtypeDV    <- 0   # The start FREMTYPE for new DV


  ## define print function
  printq <- function(str, quiet) {
    if (!quiet) print(str)
  }

  ## Read in the data ##
  dfFFEM <- NULL
  dfFREM <- NULL

  if (file.exists(strFFEMData)) {
    dfFFEM <- fread(strFFEMData, h = T, data.table = FALSE, check.names = TRUE, showProgress = !quiet)
  } else {
    stop("Cannot find FFEM dataset: ", strFFEMData)
  }

  printq(paste0("Read the FFEM dataset, consisting of ", ncol(dfFFEM), " columns and ", nrow(dfFFEM), " rows"), quiet = quiet)

  ########################
  ## Sort out fremtypes ##
  ########################
  if (!is.null(cstrCatCovs)) {
    ## Need to figure out the number of levels for categorical covariates to
    ## manage polycothmous variables. Compute the number of non-missing
    ## categorical levels -1*the number categorical covariates
    cstrCatCovs <- cstrCatCovs[cstrCatCovs %in% names(dfFFEM)]
    numCatLevels <- dfFFEM %>%
      mutate_at(cstrCatCovs, function(x) length(unique(x[x != -99])) - 1) %>%
      select_at(cstrCatCovs) %>%
      slice(1) %>%
      sum(.)
  } else {
    numCatLevels <- 0
  }

  ## fremtypes are not provided by the user
  if (is.null(cFremtypes)) {
    ## DV fremtypes
    cFremtypes <- c(iNewFremtypeDV:(length(cstrDV) - 1))

    ## Continuous fremtypes
    if (!is.null(cstrContCovs)) {
      cFremtypes <- c(cFremtypes,
        seq(CovFremType, CovFremType + FremtypeIncrement * (length(cstrContCovs) - 1), by = FremtypeIncrement))
    }

    ## Categorical fremtypes
    maxval <- max(cFremtypes[cFremtypes >= CovFremType] + FremtypeIncrement, CovFremType)
    if (!is.null(cstrCatCovs)) {
      cFremtypes <- c(cFremtypes, seq(maxval, maxval + (numCatLevels - 1) * FremtypeIncrement, by = FremtypeIncrement))
    }
  }

  ## Make sure the cFremtypes are unique
  cFremtypes <- unique(cFremtypes)

  ## Check that the cFremtypes match the data
  if (length(cFremtypes) != length(cstrDV) + length(cstrContCovs) + numCatLevels) {
    stop("The number of fremtypes are not the same as the number of frem variables, i.e. ", length(cFremtypes), " != ", length(cstrDV) + length(cstrContCovs) + length(cstrCatCovs))
  }


  ## Identify the columns we should keep
  cstrKeepCols <- c(cstrKeepCols[cstrKeepCols %in% names(dfFFEM)], "FREMTYPE", cstrDV)

  ## Identify columns that should be set to zero on covariate lines
  cstrSetToZero <- cstrSetToZero[cstrSetToZero %in% cstrKeepCols]

  dfAddList <- list()
  ### Add new DVs for all individuals
  for (i in 1:length(cstrDV)) {
    strDV <- cstrDV[i]
    namestokeep <- unique(c(cstrKeepCols, strDV))
    dfDVData    <- dfFFEM[dfFFEM[[strDV]] != -99, namestokeep[namestokeep %in% names(dfFFEM)]] # Get the dataset with non-missing new DV values

    if (nrow(dfDVData) == 0) { # If all DVs are missing
      printq(paste0("No observations for ", strDV, " (fremtype=", cFremtypes[i], "); not adding any observations!"), quiet = quiet)
      warning(paste0("Note that it might be inconsistencies in DV fremtypes since fremtype ", cFremtypes[i], " is not present!"))

    } else {
      dfDVData$DV       <- dfDVData[[strDV]]
      dfDVData$FREMTYPE <- cFremtypes[i]
      dfDVData          <- dfDVData[, unique(cstrKeepCols)] # Only keep wanted columns
      dfAddList[[i]]    <- dfDVData
      printq(paste0("Adding ", nrow(dfDVData), " observations (", strDV, ") from ", nrow(dfDVData[!duplicated(dfDVData[[strID]]), ]), " individuals as fremtype ", cFremtypes[i]), quiet = quiet)
    }
  }

  # Add continuous covariates
  if (!is.null(cstrContCovs)) {
    for (i in 1:length(cstrContCovs)) {
      if (!cstrContCovs[i] %in% names(dfFFEM)) {
        printq(paste0("Can't add covariate ", cstrContCovs[i], ", not found in FFEM dataset. Skipping this covariate."), quiet = quiet)
      } else {
        dfDVData <- dfFFEM[dfFFEM[[cstrContCovs[i]]] != -99, ] # Get the dataset with non-missing covariate values
        dfDVData <- dfDVData[!duplicated(dfDVData[[strID]]), ] # Assume, time independent, i.e. one observation per individual

        if (nrow(dfDVData) > 0) { # If we have any non-missing cov values
          dfDVData$DV                        <- dfDVData[[cstrContCovs[i]]]
          dfDVData$FREMTYPE                  <- cFremtypes[i + length(cstrDV)]
          dfDVData                           <- dfDVData[, unique(cstrKeepCols)] # Only keep wanted columns
          dfDVData                           <- dfDVData %>% mutate_at(cstrSetToZero, function(x) return(0)) # Set the cstrSetToZero columns to zero
          dfAddList[[length(dfAddList) + 1]] <- dfDVData
          printq(paste0("Adding ", nrow(dfDVData), " covariate values (", cstrContCovs[i], ") from ", nrow(dfDVData), " individuals as fremtype ", cFremtypes[i + length(cstrDV)]), quiet = quiet)
        } else {
          printq(paste0("No non-missing covariate values for ", cstrContCovs[i], ". Skipping this covariate."), quiet = quiet)
        }

      }
    }
  }

  # Add categorical covariates
  if (!is.null(cstrCatCovs)) {
    k <- 1 # Counter for FREMTYPES
    for (i in 1:length(cstrCatCovs)) {

      strCov <- cstrCatCovs[i]
      if (!strCov %in% names(dfFFEM)) {
        printq(paste0("Can't add covariate ", strCov, ", not found in FFEM dataset. Skipping this covariate."), quiet = quiet)
      } else {
        dfDVData <- dfFFEM[dfFFEM[[strCov]] != -99, ] # Get the dataset with non-missing covariate values
        dfDVData <- dfDVData[!duplicated(dfDVData[[strID]]), ] # Assume, time independent, i.e. one observation per individual

        if (nrow(dfDVData) > 0) { # If we have any non-missing cov values
          uniqval <- sort(unique(dfDVData[[strCov]])) # Get the unique values of the categorical covariate, sort from smaller to bigger

          if (length(uniqval) > 2 || (length(uniqval) == 2 && bRecodeDichotomous)) { # Dichotomous covariate that should be recoded or higher order polycotomous cov

            for (j in 2:length(uniqval)) {
              strCov2                                               <- paste0(strCov, "_", uniqval[j])
              dfDVData[[strCov2]]                                   <- dfDVData[[strCov]]
              dfDVData[[strCov2]][dfDVData[[strCov]] == uniqval[j]] <- 1
              dfDVData[[strCov2]][dfDVData[[strCov]] != uniqval[j]] <- 0
              dfDVData$DV                                           <- dfDVData[[strCov2]]
              dfDVData$FREMTYPE                                     <- cFremtypes[k + length(cstrDV) + length(cstrContCovs)]
              # dfDVData                                              <- dfDVData[, unique(cstrKeepCols)] # Only keep wanted columns
              dfDVData                                              <- dfDVData %>% mutate_at(cstrSetToZero, function(x) return(0)) # Set the cstrSetToZero columns to zero
              dfAddList[[length(dfAddList) + 1]]                    <- dfDVData[, unique(cstrKeepCols)] # Only keep wanted columns
              printq(paste0("Adding ", nrow(dfDVData), " covariate values (", strCov2, ") from ", nrow(dfDVData), " individuals as fremtype ", cFremtypes[k + length(cstrDV) + length(cstrContCovs)]), quiet = quiet)
              k                                                     <- k + 1
            }
          } else { # Do not change the name of the covariate
            dfDVData$DV                        <- dfDVData[[strCov]]
            dfDVData$FREMTYPE                  <- cFremtypes[k + length(cstrDV) + length(cstrContCovs)]
            dfDVData                           <- dfDVData[, unique(cstrKeepCols)] # Only keep wanted columns
            dfDVData                           <- dfDVData %>% mutate_at(cstrSetToZero, function(x) return(0)) # Set the cstrSetToZero columns to zero
            dfAddList[[length(dfAddList) + 1]] <- dfDVData
            printq(paste0("Adding ", nrow(dfDVData), " covariate values (", strCov, ") from ", nrow(dfDVData), " individuals as fremtype ", cFremtypes[k + length(cstrDV) + length(cstrContCovs)]), quiet = quiet)
            k                                  <- k + 1
          }
        } else {
          printq(paste0("No non-missing covariate values for ", strCov, ". Skipping this covariate."), quiet = quiet)
        }
      }
    }
  }

  # Add new DVs and covariates to FREM dataset
  dfFREM <- rbind(dfFREM, as.data.frame(data.table::rbindlist(dfAddList)))

  if (!is.null(cSortCols)) { # Sort everything according to cSortCols
    cstrSortTxt <- NULL
    for (i in 1:length(cSortCols)) {
      strPrefix <- ""
      if (cSortDirection[i] == -1) strPrefix <- "-"
      cstrSortTxt <- c(cstrSortTxt, paste0(strPrefix, "dfFREM$", cSortCols[i]))
    }
    dfFREM <- eval(parse(text = paste0("dfFREM[order(", paste0(cstrSortTxt, collapse = ","), "),]")))
  }

  dfFREM <- dfFREM[, cstrKeepCols] # Only keep the specified columns
  if (!is.null(strFREMDataFileName)) {
    write.csv(dfFREM, file = strFREMDataFileName, row.names = FALSE, quote = FALSE)
  }

  return(dfFREM)
}
