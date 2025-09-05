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

  CovFremType       <- 100
  FremtypeIncrement <- 100
  iNewFremtypeDV    <- 0

  printq <- function(str, quiet) {
    if (!quiet) print(str)
  }

  dfFFEM <- NULL
  if (file.exists(strFFEMData)) {
    dfFFEM <- data.table::fread(strFFEMData, h = TRUE, data.table = FALSE, check.names = TRUE, showProgress = !quiet)
  } else {
    stop("Cannot find FFEM dataset: ", strFFEMData)
  }
  printq(paste0("Read the FFEM dataset, consisting of ", ncol(dfFFEM), " columns and ", nrow(dfFFEM), " rows"), quiet = quiet)

  valid_ContCovs <- cstrContCovs[cstrContCovs %in% names(dfFFEM)]
  valid_CatCovs  <- cstrCatCovs[cstrCatCovs %in% names(dfFFEM)]

  skipped_ContCovs <- setdiff(cstrContCovs, valid_ContCovs)
  for (cov in skipped_ContCovs) {
    printq(paste0("Can't add covariate ", cov, ", not found in FFEM dataset. Skipping this covariate."), quiet = quiet)
  }
  skipped_CatCovs <- setdiff(cstrCatCovs, valid_CatCovs)
  for (cov in skipped_CatCovs) {
    printq(paste0("Can't add covariate ", cov, ", not found in FFEM dataset. Skipping this covariate."), quiet = quiet)
  }

  numCatLevels <- 0
  if (length(valid_CatCovs) > 0) {
    numCatLevels <- dfFFEM %>%
      dplyr::mutate_at(valid_CatCovs, function(x) pmax(0, length(unique(x[x != -99])) - 1)) %>%
      dplyr::select_at(valid_CatCovs) %>%
      dplyr::slice(1) %>%
      sum(.)
  }

  if (is.null(cFremtypes)) {
    cFremtypes <- c(iNewFremtypeDV:(length(cstrDV) - 1))
    if (length(valid_ContCovs) > 0) {
      cFremtypes <- c(cFremtypes,
                      seq(CovFremType, CovFremType + FremtypeIncrement * (length(valid_ContCovs) - 1), by = FremtypeIncrement))
    }
    maxval <- max(c(0, cFremtypes[cFremtypes >= CovFremType])) + FremtypeIncrement
    if (length(valid_CatCovs) > 0 && numCatLevels > 0) {
      cFremtypes <- c(cFremtypes, seq(maxval, maxval + (numCatLevels - 1) * FremtypeIncrement, by = FremtypeIncrement))
    }
  }

  cFremtypes <- unique(cFremtypes)

  if (length(cFremtypes) != length(cstrDV) + length(valid_ContCovs) + numCatLevels) {
    stop("The number of fremtypes are not the same as the number of frem variables.")
  }

  # <<< FIX for Core Bug: Define the full set of columns to return correctly.
  cols_to_return <- unique(c(cstrKeepCols, "FREMTYPE", cstrDV))
  cstrSetToZero <- cstrSetToZero[cstrSetToZero %in% names(dfFFEM)]

  dfAddList <- list()
  for (i in seq_along(cstrDV)) {
    strDV <- cstrDV[i]
    dfDVData <- dfFFEM[dfFFEM[[strDV]] != -99, ]
    if (nrow(dfDVData) > 0) {
      dfDVData$DV <- dfDVData[[strDV]]
      dfDVData$FREMTYPE <- cFremtypes[i]
      dfAddList[[length(dfAddList) + 1]] <- dfDVData
    } else {
      warning(paste0("Note that it might be inconsistencies in DV fremtypes since fremtype ", cFremtypes[i], " is not present!"))
    }
  }

  if (length(valid_ContCovs) > 0) {
    for (i in seq_along(valid_ContCovs)) {
      cov_name <- valid_ContCovs[i]
      dfDVData <- dfFFEM[dfFFEM[[cov_name]] != -99, ]
      dfDVData <- dfDVData[!duplicated(dfDVData[[strID]]), ]
      if (nrow(dfDVData) > 0) {
        dfDVData$DV <- dfDVData[[cov_name]]
        dfDVData$FREMTYPE <- cFremtypes[i + length(cstrDV)]
        dfDVData <- dplyr::mutate_at(dfDVData, .vars = cstrSetToZero, .funs = function(x) return(0))
        dfAddList[[length(dfAddList) + 1]] <- dfDVData
      } else {
        # <<< FIX for Typo Bug (added a space)
        printq(paste0("No non-missing covariate values for ", cov_name, ". Skipping this covariate."), quiet = quiet)
      }
    }
  }

  if (length(valid_CatCovs) > 0) {
    k <- 1
    for (i in seq_along(valid_CatCovs)) {
      strCov <- valid_CatCovs[i]
      dfDVData <- dfFFEM[dfFFEM[[strCov]] != -99, ]
      dfDVData <- dfDVData[!duplicated(dfDVData[[strID]]), ]
      if (nrow(dfDVData) > 0) {
        uniqval <- sort(unique(dfDVData[[strCov]]))
        if (length(uniqval) > 2 || (length(uniqval) == 2 && bRecodeDichotomous)) {
          for (j in 2:length(uniqval)) {
            dfTemp <- dfDVData
            strCov2 <- paste0(strCov, "_", uniqval[j])
            dfTemp[[strCov2]] <- as.numeric(dfTemp[[strCov]] == uniqval[j])
            dfTemp$DV <- dfTemp[[strCov2]]
            dfTemp$FREMTYPE <- cFremtypes[k + length(cstrDV) + length(valid_ContCovs)]
            dfTemp <- dplyr::mutate_at(dfTemp, .vars = cstrSetToZero, .funs = function(x) return(0))
            dfAddList[[length(dfAddList) + 1]] <- dfTemp
            k <- k + 1
          }
        } else {
          dfDVData$DV <- dfDVData[[strCov]]
          dfDVData$FREMTYPE <- cFremtypes[k + length(cstrDV) + length(valid_ContCovs)]
          dfDVData <- dplyr::mutate_at(dfDVData, .vars = cstrSetToZero, .funs = function(x) return(0))
          dfAddList[[length(dfAddList) + 1]] <- dfDVData
          k <- k + 1
        }
      } else {
        # <<< FIX for Typo Bug (added a space)
        printq(paste0("No non-missing covariate values for ", strCov, ". Skipping this covariate."), quiet = quiet)
      }
    }
  }

  if (length(dfAddList) > 0) {
    dfFREM <- as.data.frame(data.table::rbindlist(dfAddList, use.names = TRUE, fill = TRUE))
  } else {
    dfFREM <- as.data.frame(matrix(ncol = length(cols_to_return), nrow = 0))
    names(dfFREM) <- cols_to_return
  }

  missing_cols <- setdiff(cols_to_return, names(dfFREM))
  if (length(missing_cols) > 0) {
    dfFREM[, missing_cols] <- NA
  }

  if (!is.null(cSortCols) && nrow(dfFREM) > 0) {
    cstrSortTxt <- NULL
    for (i in seq_along(cSortCols)) {
      strPrefix <- ""
      col_to_sort <- paste0("as.numeric(dfFREM$", cSortCols[i], ")")
      if (cSortDirection[i] == -1) strPrefix <- "-"
      cstrSortTxt <- c(cstrSortTxt, paste0(strPrefix, col_to_sort))
    }
    dfFREM <- eval(parse(text = paste0("dfFREM[order(", paste0(cstrSortTxt, collapse = ","), "),]")))
  }

  dfFREM <- dfFREM[, cols_to_return, drop = FALSE]

  if (!is.null(strFREMDataFileName)) {
    write.csv(dfFREM, file = strFREMDataFileName, row.names = FALSE, quote = FALSE)
  }

  return(as.data.frame(dfFREM))
}
