#' Finalize a FREM Dataset
#'
#' Applies final sorting and column selection to an augmented FREM dataset and
#' optionally writes it to a file.
#'
#' @param dfFREM The augmented FREM data frame.
#' @param sortFREMDataset Character vector of columns to sort the final dataset by.
#'   This argument is required and cannot be NULL.
#' @param cstrKeepCols Character vector of column names to keep in the final dataset.
#' @param bWriteData Logical; if TRUE, write the new data file.
#' @param strNewFREMData The file path for the new FREM dataset to be written.
#'
#' @return The final, processed data frame.
#'
finalizeFremData <- function(dfFREM,
                             sortFREMDataset,
                             cstrKeepCols,
                             bWriteData,
                             strNewFREMData) {
  
  if (is.null(dfFREM)) {
    return(NULL)
  }
  
  # Per your proposal, this function now assumes sortFREMDataset is always provided.
  # The redundant if/else structure has been removed.
  if (is.null(cstrKeepCols)) {
    dfFREM <- dfFREM %>% dplyr::arrange(!!!rlang::syms(sortFREMDataset))
  } else {
    dfFREM <- dfFREM %>% dplyr::arrange(!!!rlang::syms(sortFREMDataset)) %>% dplyr::select(dplyr::one_of(cstrKeepCols))
  }
  
  # Write file if requested
  if (bWriteData) {
    write.csv(dfFREM, file = strNewFREMData, row.names = FALSE, quote = FALSE)
    if (!("FREMTYPE" %in% names(dfFREM))) warning("No FREMTYPE available in dataset, add in cstrKeepCols and rerun updateFREM")
  }
  
  return(dfFREM)
}