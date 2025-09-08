#' Finalize a FREM Dataset
#'
#' Applies final stable sorting and column selection to an augmented FREM dataset and
#' optionally writes it to a file. The stable sort preserves the original intra-subject
#' data record sequence (protecting EVID=4 resets) while correctly grouping new 
#' FREMTYPE covariates immediately after the first baseline record.
#'
#' @param dfFREM The augmented FREM data frame.
#' @param strID Character string specifying the subject identifier column.
#' @param cstrKeepCols Character vector of column names to keep in the final dataset.
#' @param bWriteData Logical; if TRUE, write the new data file.
#' @param strNewFREMData The file path for the new FREM dataset to be written.
#'
#' @return The final, processed data frame.
#'
#' @keywords internal
finalizeFremData <- function(dfFREM,
                             strID,
                             cstrKeepCols,
                             bWriteData,
                             strNewFREMData) {
  
  if (is.null(dfFREM) || nrow(dfFREM) == 0) {
    return(dfFREM)
  }
  
  # --- STABLE SORT IMPLEMENTATION ---
  # Protect intra-subject sequences (e.g., EVID=4 resets) by avoiding naive sorting on TIME.
  base_data <- dfFREM[dfFREM$FREMTYPE == 0, , drop = FALSE]
  pseudo_data <- dfFREM[dfFREM$FREMTYPE > 0, , drop = FALSE]
  
  # 1. Establish Subject Order (ID_IDX)
  # This guarantees subjects stay together and retain their original cohort order
  id_order <- unique(dfFREM[[strID]])
  
  if (nrow(base_data) > 0) {
    base_data$ID_IDX <- match(base_data[[strID]], id_order)
    # Create a strict intra-subject sequence (1, 2, 3...)
    base_data$SEQ <- ave(integer(nrow(base_data)), base_data$ID_IDX, FUN = seq_along)
  }
  
  if (nrow(pseudo_data) > 0) {
    pseudo_data$ID_IDX <- match(pseudo_data[[strID]], id_order)
    # Create a fractional intra-subject sequence (1.001, 1.002, 1.003...)
    # This forces pseudo-observations to slot exactly between row 1 and row 2
    # of the base data, mimicking the behavior of sorting by TIME=0,
    # but making it completely immune to EVID=4 TIME resets.
    pseudo_data$SEQ <- 1 + (ave(integer(nrow(pseudo_data)), pseudo_data$ID_IDX, FUN = seq_along) / 10000)
  }
  
  # Combine and perform a stable sort on ID then Sequence
  final_df <- rbind(base_data, pseudo_data)
  final_df <- final_df[order(final_df$ID_IDX, final_df$SEQ), ]
  
  # Clean up temporary indices
  final_df$ID_IDX <- NULL
  final_df$SEQ <- NULL
  
  dfFREM <- final_df
  
  # Column selection
  if (!is.null(cstrKeepCols)) {
    cols_to_keep <- cstrKeepCols[cstrKeepCols %in% names(dfFREM)]
    dfFREM <- dfFREM[, cols_to_keep, drop = FALSE]
  }
  
  # Write file if requested
  if (bWriteData) {
    write.csv(dfFREM, file = strNewFREMData, row.names = FALSE, quote = FALSE)
    if (!("FREMTYPE" %in% names(dfFREM))) {
      warning("No FREMTYPE available in dataset, add in cstrKeepCols and rerun updateFREM")
    }
  }
  
  return(dfFREM)
}
