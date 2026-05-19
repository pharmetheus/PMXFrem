#' Parse a NONMEM OMEGA or SIGMA text block into a matrix
#'
#' @param block A character vector representing the lines of an $OMEGA or $SIGMA block.
#'
#' @return A square numeric matrix.
#'
#' @family NONMEM Parsers Internal
#' @concept nonmem_parsers
#' @keywords internal
parseMatrixBlockToMatrix <- function(block) {
  # --- 1. Clean each line individually ---
  clean_lines <- sapply(block, function(line) {
    line <- gsub("(?i)^\\$\\w+\\s+", "", line, perl = TRUE)
    line <- gsub(";.*", "", line)
    return(trimws(line))
  }, USE.NAMES = FALSE)
  
  # --- 2. Combine clean lines and extract all items ---
  full_string <- paste(clean_lines, collapse = " ")
  items <- strsplit(full_string, "\\s+")[[1]]
  items <- items[nchar(items) > 0]
  
  # --- 3. Separate BLOCK(N) declaration from numeric values ---
  is_block_declaration <- grepl("BLOCK", items, ignore.case = TRUE)
  block_string <- items[is_block_declaration]
  
  value_strings <- items[!is_block_declaration]
  value_strings <- value_strings[!grepl("FIX", value_strings, ignore.case = TRUE)]
  
  values <- as.numeric(value_strings)
  if (any(is.na(values))) {
    warning("NAs introduced by coercion while parsing OMEGA/SIGMA block.")
    values <- values[!is.na(values)]
  }
  
  # --- 4. Construct the matrix ---
  if (length(block_string) > 0) {
    # Handle BLOCK matrix
    size <- as.numeric(gsub(".*BLOCK\\((\\d+)\\).*", "\\1", block_string[1]))
    expected_elements <- size * (size + 1) / 2
    
    if (length(values) != expected_elements) {
      stop(paste("Number of values in OMEGA BLOCK does not match expected number for a block of size", size))
    }
    
    mat <- matrix(0, nrow = size, ncol = size)
    
    # DEFINITIVE FIX: Fill the lower triangle row-by-row to match NONMEM convention
    value_index <- 1
    for (i in 1:size) {
      for (j in 1:i) {
        mat[i, j] <- values[value_index]
        value_index <- value_index + 1
      }
    }
    
    # Symmetrize the matrix
    mat[upper.tri(mat)] <- t(mat)[upper.tri(mat)]
    return(mat)
    
  } else {
    # Handle simple diagonal matrix
    size <- length(values)
    mat <- diag(values, nrow = size, ncol = size)
    return(mat)
  }
}
