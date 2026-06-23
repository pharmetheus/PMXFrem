#' Generate NONMEM code for Cholesky Decomposition
#'
#' @description
#' Generates the NONMEM `$PK` or `$PRED` block equations required to implement a 
#' Cholesky decomposition. This dynamically writes the math to factorize the 
#' variance-covariance matrix (`V`) into a lower triangular matrix (`L`), and maps 
#' independent standard normal `ETA`s to correlated `MYETA`s.
#'
#' @param n Integer. The dimension of the variance-covariance matrix (number of ETAs).
#' @param eta_offset Integer. The offset for the ETA indexing (typically equivalent to `numSkipOm`). Defaults to 0.
#'
#' @return A character vector of NONMEM equations ready for injection.
#' 
#' @family FFEM Conversion Internal
#' @concept ffem_conversion
#' @keywords internal
#' @noRd
generate_cholesky_lines <- function(n, eta_offset = 0) {
  lines <- c()
  lines <- c(lines, "; ----------------------------------------------------------------")
  lines <- c(lines, sprintf("; 1. Cholesky factors (L) for an %dx%d matrix", n, n))
  if (eta_offset > 0) {
    lines <- c(lines, sprintf(";    (Offset applied: starting at index %d)", eta_offset + 1))
  }
  lines <- c(lines, "; ----------------------------------------------------------------")
  
  # Generate equations for the L factors (column by column)
  for (j in seq_len(n)) {
    jo <- j + eta_offset
    
    lines <- c(lines, "", sprintf("; Column %d", jo))
    for (i in j:n) {
      io <- i + eta_offset
      
      if (i == j) {
        if (j == 1) {
          lines <- c(lines, sprintf("L%d%d = SQRT(MAX(0.000001, V%d%d))", io, jo, io, jo))
        } else {
          ko_seq <- (1:(j-1)) + eta_offset
          terms <- paste0("L", io, ko_seq, "**2", collapse = " - ")
          lines <- c(lines, sprintf("L%d%d = SQRT(MAX(0.000001, V%d%d - %s))", io, jo, io, jo, terms))
        }
      } else {
        if (j == 1) {
          lines <- c(lines, sprintf("L%d%d = V%d%d / L%d%d", io, jo, io, jo, jo, jo))
        } else {
          ko_seq <- (1:(j-1)) + eta_offset
          terms <- paste0("L", io, ko_seq, " * L", jo, ko_seq, collapse = " - ")
          lines <- c(lines, sprintf("L%d%d = (V%d%d - %s) / L%d%d", io, jo, io, jo, terms, jo, jo))
        }
      }
    }
  }
  
  # Generate equations for MYETA
  lines <- c(lines, "", "; ----------------------------------------------------------------")
  lines <- c(lines, "; 2. Multiply standard ETA with L for correlated MYETA")
  lines <- c(lines, "; ----------------------------------------------------------------")
  for (i in seq_len(n)) {
    io <- i + eta_offset
    ko_seq <- (1:i) + eta_offset
    
    terms <- paste0("L", io, ko_seq, " * ETA(", ko_seq, ")", collapse = " + ")
    lines <- c(lines, sprintf("MYETA%d = %s", io, terms))
  }
  
  return(lines)
}