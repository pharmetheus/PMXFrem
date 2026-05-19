#' Create the NONMEM model $OMEGA or $SIGMA code from a matrix with
#' variance-covariance information.
#'
#' @param matrix The matrix to be converted.
#' @param strName The name of the matrix, normally "$OMEGA" (default) or "$SIGMA".
#' @param assumesame Assume equal block matrix should be marked as SAME.
#' @param forceSingleBlock If TRUE, treats the entire matrix as a single block,
#'   ignoring any block-diagonal structure.
#'
#' @return A vector of character strings with the NONMEM representation of the matrix.
#' @family NONMEM Parsers Internal
#' @concept nonmem_parsers
#' @keywords internal
buildmatrix <- function(matrix,
                        strName    = "$OMEGA",
                        assumesame = TRUE,
                        forceSingleBlock = FALSE) {
  
  if (is.null(matrix) || nrow(matrix) == 0 || ncol(matrix) == 0) {
    return(character(0))
  }
  
  strres <- c()
  
  getnumstr <- function(blocksize, submat) {
    strvec <- rep("", blocksize)
    str    <- ""
    if (blocksize == 1 && !is.matrix(submat)) {
      return(paste0(submat))
    }
    for (i in 1:blocksize) {
      for (j in 1:i) {
        str <- paste0(str, submat[i, j], " ")
      }
      strvec[i] <- str
      str       <- ""
    }
    return(strvec)
  }
  
  if (forceSingleBlock) {
    block_size <- nrow(matrix)
    # FIX: Add a trailing space to be consistent with the default logic's output.
    strres <- c(strres, paste0(strName, " BLOCK(", block_size, ") "))
    strres <- c(strres, getnumstr(block_size, matrix))
    return(strres)
  }
  
  checksame <- function(mat, nb, po, o, i, as) {
    if (o == 0) { return("") }
    tmpmat <- as.matrix(mat[(o + 1):i, (o + 1):i])[1:nb, 1:nb]
    anmat  <- mat[(po + 1):o, (po + 1):o]
    if (((!is.matrix(tmpmat) && !is.matrix(anmat)) ||
         (nrow(tmpmat) == nrow(anmat) && ncol(tmpmat) == ncol(anmat))) &&
        all(tmpmat == anmat) && as == TRUE) {
      return("SAME")
    }
    return("")
  }
  
  offset     <- 0
  prevoffset <- 0
  numinblock <- 1
  
  for (i in 1:nrow(matrix)) {
    if ((i - 1) > offset && matrix[1 + offset, i] != 0) numinblock <- numinblock + 1
    if ((i - 1) > offset && matrix[1 + offset, i] == 0) {
      strSAME <- checksame(matrix, numinblock, prevoffset, offset, i, assumesame)
      strres  <- c(strres, paste0(strName, " BLOCK(", numinblock, ") ", strSAME))
      if (strSAME == "") strres <- c(strres, getnumstr(numinblock, matrix[(offset + 1):i, (offset + 1):i]))
      numinblock <- 1
      prevoffset <- offset
      offset     <- i - 1
    }
  }
  strSAME <- checksame(matrix, numinblock, prevoffset, offset, i, assumesame)
  strres  <- c(strres, paste0(strName, " BLOCK(", numinblock, ") ", strSAME))
  if (strSAME == "") strres <- c(strres, getnumstr(numinblock, matrix[(offset + 1):i, (offset + 1):i]))
  
  return(strres)
}
