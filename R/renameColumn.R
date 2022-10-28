#' renameColumn
#'
#' @description Change name of a column
#' @param data Data frame
#' @param oldvar The name of the column to rename.
#' @param newvar The new name
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' testDf <- data.frame(XID=rep(c(100,200,300),each=2),DV=2)
#' renameColumn(testDf,oldvar="XID",newvar="ID")
renameColumn <- function(data,oldvar="ID",newvar="ID") {
  oldvar <- sym(oldvar)
  newvar <- sym(newvar)
  newDf <- eval_tidy(quo(data %>% rename(UQ(newvar) := UQ(oldvar))))
  return(newDf)
}
