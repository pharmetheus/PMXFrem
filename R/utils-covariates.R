#' Each subject's covariate value as the FREM data set sees it
#'
#' The FREM data set takes, for each subject, the first record that holds a
#' value - not `missVal` and not `NA`. Phase 1 and phase 2 of
#' [createFREMmodel()] both summarise a covariate this way, so that its mean,
#' its name and whether its THETA is fixed do not depend on where it sits in
#' `covariates`.
#'
#' @param df A data frame with one or more records per subject.
#' @param cov The covariate column.
#' @param strID The subject identifier column.
#' @param missVal The missing-value indicator.
#' @return A named vector: one value per subject that has one, named by subject.
#' @keywords internal
#' @noRd
.fremCovFirstValues <- function(df, cov, strID = "ID", missVal = -99) {
  x <- df[[cov]]
  has <- !is.na(x) & x != missVal
  ids <- df[[strID]][has]
  stats::setNames(x[has][!duplicated(ids)], ids[!duplicated(ids)])
}

#' Does any subject lack a value for a covariate?
#'
#' `TRUE` when at least one subject has no record holding a value, which is
#' when the covariate's THETA is estimated rather than fixed. A missing value
#' on some records of a subject that has a value elsewhere does not count:
#' the FREM data set never sees it.
#'
#' @inheritParams .fremCovFirstValues
#' @return A single logical.
#' @keywords internal
#' @noRd
.fremCovHasMissing <- function(df, cov, strID = "ID", missVal = -99) {
  length(.fremCovFirstValues(df, cov, strID, missVal)) < length(unique(df[[strID]]))
}
