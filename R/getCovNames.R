#' Get the covariate names from a FREM model file
#'
#' Figure out the FREM covariate names from a FREM model file. It
#'   relies on the covariate names added by PsN in the FREM code.
#'
#' @param modFile The name of the FREM model file
#' @param keepComment If FALSE (default), remove the leading ; and white space
#'   in front of the covariate name in the model file.
#'
#' @return
#' A list with three components:
#' * covNames = the names of the covariates as given in the model file. These
#'   corresponds to the covariate column names in the FREM data set created by
#'   PsN.
#' * polyCatCovs = the name of the dichotomized covariates created by PsN.
#' * orgCovNames = original covariate names (removing the frem specific ones)
#' @export
#'
#' @examples
#'
#' covList <- getCovNames(modFile = system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"))
#'
#' @family NONMEM Parsers
#' @concept nonmem_parsers
getCovNames <- function(modFile,
                        keepComment = FALSE) {
  mod <- scan(modFile, what = "character", sep = "\n", quiet = TRUE)
  fremStart <- grep(";;;FREM CODE BEGIN COMPACT", mod)
  fremEnd <- grep(";;;FREM CODE END COMPACT", mod)

  if (length(fremStart) == 0) {
    stop(paste("Could not find", ";;;FREM CODE BEGIN COMPACT", "in the model file. Is this a FREM model?"))
  }

  mod1 <- mod[(fremStart + 2):(fremEnd - 1)]
  covNames <- mod1[grep(";", mod1)]

  covNames <- str_replace(covNames, " 1", "") # Always remove " 1" in the end of the covNames

  if (!keepComment) {
    covNames <- str_replace(covNames, ";\\s*", "")
    covNames <- str_replace(covNames, " ", "")
  }

  orgCovNames <- sort(unique(.fremBaseCov(covNames)))

  ## The poly-categorical ones: binarized by PsN as <cov>_<level>
  fremCovs <- covNames[.fremIsBinarized(covNames)]

  return(list(
    covNames = covNames,
    polyCatCovs = fremCovs,
    orgCovNames = orgCovNames
  ))
}

## A FREM covariate is <cov>, or <cov>_<level> with an integer level for a
## binarized category (PsN's naming). Only that trailing suffix is the level:
## stripping "_.*" or the first "_[0-9]*" split a covariate whose own name holds
## an underscore (BL_BILI became BL, or BLBILI). Trailing blanks are allowed for
## getCovNames(keepComment = TRUE).
##
## @noRd
.fremBaseCov <- function(x) sub("_[0-9]+(\\s*)$", "\\1", x)

## @noRd
.fremIsBinarized <- function(x) grepl("_[0-9]+\\s*$", x)
