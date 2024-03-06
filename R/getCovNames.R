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
#' covList <- getCovNames(modFile = system.file("extData/SimNeb/run31.mod", package = "PMXFrem"))
#'
getCovNames <- function(modFile,
                        keepComment = FALSE) {

  mod       <- scan(modFile, what = "character", sep = "\n", quiet = TRUE)
  fremStart <- grep(";;;FREM CODE BEGIN COMPACT", mod)
  fremEnd   <- grep(";;;FREM CODE END COMPACT", mod)

  if (length(fremStart) == 0) {
    stop(paste("Could not find", ";;;FREM CODE BEGIN COMPACT", "in the model file. Is this a FREM model?"))
  }

  mod1      <- mod[(fremStart + 2):(fremEnd - 1)]
  covNames  <- mod1[grep(";", mod1)]

  covNames  <- str_replace(covNames, " 1", "") # Always remove " 1" in the end of the covNames

  if (!keepComment) {
    covNames  <- str_replace(covNames, ";\\s*", "")
    covNames  <- str_replace(covNames, " ", "")
  }

  orgCovNames <- sort(unique(str_replace(covNames, "_.*", "")))

  ## Figure out which ones that are poly-categorical, i.e. those with an '_' in the name
  fremCovs  <- covNames[grep("_", x = covNames)]

  return(list(covNames = covNames,
    polyCatCovs = fremCovs,
    orgCovNames = orgCovNames)
  )

}
