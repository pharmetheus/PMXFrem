#' getModel
#'
#' @description Parse the information in a FREM model file and corresponding .ext file and returns a list of relevant model characteristics.
#' @param runno The run number. Will be used to create modName (if it isn't specified) by appendning "runno" to  "run".
#' @param modName The name of the model, e.g. "run10". The corresponding model file and ext files will be cfeated by appending ".mod" and .ext".
#' @param modDir The directory in which the model and ext files can be found.
#'
#' @return A list with nine components:
#'
#' covnames = a list of two component (the putput from getCovNames); (1=covNames) the names of the covariates as given in the model file and
#' (2=polyCatCovs) = the name of the dichotomized covariates created by PsN.
#'
#' noCovThetas= The number of covariate thetas.
#'
#' dfExt = the ITERATION=-1000000000 line from the .ext file.
#'
#' iNumTHETA = The number of THETAs.
#'
#' noBaseThetas = The number of non-covariate THETAs.
#'
#' noBaseEtas = The number of non-covariate ETAs (set to noBaseThetas).
#'
#' thbasevector = A vector of the base THETA parameter estimates.
#'
#' thcovvector  = A vector of the covariate THETA parameter estimates.
#'
#' sigvector    = A vector of the non-zero SIGMA parameter estimates (typically the diagonal elements).
#' @export
#'
#' @examples
#' \dontrun{
#' getModel(20)
#' }
getModel <- function(runno=NULL,modName=paste0("run",runno),modDir="./") {
  covnames     <- getCovNames(modFile=paste0(modDir,modName,".mod"),keepComment=FALSE) #Get the covariate names from the FREM model
  noCovThetas  <- length(covnames$covNames) # the number of covariates in the frem model

  dfext        <- subset(getExt(extFile = paste0(modDir,modName,'.ext')),ITERATION=="-1000000000") #REad in parameter values
  iNumTHETA    <- length(names(dfext)[regexpr('THETA.*',names(dfext))==1])
  noBaseThetas <- iNumTHETA - noCovThetas
  noBaseEtas   <- noBaseThetas

  thbasevector <- as.numeric(dfext[2:(noBaseThetas+1)])

  df_thm       <- as.numeric(dfext[,(noBaseThetas+2):(noBaseThetas+1+noCovThetas)]) #Get the mean estimate from the FREM covariates

  ## Get the sigmas from ext and select the diagonals by assuming the off-diagonals are 0.
  sigmas       <- dfext[names(dfext)[regexpr("SIGMA",names(dfext))==1]]
  sigmas       <- sigmas[sigmas!=0]

  return(list(covnames=covnames,
              noCovThetas=noCovThetas,
              dfext=dfext,
              iNumTHETA=iNumTHETA,
              noBaseThetas=noBaseThetas,
              noBaseEtas = noBaseEtas,
              thbasevector=thbasevector,
              thcovvector=df_thm,
              sigvector=sigmas))
}
