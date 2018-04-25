#' calcFFEM
#'
#' @description Computes the corresponding FFEM from a FREM model. Can handle missing covariates.
#' @param noBaseThetas Number of thetas that are not covariates
#' @param noCovThetas  Number of FREM covariates that are tested, i.e. number of thetas associated with covariate effects.
#' @param noSigmas  Number of sigma (epsilon) parameters in the model.
#' @param noParCov Number of parameters for which covariate relations are sought (often the same as noBaseThetas).
#' @param noSkipOm Number of omegas that should not be part of the FREM calculations. Such omegas has to come before the large FREM omega block.
#' @param dfExt A data frame of the ext file for the model. In case of mutiple $EST, the dfext should contain the estimates of the *one* relevant $EST.
#' @param parNames Names of the parameters
#' @param covNames Names of the covariates
#' @param availCov Names of the covariates to use in teh calculation of the FFEM model.
#' @param quiet If FALSE, will print the FFEM model + associated $OMEGA BLOCK
#'
#' @return A list with components Coefficients, Vars and Expr:
#' Coeffients: A noBaseThetas x availCov matrix with FFEM model coefficients
#' Vars:       A noBaseThetas x noBaseThetas matrix with the FFEM variances (OMEGA)
#' Expr:       A vector of FFEM expressions, one for each base model parameter.
#' @export
#'
#' @examples
#' \dontrun{
#' covList <- getCovNames(modFile="fremModel.mod")
#' }

calcFFEM <- function(noBaseThetas,noCovThetas,noSigmas,noParCov=noBaseThetas,noSkipOm=0,dfext,
                     parNames=paste("Par",1:noParCov,sep=""),covNames=paste("Cov",1:noCovThetas,sep=""),
                     availCov=covNames,quiet=FALSE) {

  ## Computes the corresponding FFEM from a FREM model. Can handle missing covariates.

  # noBaseThetas: Number of thetas that are not covariates
  # noCovThetas:  Number of FREM covariates that are tested, i.e. number of thetas associated with covariate effects
  # noParCov:     Number of parameters for which covariate relations are sought (often the same as noBaseTheta)
  # dfExt:        A data frame of the ext file for the model. In case of mutiple $EST, the dfext should contain the estimates of the *one* relevant $EST.
  # parNames:     Names of the parameters
  # covNames:     Names of the covariates
  # availCov:     Names of the non-missing covariates
  # quiet:        If FALSE, will print the FFEM model + associated $OMEGA BLOCK

  ## Return value: A list with components Coefficients, Vars and Expr
  ##    Coeffients: A noBaseThetas x availCov matrix with FFEM model coefficients
  ##    Vars:       A noBaseThetas x noBaseThetas matrix with the FFEM variances (OMEGA)
  ##    Expr:       A vector of FFEM expressions, one for each base model parameter.

  iNumFREMOM<-(noCovThetas+noParCov)*(noCovThetas+noParCov+1)/2
  iNumSigma <- noSigmas*(noSigmas+1)/2

  if (nrow(dfext)>1) dfext  <- dfext[dfext$ITERATION==-1000000000,]

  df_th  <- as.numeric(dfext[,2:(noBaseThetas+1)])
  df_thm <- as.numeric(dfext[,(noBaseThetas+2):(noBaseThetas+1+noCovThetas)])
  #df_om  <- as.numeric(dfext[,(noBaseThetas+5+noCovThetas):(noBaseThetas+4+noCovThetas+iNumFREMOM)])
  df_om  <- as.numeric(dfext[,(noBaseThetas+iNumSigma+2+noCovThetas+noSkipOm):(ncol(dfext)-1)])

  num_om <- -1/2+sqrt(1/4+2*iNumFREMOM)

 # transformed_params <- data.frame()
  om_matrix          <- as.numeric(df_om)

  browser()
  #Get the om-matrix
  OM                             <- matrix(0, nrow=num_om, ncol=num_om) #Define an empty matrix
  OM[upper.tri(OM,diag = TRUE)]  <- om_matrix #Assign upper triangular + diag
  tOM                            <- t(OM) #Get a transposed matrix
  OM[lower.tri(OM,diag = FALSE)] <- tOM[lower.tri(tOM,diag = FALSE)] #Assign the lower triangular except diag

  OM_PAR     <- OM[1:noParCov,1:noParCov] #The parameter covariance matrix
  OM_COV     <- OM[(noParCov+1):(noParCov+noCovThetas),(noParCov+1):(noParCov+noCovThetas)] #The covariates covariance matrix
  OM_PAR_COV <- OM[1:noParCov,(noParCov+1):(noParCov+noCovThetas)] #The covariance between covariates and parameters matrix


  if(length(availCov)!=0 & length(c(1:length(covNames))[!(covNames %in% availCov)])!=0) {
    missCov    <- c(1:length(covNames))[!(covNames %in% availCov)]
    #inv        <- inv[-missCov,-missCov]

    OM_COV <- OM_COV[-missCov,-missCov]
    inv    <- solve(OM_COV)

    OM_PAR_COV <-as.matrix(OM_PAR_COV[,-missCov])

    ## Fix the covariate names and means
    covNames   <- covNames[-missCov]
    df_thm     <- df_thm[-missCov]

    COEFF     <- OM_PAR_COV%*%inv #The parameter-covariate coefficients
    COEFF_VAR <- OM_PAR-OM_PAR_COV%*%inv%*%t(OM_PAR_COV) #The parameter variances

  } else if(length(c(1:length(covNames))[!(covNames %in% availCov)])==0) {
    inv       <- solve(OM_COV)
    COEFF     <- OM_PAR_COV%*%inv #The parameter-covariate coefficients
    COEFF_VAR <- OM_PAR-OM_PAR_COV%*%inv%*%t(OM_PAR_COV) #The parameter variances
  } else {
    COEFF     <- OM_PAR_COV
    #COEFF_VAR <- OM_PAR-OM_PAR_COV # Check with Jocke if this is the way it should be
    COEFF_VAR <- OM_PAR
  }

  ## Print the FFEM for inspeciton
  if(!quiet) {
    for(p in 1:nrow(COEFF)) {
      for(c in 1:ncol(COEFF)) {
        if(c==1) cat(parNames[p],":",sep="")
        if(length(availCov)==0) {
            cat(paste("0","*","(",covNames[c],"-",paste(round(df_thm[c],3),")",sep=""),sep=""))
        } else {
          cat(paste(round(COEFF[p,c],3),"*","(",covNames[c],"-",paste(round(df_thm[c],3),")",sep=""),sep=""))
        }
        if(c!=ncol(COEFF)) cat("+")
        if(c==ncol(COEFF)) cat("\n")
      }
    }
  }


  ## Create evaluable expression
  myExpr <- c()
  for(p in 1:nrow(COEFF)) {
    myExpr[p] <- ""
    for(c in 1:ncol(COEFF)) {
      if(length(availCov)==0) {
          myExpr[p] <- paste(myExpr[p],"0","*","(data$",covNames[c],"-",df_thm[c],")",sep="")
      } else {
        myExpr[p] <- paste(myExpr[p],COEFF[p,c],"*","(data$",covNames[c],"-",df_thm[c],")",sep="")
      }
      if(c!=ncol(COEFF)) myExpr[p] <- paste(myExpr[p],"+")
    }
  }

  if(!quiet) {
    cat(paste("\n$OMEGA BLOCK(",nrow(COEFF_VAR),")",sep=""),"\n")

    for(v in 1:nrow(COEFF_VAR)) {
      for(v2 in 1:v) {
        cat(round(COEFF_VAR[v,v2],5),"")
        if(v2==v) cat("\n")
      }
    }
  }

  return(
    invisible(
      list(Coefficients = COEFF,
           Vars         = COEFF_VAR,
           Expr         = myExpr
      )
    )
  )
}
