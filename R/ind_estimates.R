#' ind_estimates 
#'
#' @description Computes individual estimates of parameters using posterior Bayes estimation. Assumes no eta-eps interaction and a homoscedastic error.
#' @param cData Vector of data points for this individual.
#' @param cTime Vector of independent variable (e.g. time points) for this individual.
#' @param theta Vector of fixed effects.
#' @param om    Omega matrix (IIV).
#' @param sig   igma matrix (RUV)
#' @param start_eta Start point for the EBE-search, default = typical values, i.e. vector of 0.
#' @param eta_fixed A vector of the same length as start_eta with the number of fixed/constant etas, constant implies that they are fixed to 0, i.e. not the same as OMEGA fixed.
#' @param modelfunction The function with the model to use.
#'
#' @return List of individual estimates (EBEs) and the variance-covariace matrix of the estimated EBEs.
#' @export
#'
#' @examples
#' #' \dontrun{
#' modelfunc <- function(age,thetas,etas) {
#'      pred <- calcHAZVector(age,basethetas = thetas[1:6],covthetas=thetas[7:12],etas=etas)
#'      return(pred)
#' }
#'
#' ebe_est <-ind_estimates(cData=as.numeric(obsDf$DV), 
#'                         cTime=as.numeric(obsDf$AGE), 
#'                         theta=as.numeric(thetaDf[1,-1]), 
#'                         om=om, 
#'                         sig=sig, 
#'                         start_eta = rep(0,6),
#'                         modelfunction=modelfunc)
#' }
ind_estimates <-function(cData,cTime,theta,om,sig,start_eta=rep(0,nrow(om)),eta_fixed=rep(FALSE,length(start_eta)),modelfunction){
  
  ## Removed fix parameters
  start_eta <- start_eta[which(eta_fixed==FALSE)] 
  om        <- om[which(eta_fixed==FALSE),which(eta_fixed==FALSE)] 
  
  c1 <-  length(start_eta)/2*log(2*pi)
  c2 <-  1/2*log(det(om))
  c3 <-  solve(om)
  
  ## Assume no interaction model and additive (homeoscedastic) residual error
  h<-rep(1,length(cTime))
  if (length(h)!=1) {
    res_var <- diag(as.numeric(h*sig*t(h)))
  } else {
    res_var<-as.matrix(h*sig*t(h))
  }
  
  ## Calulcate inverse of cholesky factorized residual variance
  lC          <- solve(chol(res_var)) 
  
  ## Calculate determinant of residual variance
  det_res_var <- det(res_var) 
  
  ## This is the function that should be minimized, w.r.t eta
  min_function <- function(eta_ind,cData,cTime,theta,om,sig,c1,c2,c3,lC,det_res_var,eta_fixed,modelfunction) {
    li  <- ind_likelihood(cData,cTime,theta,eta_ind,det_res_var,lC,eta_fixed,modelfunction) #Individual log likelihood
    ret <- c1+c2+1/2*t(eta_ind)%*%c3%*%eta_ind-li
  }
  
  ## Function for calculaiton of the individual log likelihood
  ind_likelihood <-function(cData,cTime,theta,eta,det_res_var,lC,eta_fixed,modelfunction) {
    cIpred <- modelfunction(cTime,theta,etas=ifelse(eta_fixed==FALSE,eta,0))
    res    <-  cData-cIpred 
    R      <- (t(res)%*%lC)
    li     <-  -1/2*log(det_res_var)-1/2*R%*%t(R)
    return(li)
  }
  
  ## This could be changed to any optimization method in R or user written
  ebe <- nlm(min_function,start_eta,cData,cTime,theta,om,sig,c1,c2,c3,lC,det_res_var,eta_fixed,modelfunction,hessian=TRUE)
  
  return(list(ebe$estimate,solve(ebe$hessian)))
}



                    