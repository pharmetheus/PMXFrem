#' deltaRule
#'
#' @description Compute the uncertainty in a transformed variable using the delta rule and numerical derivatives.
#' @param ebes The parameters of the transformation.
#' @param varCov The variance-covariance matrix of the parameters.
#' @param transform_fun The function that will transform the parameters to the new variable.
#' @param bIncludeCov Boolean, if covariances should be included in the variance transformations or not.
#' @param ... Additional parameters passed on to the transform_fun function.
#'
#' @return A named vector with the tranformed parameter value (Value) and the transformed parameter variance (Var).
#' @export
#'
#' @examples
#' \dontrun{
#' deltaRule(ebes,varCov,computeNadir)
#' }
deltaRule<- function(ebes,varCov,transform_fun,bIncludeCov=TRUE,...) {
  
  ## Compute the transformation
  th_new   <- transform_fun(ebes,...) 
  
  ## Compute the numerical derivatives
  th_deriv <-grad(transform_fun,ebes, ...) 
  
  ## Initialize the variance of the transformed function
  new_var  <- 0 
  
  for (i in 1:length(ebes))
    for (j in 1:length(ebes)) {
      if ((!bIncludeCov && i==j) || bIncludeCov) new_var<-new_var+th_deriv[i]*th_deriv[j]*varCov[i,j]      
    }
  
  return (data.frame(Value=th_new,Var=new_var)) # 
}
