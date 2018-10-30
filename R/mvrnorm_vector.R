#' mvrnorm_vector
#'
#' @description Samples from a NONMEM .cov matrix
#' @param mu Mean vector
#' @param sigma Covariace matrix
#' @param fixed_mu To be described
#' @param dSeed The seed number
#' @param iSampleIndex To be described
#' @return A vector.
#' @export
#'
#' @examples
#' \dontrun{
#' ## To extract the final parameter estimates
#' dfext    <- subset(getExt(extFile = "myExtFile.ext"),ITERATION=="-1000000000")
#' }                                     
mvrnorm_vector <- function(mu,sigma,fixed_mu=NULL,dSeed=NULL,iSampleIndex=1)
 {
  ####################################################################################
  ##
  ## Generate a vector of multivariate samples,
  ## mean vector mu
  ## covariance matrix sigma
  ## a vector where 1 indicates that a mu is fixed (note that size of sigma is the size
  ## of the number of unfixed parameters
  #  iSampleIndex = 0 (return mu), 1=return first sample vector,
  ## n = return n-sample vector
  ## dSeed, the seed to use while sampling, NULL=no seed is set
  ##
  ## by Joakim Nyberg 2014
  ####################################################################################
     require(MASS) #For mvrnorm
   if (iSampleIndex==0) return (mu)
   if (is.null(fixed_mu)) fixed_mu<-rep(0,length(mu))
   tmp_mu<-mu[which(fixed_mu==0)] #Get the non-fixed mu
   if (!is.null(dSeed)) set.seed(dSeed)
   samples<-mvrnorm(n=iSampleIndex,tmp_mu,sigma)
   if (iSampleIndex==1) mu[which(fixed_mu==0)]<-samples
   if (iSampleIndex!=1) mu[which(fixed_mu==0)]<-samples[iSampleIndex,]
   return (mu)
 }
