#' calcHAZVector
#'
#' @description Calculates HAZ values based on the inverse Bateman function.
#' @param age A vector of ages to compute the HAZ values for, default is age=0.
#' @param basethetas A vector with base tehtas (fixed effects).
#' @param covthetas A vector with the ffem, parameter specific covariate effetcs.
#' @param etas A vecor of individual specific random effects
#'
#' @return  A vector of HAZ values.
#' @export
#'
#' @examples
#' #' \dontrun{
#' calcHAZVector(c(1,2),basethetas = thetas[1:6],covthetas=thetas[7:12],etas=rep(0,6))
#' }
calcHAZVector <- calcHAZ <- function(age=0,basethetas=NULL,covthetas=rep(0,length(basethetas)),etas=rep(0,length(basethetas))) {

  BASE    <- basethetas[1]          + covthetas[1] + etas[1]
  PLMAX   <- basethetas[2]          + covthetas[2] + etas[2]
  HLKON   <- exp(log(basethetas[3]) + covthetas[3] + etas[3])
  HLKOFF  <- exp(log(basethetas[4]) + covthetas[4] + etas[4])
  BASSL   <- basethetas[5]          + covthetas[5] + etas[5]
  BP      <- exp(log(basethetas[6])      + covthetas[6] + etas[6])
  
  KON       <- log(2)/HLKON
  KOFF      <- log(2)/HLKOFF
  
  BASFAC1   <- BASSL*age
  BASFAC2   <- BASSL*BP
  
  PHI      <- age**2.25/(age**2.25 + BP**2.25)
  MYBASE   <- BASE + (1-PHI)*BASFAC1 + PHI*BASFAC2
  IPRED    <-  MYBASE-PLMAX*KON*(exp(-KOFF*age)-exp(-KON*age))/(KON-KOFF)
  
  return(IPRED)
}
  

