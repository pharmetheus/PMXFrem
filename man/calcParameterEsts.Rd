% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcParameterEsts.R
\name{calcParameterEsts}
\alias{calcParameterEsts}
\title{Calculate parameter estimates from FREM output to be used in a parameter
table}
\usage{
calcParameterEsts(
  parVector,
  thetaNum,
  omegaNum,
  sigmaNum,
  numNonFREMThetas,
  numSkipOm,
  ...
)
}
\arguments{
\item{parVector}{A vector of values corresponding to the line with final
parameter estimates from a NONMEM ext file.}

\item{thetaNum}{The theta numbers to include in the output. This refers to
the number associated with the theta value in \code{parVector}. `}

\item{omegaNum}{The omega numbers to include in the output. This refers to
the number associated with the omega value in \code{parVector}. `}

\item{sigmaNum}{The sigma numbers to include in the output. This refers to
the number associated with the omega value in \code{parVector}. `}

\item{numNonFREMThetas}{Number of thetas in the FREM model that are not FREM
covariates. These need to come before the FREM covariate thetas in the FREM
model file.}

\item{numSkipOm}{Number of diagonal omegas that should not be part of the
FREM calculations. Such omegas has to come before the large FREM omega
block in the FREM model file.}
}
\value{
A vector of parameter estimates corresponding to \code{thetaNum},
\code{omegaNum}and \code{sigmaNum}.
}
\description{
This is an internal utility function that facilitates a standardized way of
computing FFEM parameter estimates from FREM results.
}
\examples{
\dontrun{
 fremParEsts <- calcParameterEsts(extRes,thetaNum,omegaNum,sigmaNum,numNonFREMThetas,numSkipOm,
covNames=covNames, availCov=availCov,quiet=quiet)
}
}
\seealso{
\code{\link[=fremParameterTable]{fremParameterTable()}}
}
\keyword{internal}
