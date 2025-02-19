% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcFFEM.R
\name{calcFFEM}
\alias{calcFFEM}
\title{Calculate the corresponding FFEM charactersitics from a FREM model-}
\usage{
calcFFEM(
  dfext,
  numNonFREMThetas,
  numSkipOm = 0,
  numFREMThetas = length(grep("THETA", names(dfext))) - numNonFREMThetas,
  numSigmas = length(grep("SIGMA", names(dfext))),
  numParCov = NULL,
  parNames = paste("Par", 1:numParCov, sep = ""),
  covNames = paste("Cov", 1:numFREMThetas, sep = ""),
  availCov = covNames,
  quiet = FALSE,
  fremETA = NULL,
  eqFile = "",
  omFile = "",
  ...
)
}
\arguments{
\item{dfext}{A data frame of the ext file for the FREM model. In case of
multiple $EST, the dfext should contain the estimates of the \emph{one} relevant
$EST.}

\item{numNonFREMThetas}{Number of thetas in the FREM model that are not FREM
covariates. These need to come before the FREM covariate thetas in the FREM
model file.}

\item{numSkipOm}{Number of diagonal omegas that should not be part of the
FREM calculations. Such omegas has to come before the large FREM omega
block in the FREM model file.}

\item{numFREMThetas}{The number thetas associated with FREM covariate
effects.}

\item{numSigmas}{Number of sigma parameters in the ext file model from the
FREM model (including zero sigma covariance elements).}

\item{numParCov}{Number of parameters for which covariate relations are
sought (often the same as numNonFREMThetas). default (NULL) indicates that
this is guessed by the function by assuming that each covariate effect is
modeled as one theta and one eta.}

\item{parNames}{Names of the parameters. Only used in the STDOUT when \code{quiet =FALSE} or in \code{eqFile} if it is non-blank. Defaults to \code{Par} folowed by a
number.}

\item{covNames}{Names of the covariates in the FREM model. Defaults to \code{Cov}
followed by a number.}

\item{availCov}{Names of the covariates to use in the calculation of the FFEM
model. Should be covariate names used in the FREM model.}

\item{quiet}{If FALSE, will print the FFEM model + associated $OMEGA BLOCK to
STDOUT.}

\item{eqFile}{File name to save the FFEM equations in.}

\item{omFile}{File name to save the omega prim matrix in.}

\item{fremEta}{NULL or a vector of individual ETAs from the FREM model.}
}
\value{
A list with components Coefficients, Vars and Expr:

Coeffients: The numNonFREMThetas x availCov matrix with FFEM model
coefficients

Vars:       The numNonFREMThetas x numNonFREMThetas matrix with the FFEM
variances (OMEGA)

Expr:       The vector of FFEM expressions, one for each base model
parameter.

Fullvars:   The full FFEM variance-covariance matrix of all OMEGAs in the
model.

Uppervars:  The variance-covariance matrix of the skipped OMEGAs in the
model.

Eta_prim:  The matrix of individual etas re-scaled to FFEM etas.
}
\description{
Computes the corresponding FFEM from a FREM model. Can handle missing
covariates.
}
\details{
\verb{calcFFEM()`` computes the corresponding FFEM characteristics for a covariate model consisting of the specified covariates (}availCov\verb{). The covariate names specified by }availCov\verb{should be among the names listed in}covNames`.

The computed characteristics are the covariate coefficients, the FFEM
expressions, the omega prims and the full omega matrix to be used in the FFEM
model. Optionally, it also computes the corresponding eta_prims for a subject
with the specified set of covariates.

If \code{fremETA} is non-\code{NULL}, ETA prims will be calculated. In this case,
\code{fremETA} should be a numeric vector of individual ETAs from the FREM model.
Only one set of ETA prims will be computed (for one individual). To compute
the ETA prims for all subjects at the same ime, please use \code{\link[=calcEtas]{calcEtas()}}.
}
\examples{

extFile         <- system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
dfExt           <- getExt(extFile = extFile)
calcFFEMtestout <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = T)

## Use fremETA to also compute the ETA prim
phiFile         <- system.file("extdata/SimNeb/run31.phi", package = "PMXFrem")
dfPhi           <- getPhi(phiPhile) \%>\% select(starts_with("ETA"))
calcFFEMtestout <- calcFFEM(dfExt, numNonFREMThetas = 7, numSkipOm = 2, quiet = T,
  etaFREM = as.numeric(dfPhi[1, ]))

}
