% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createFFEMmodel.R
\name{createFFEMmodel}
\alias{createFFEMmodel}
\title{Create an FFEM version of a FREM model together with an appropriate data file.}
\usage{
createFFEMmodel(
  runno = NULL,
  numNonFREMThetas,
  modName = NULL,
  modExt = ".mod",
  lstExt = ".lst",
  numFREMThetas = length(grep("THETA", names(dfext))) - numNonFREMThetas,
  covSuffix = "FREMCOV",
  parNames = NULL,
  numParCov = NULL,
  numSkipOm = 0,
  dataFile,
  newDataFile = paste("vpcData", runno, ".csv", sep = ""),
  availCov = "all",
  idvar = "ID",
  modDevDir = NULL,
  quiet = FALSE,
  cores = 1,
  dfext = NULL,
  baserunno = NULL,
  baseModName = NULL,
  baseModDevDir = modDevDir,
  ffemTabName = "ffemtab",
  ffemModName = NULL,
  ...
)
}
\arguments{
\item{runno}{The run number. Must be provided if \code{modName} is NULL. Will be
used to create NONMEM model and results file names  with the structure
run#.mod and run#. NONMEM ext, phi and cov files will be assumed to be
named run# followed by the appropriate suffix (where # is \code{runno}).}

\item{numNonFREMThetas}{Number of thetas in the FREM model that are not FREM
covariates. These need to come before the FREM covariate thetas in the FREM
model file.}

\item{modName}{The model name. Will be used together with \code{modExt}, \code{lstExt},
ext, phi and cov to form the NONMEM file names, for example
\code{modName}.mod for the model file.}

\item{modExt}{The model file name extension. Default is ".mod".}

\item{lstExt}{The NONMEM out file name extension. Default is ".lst".}

\item{numFREMThetas}{The number thetas associated with FREM covariate
effects.}

\item{covSuffix}{Use to create the column name fo rthe covariate effects
columns. The column names will be the parameter name followed by this
string.}

\item{parNames}{Names of the parameters. Only used in the STDOUT when \code{quiet =FALSE} or in \code{eqFile} if it is non-blank. Defaults to \code{Par} folowed by a
number.}

\item{numParCov}{Number of parameters for which covariate relations are
sought (often the same as numNonFREMThetas). default (NULL) indicates that
this is guessed by the function by assuming that each covariate effect is
modeled as one theta and one eta.}

\item{numSkipOm}{Number of diagonal omegas that should not be part of the
FREM calculations. Such omegas has to come before the large FREM omega
block in the FREM model file.}

\item{dataFile}{The name of the data file used in the base model, i.e. the
original data file, or a data.frame with the same data.}

\item{newDataFile}{The name of a new data file with the FFEM columns added.
Default is vpcData{runno}.csv. If NULL, will return a data frame with the
data instead of writing it to disk.}

\item{availCov}{Names of the covariates to use in the calculation of the FFEM
model. Should be covariate names used in the FREM model.}

\item{idvar}{The name of the ID column,}

\item{modDevDir}{The directory where the files reside. Default is the current
directory, i.e. ".".}

\item{quiet}{If FALSE, will print the FFEM model + associated $OMEGA BLOCK to
STDOUT.}

\item{cores}{How many cores to use in the calculations of the FFEM
expressions.}

\item{dfext}{A data frame of the ext file for the FREM model. In case of
multiple $EST, the dfext should contain the estimates of the \emph{one} relevant
$EST.}

\item{baserunno}{The run number of the base model.}

\item{baseModName}{The name of the base model. If NULL (default) no model file
will be printed to file.}

\item{baseModDevDir}{The directory name in which the base model files can be
found. Default the same as modDevDir.}

\item{ffemTabName}{The table file name to insert into the FFEM model. Default
is 'ffemtab'.}

\item{ffemModName}{The name of the file to write the ffem model to, or NULL.
NULL is the default}

\item{...}{Arguments passed to \code{createFFEMdata}}
}
\value{
A character vector with the code for the FFEM model.
}
\description{
Create FFEM versions of a FREM model and data set to be used for generating
GOF plots and VPC analyses.
}
\details{
To create GOF plots and VPC analysies for a FREM model it is necessary to
convert it to a FFEM model, with the appropriate covariate effects and omega
prim.

There are two ways the FREM covariate effects can be included in the FFEM
model. The first is to amend the FFEM code with the full covariate model
expressions (these expressions can be obtained from the \code{\link[=createFFEMdata]{createFFEMdata()}}
function. This may be practically possible for one or a few covariates, but
will quickly run into NONMEM related issues regarding the number of allowed
constants, and similar. The second and recommended approach, is to precompute
the total impact of the covariates for each individual, and to add these
\emph{individual covariate coefficients} to the original data set. They can then be
added as constants in the the FFEM code additatively to the corresponding ETA:

Change the FFEM code from:

\code{PAR = TVPAR * EXP(ETA)}

to

\code{PAR = TVPAR * EXP(ETA + PARCOV)}

where PARCOV is the individual covariate coefficient. (The PARCOV term

The addition of the individual covariate coefficients to the original data set
is done by´ the \code{\link[=createFFEMdata]{createFFEMdata()}} function. However, \code{createFFEMdata()} is
called by \code{createFFEMmodel()} so there is no need to do this as a separate
step.

The are also other changes to the original model file to be suitable for
generating GOF diagnostics for the FREM model:
\itemize{
\item Replace the data file name on $DATA with name of the file with the individual covariate coefficients. (Note! It is assumed that the data files name follows directly after $DATA. If there are other commands on the same line as $DATA and the data file name, they should follow after the data file name.)
\item Append column names to $INPUT to match the new data file.
\item Update the initial estimates for $THETA and $SIGMA with the corresponding estimates from the FREM run.
\item Replace the initial estimates of the OMEGAs with the adjusted OMEGA ("Omega prim") from the FREM model.
\item Add the individual covariate coefficients to the corresponding ETAs (see above).
\item Replace the options on $EST with MAX=1 METHOD=1 INTER. If more that one $EST is present in the file, the second, third, etc are removed.
\item Update any table file name with \code{ffemTabName}
}

\code{createFFEMmodel()} implements these changes to the model automatically.

The individual covariate coefficients will take missing covariates into
account while the omega prim will assume that all covariates are non-missing.

The resulting model is not intended for estimation of the population
parameters, only for EBE estimation and generation of predictions in a
MAXEVAL=0 run. The the tabulated output is suitable for the generation of
regular GOF plots. The model file can easily be modified to perform
simulations for a VPC analysis.
}
\section{Side effects}{
 The FFEM model will be written to a file with the name
\code{ffemModName} if it is non-\code{NULL}.
}

\examples{
\dontrun{
ffemMod <- createFFEMmodel(runno            = 31,
                           modDevDir        = modDevDir,
                           numNonFREMThetas = 7,
                           numSkipOm        = 2,
                           parNames         = c("CL","V","MAT"),
                           dataFile         = "DAT-2-MI-PMX-2-onlyTYPE2-new.csv",
                           newDataFile      = "testFileName.csv",
                           quiet            = FALSE,
                           baserunno        = 30)
}
}
\seealso{
\code{\link[=getFileName]{getFileName()}} \code{\link[=createFFEMdata]{createFFEMdata()}} \code{\link[=calcFFEM]{calcFFEM()}}]
}
