# PMXFrem 1.2.2

# PMXFrem 1.2.1

Fixed bug in addFremCovariates.

# PMXFrem 1.2.0

This release prepares PMXFrem for public release. Some feature additions. bug
fixes and much improved documentation (not the least that a majority of the
examples in the help files are now executable).

## New or revised functionality
* Added setupdfCovs() fuction that facilitates the creation of the dfCovs for getExplainedVar().
* Moved getForestDFREM() from PMXForest to PMXFrem.
* Added a parameter table function (fremParTable())
* Made parNames mandatory for createFFEMmodel()
* Some harmonization of arguments across functions.
* Added a ':' between the parameter name and the FFEM expression in the output from calcFFEM.

## Bug fixes and error handling and prevention
* Added an input check to getExplainedVar() so that dataI and etas must have the
same number of rows if type=1
* Fixed the potential issue with misalignment between panels and facet labels in 
plotExplainedVar().
* Updated unit tests for many functions.

## Documentation
* Extensive updates to the help file documentation.
* Made many examples in the help page documentation directly executable.
* Updated some of the vignettes.
* Linting and other cleaning of source code
* Cleaned up repository directory
* Various other editorial changes.
