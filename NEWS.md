# PMXFrem (development version)

* Updated documentation of `getExplainedVar()` to make i clear that `type=0` assumes that each function in `functionList` returns a scalar value and not a vector or list.
* Added information in the documentation to `updateFREM()` about the assumption that binary covariates are coded as 0 and 1.

# PMXFrem 1.2.6

* Made it possible to use availCov to specify the covariates to be used for the derivation of TOTCOVVAR.
* Changed the default in plotExplainedVar so tha the main effects ordering is based on the mean instead of the median.
* Added the argument reordFun to specify the reordering function for the main effects ordering.
    
# PMXFrem 1.2.5

* Added traceplot() and fixed a bug in createFFEMdata().

# PMXFrem 1.2.4

* Fixed the situation when you have more etas than base thetas, which may occur if you have skipped omegas for etas not associated with thetas.

# PMXFrem 1.2.3

* Made it possible to omit sigma in fremParameterTable by setting sigmaNum and sigmaLabel to NULL.
* Made it possible to not have ffem OMEGAS in fremParameterTable

# PMXFrem 1.2.2

Removed the import of an non-public package.

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
