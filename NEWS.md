# PMXFrem 2.2.0

PMXFrem 2.1.0 was published but never promoted, so its changes are described
here together with 2.2.0's.

## Automatically generate the parameter function from the NONMEM code

`createFREMParamFunction()` writes the parameter function `getForestDFFREM()`
and `getExplainedVar()` need, by translating the FREM model's `$PK` block,
similar to what `PMXForest::createParamFunction()` does for non-FREM models.

One generated function serves both Forest plots and explained variability
plots: `etas = 0` for a Forest plot, sampled etas for explained variability.

`verifyFREMParamFunction()` is the check to run before trusting it. It compares
the function against the FFEM version of the same model.

## Extending an established FREM model

`addFremStructuralTheta()` adds a structural `$THETA`, and with `addEta = TRUE`
a matching IIV. `addFremIIV()` adds the random effect on its own.

The THETAs and ETAs are added before the FREM part of the model and renumber
every `THETA()`, `ETA()`, `MU_` and `COV` reference that follows.

## Individual variances for each missing covariate pattern

`omegaToData = TRUE` in `createFFEMdata()` / `createFFEMmodel()` use Cholesky
decomposition to write each subject's variance-covariance elements into the
data set and sets `$OMEGA` to an identity matrix. Each subject's variance then
follows their own missing-covariate pattern, instead assuming that there are no
missing covariates.

## Other notable additions

* **`numNonFREMThetas` and `numSkipOm` no longer have to be specified by the
  user.** `fremModelInfo()` works them out from a model and its `.ext`, so they
  no longer have to be passed to `fremParameterTable()`, `createFFEMmodel()`,
  `createFFEMdata()`, `calcEtas()` or `updateFREMmodel()`.
* `createFREMmodel()` warns about `$EST` settings FREM struggles with: `SAEM`
  with missing covariates, no `IMP` / `IMPMAP`, `NITER` below 150, `PHITYPE=0`.
* `missVal` replaces the hardcoded `-99` through the data assembly functions.
* `getForestDFFREM()` one-hot encodes categorical covariates if requested
  (`oneHot` / `oneHotSep`).
* `add.stamp` on `plotExplainedVar()`, `traceplot()`, `plotEtasCov()` and
  `plotCovDist()` records where and when a figure was made.

## Changes to existing behaviour

* **`setupDfCovsEV()`'s `additionalCovs` is renamed `conditionalCovs`, and they
  are on in every row.** Each FREM covariate's row now reports what it explains
  with them in the model, which includes their own contribution.
  `additionalCovs` still works, with a deprecation warning.
* **PMXFrem now requires PMXForest v1.3.0 or later.**

## Bug fixes

A careful code review surfaced a number of edge cases in which
`getExplainedVar()` did not behave as intended. The same review found the
single-covariate and underscore problems below.

* **`getExplainedVar()` returned wrong numbers, silently, in five cases:** a
  missing covariate blanked others whose names contain it (`WT` removed
  `LBWT`); type 1 paired phi etas with subjects by row position rather than by
  ID; type 3 passed the sample index into the parameter function;
  `availCov = "RACEL"` was dropped instead of expanded; a function returning
  nothing for a covariate row shifted later values onto the wrong parameter.
* **`getExplainedVar()` rejected calls it should have accepted:** extra
  arguments meant for the parameter function, a single function given instead
  of a list of functions, parameter names given without their number, and a
  `dfCovs` row holding only non-FREM covariates.
* **A single-covariate `dfCovs`** reported no explained variability at all, and
  two internal calls passed a too-short eta vector.
* **An underscore in a covariate's own name** (`BL_BILI`) was read as a
  binarized level, leaving the covariate out.

## Under the hood

* `addFREMcovariates()` binarises through `PMXForest::oneHotEncode()`, so the
  `<cov>_<level>` convention has one implementation across the two packages.
* `getForestDFFREM()` builds its result once instead of growing it in a loop,
  which shortens run times.

## Two numbers you have seen before change

* **RSE and CI are reproducible.** `seed` defaults to `1` rather than `NULL`,
  so the sampled uncertainty no longer depends on the random number state the
  caller happens to arrive with.
* **A negative shrinkage is reported as in NONMEM.** By default a negative
  shrinkage is reported as `1.0000e-10`. Setting `rawShrinkage = TRUE` will
  report the shrinkage as it is calculated, even if negative.

# PMXFrem 2.0.0

## Build a FREM model without PsN

`createFREMmodel()` builds the FREM model and its data set from a base model and
a data file, so an analysis no longer has to start by running PsN's `frem`
command.
`keepDoseOnlySubjects` (default `FALSE`) retains subjects that have no PK
observations.

## Parameter tables with uncertainty and shrinkage

`fremParameterTable()` draws base parameters and covariate coefficients from the
same sample space. `uncertainty` reports `"RSE"` or `"CI"`; `includeShrinkage`
adds shrinkage, with `shrinkageType`, `shkDigs` and `rawShrinkage` deciding what
is shown.

## Covariate coefficient tables

`fremParameterTable()` also returns `coefficientTable_long` for programmatic use
and `coefficientTable_wide` for reports.

## Diagnostic plots for ETAs and covariates

`plotEtasCov()` plots FREM ETAs, ETA_PRIMs and FFEM EBEs against the covariates
in facets; `plotCovDist()` plots the distributions of the estimated covariates.
Both group the data by whether the covariate was observed.

## Forest plot labels built from the covariate table

`generateCovNames()` builds them from the covariate table, rather than having
them typed out and kept in step by hand.

## Vignettes

A quick start and a walk-through, plus deep dives for `createFREMmodel()`,
diagnostics, Forest plots, explained variability and `updateFREMmodel()`.

## Changes to existing behaviour

* **Output data sets are sorted stably**, by `ORIG_ROW_IDX` and `FREMTYPE`, so
  the record order within a subject survives. `updateFREMmodel()`'s
  `cstrSortCols` was deprecated with it.
* **A dichotomous covariate coded `1`/`2` is an error**, since FREM expects
  `0`/`1`. `allowNon01 = TRUE` or `bRecodeDichotomous = TRUE` restores the PsN
  behaviour.

## Bug fixes

* **Covariates with overlapping names overwrote each other** - `AGE` blanked
  `PAGE`, dropping valid data and pushing `TOTCOVVAR` below `COVVAR`.
* **`getExplainedVar()` failed on Windows** with `ncores > 1`, and left its
  cluster running if the calculation errored.
* **`fremParameterTable()` transformed to the SD scale after computing the CI /
  RSE**, so the uncertainties did not match the scale they were reported on.

# PMXFrem 1.2.12

## Bug Fixes
* **`getExplainedVar()`**: Fixed a logical error in dataset subsetting (`!duplicated()`) where longitudinal datasets bypassed the ETA mismatch check and silently calculated incorrect variabilities.
* **`getExplainedVar()`**: Added `drop = FALSE` to prevent 1D vector collapse when evaluating models that contain exactly one covariate.
* **`getExplainedVar()`**: Upgraded a silent warning to a strict `stop()` when a categorical model covariate is missing from the provided dataset, preventing downstream `logical(0)` evaluation crashes.

# PMXFrem 1.2.11

* Fixed a unit test failure.

# PMXFrem 1.2.10

Defined the license to be GPL (>= 3)


# PMXFrem 1.2.9

* **`calcEtas()`**: Refactored for improved usability. The function can now optionally create the `FFEMdata` object internally when supplied with `dataFile`, `parNames`, etc., removing the need for a separate `createFFEMdata()` call. Also corrected internal argument handling to prevent `unused argument` errors when `...` is used (e.g., for `availCov`).
* **`traceplot()`**: Added a new feature to display a shaded chi-squared acceptance region on the OFV plot to help visualize convergence stability. This is controlled by the new arguments `includeShapedOFV`, `pvalue`, `df`, and `meanShapeLastIter`. Also fixed a bug where validation for these new arguments incorrectly used `error()` instead of the correct `stop()` function.

# PMXFrem 1.2.8

## BUG FIXES & QUALITY IMPROVEMENTS

* Greatly improved test coverage across the package, with most core functions now exceeding 90% coverage.
* Fixed numerous bugs and edge cases in `updateFREMmodel()`, `createFREMData()`, and `calcFFEM()` that were discovered through the expanded test suite.
* Refactored tests to run in a self-contained manner, eliminating side effects such as writing files to the project directory.
* Resolved test inconsistencies across different R versions by updating and stabilizing test snapshots.
* devtools::check() pass without errors.

# PMXFrem 1.2.7

* Made it possible to use type=0 in `getExplainedVar()` together with a function that returns multiple values.

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
