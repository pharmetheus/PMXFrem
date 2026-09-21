# PMXFrem 2.2.0

What is new since 2.0.0. 2.1.0 was published but never promoted, so its changes
are described here together with 2.2.0's.

## Parameter functions written from the model

`createFREMParamFunction()` writes the parameter function `getForestDFFREM()`
and `getExplainedVar()` need, by transliterating the model's own `$PK` - what
`PMXForest::createParamFunction()` does for non-FREM models. The result is
source text to read and edit; nothing is evaluated.

One generated function serves both plots: `etas = 0` for a Forest plot, sampled
etas for explained variability. `secondary` appends AUC, Cmax and the like.

`verifyFREMParamFunction()` is the check to run before trusting it. It compares
the function against the FFEM version of the same model - an independent
transliteration - and confirms each parameter's covariate coefficient and eta
land where the model says.

## Extending an established FREM model

`addFremStructuralTheta()` adds a structural `$THETA`, and with `addEta = TRUE`
a matching IIV. `addFremIIV()` adds the random effect on its own.

Both renumber every `THETA()`, `ETA()`, `MU_` and `COV` reference that follows
- including those inside the `;;;FREM CODE` block - and place the new `$OMEGA`
inside the skip region rather than after the FREM block. Both refuse rather
than write a model that is quietly wrong. The `.ext` and `.phi` are not
migrated: the model has to be re-estimated.

## Individual variances for each covariate pattern

`omegaToData = TRUE` in `createFFEMdata()` / `createFFEMmodel()` writes each
subject's variance-covariance elements into the data set as `V`-columns and
sets `$OMEGA` to an identity matrix. Each subject's variance then follows their
own missing-covariate pattern, instead of one matrix serving everyone.

## Other notable additions

* **The FREM structural integers are read from the model.** `fremModelInfo()`
  works them out from a model and its `.ext`, so `numNonFREMThetas` and
  `numSkipOm` no longer have to be passed to `fremParameterTable()`,
  `createFFEMmodel()`, `createFFEMdata()`, `calcEtas()` or `updateFREMmodel()`.
  A supplied value that disagrees warns and is kept.
* `fixTheta` (default `TRUE`) writes `FIX` on the covariate thetas that are
  fully observed, which makes the EM estimation more stable.
* `createFREMmodel()` warns about `$EST` settings FREM struggles with: `SAEM`
  with missing covariates, no `IMP` / `IMPMAP`, `NITER` below 150, `PHITYPE=0`.
* `missVal` replaces the hardcoded `-99` through the data assembly functions.
* `oneHot` / `oneHotSep` in `getForestDFFREM()` encode raw categorical columns
  to the `<cov>_<level>` form the model uses.
* `add.stamp` on `plotExplainedVar()`, `traceplot()`, `plotEtasCov()` and
  `plotCovDist()` records where and when a figure was made.

## Changes to existing behaviour

* **`setupDfCovsEV()`'s `additionalCovs` is renamed `conditionalCovs`, and they
  are on in every row.** Each FREM covariate's row now reports what it explains
  with them in the model, which includes their own contribution.
  `additionalCovs` still works, with a deprecation warning.
* **`calcFFEM()` and `plotExplainedVar()` no longer take `...`.** In
  `calcFFEM()` it silently swallowed mistyped argument names.
  `plotExplainedVar()`'s stamp goes through `PMXForest::addStamp()`, and
  `add.stamp` is no longer read from the global environment.
* **PMXForest (>= 1.3.0)** is the new floor.

## Bug fixes

* **`getExplainedVar()` returned wrong numbers, silently, in five cases:** a
  missing covariate blanked others whose names contain it (`WT` removed
  `LBWT`); type 1 paired phi etas with subjects by row position rather than by
  ID; type 3 passed the sample index into the parameter function;
  `availCov = "RACEL"` was dropped instead of expanded; a function returning
  nothing for a covariate row shifted later values onto the wrong parameter.
* **`getExplainedVar()` failed on documented inputs:** arguments in `...`, a
  bare function as `functionList`, `parNames` without `numParCov`, a type 0
  `dfCovs` row of only non-FREM covariates.
* **A single-covariate `dfCovs`** reported no explained variability at all, and
  two internal calls passed a too-short eta vector.
* **An underscore in a covariate's own name** (`BL_BILI`) was read as a
  binarized level, leaving the covariate out.

## Under the hood

* `addFREMcovariates()` binarises through `PMXForest::oneHotEncode()`, so the
  `<cov>_<level>` convention has one implementation across the two packages.
* `getForestDFFREM()` builds its result once instead of growing it in a loop.
* Smaller: `calcFFEM()` label checks, `buildmatrix(forceSingleBlock)`, `.ext`
  parsing on R before 4.0, explicit `foreach` arguments, and an argument-name
  audit that found two tests asserting nothing.

## Two numbers you have seen before change

Both are in `fremParameterTable()`, and both are worth knowing before
regenerating a table you have already shown.

* **RSE and CI are reproducible.** `seed` defaults to `1` rather than `NULL`,
  so the sampled uncertainty no longer depends on the random number state the
  caller happens to arrive with - two calls in one session could differ by 20%.
  Your values change once, then stop moving. `seed = NULL` restores the old
  behaviour.
* **A negative shrinkage is reported as one.** It read `0.00` in both
  documented modes; it now reads `1.0000e-10` by default, as NONMEM does, or
  its raw value with `rawShrinkage = TRUE`. On the bundled `run31max1-2`,
  ETA3's variance shrinkage is -26.44%.

# PMXFrem 2.0.0

## `createFREMmodel()` - the FREM model without PsN

Builds the FREM model and its data set from a base model and a data file, so an
analysis no longer has to start by running PsN's `frem` command.
`keepDoseOnlySubjects` (default `FALSE`) keeps subjects that have no PK
observations.

## `fremParameterTable()` reworked

Base parameters and covariate coefficients are drawn from the same sample
space, so the two halves of a table agree. `uncertainty` reports `"RSE"` or
`"CI"`; `includeShrinkage` adds shrinkage, with `shrinkageType`, `shkDigs` and
`rawShrinkage` deciding what is shown.

## Covariate coefficient tables

`coefficientTable_long` for programmatic use and `coefficientTable_wide` for
reports, both returned by `fremParameterTable()`.

## `plotEtasCov()` and `plotCovDist()`

`plotEtasCov()` plots FREM ETAs, ETA_PRIMs and FFEM EBEs against the covariates
in facets; `plotCovDist()` plots the distributions of the estimated covariates.
Both group the data by whether the covariate was observed.

## `generateCovNames()`

Forest plot labels built from the covariate table, rather than typed out and
kept in step by hand.

## Vignettes

A quick start and a walk-through, plus deep dives for `createFREMmodel()`,
diagnostics, Forest plots, explained variability and `updateFREMmodel()`.

## Other notable additions

* The internal pipeline functions (`augmentFremData`, `prepareAndValidateData`
  and others) moved out of the user-facing namespace.
* `setypdfCovs()` is renamed `setupDfCovsEV()`.

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
* **Longitudinal data** gave an eta-length mismatch, and a missing categorical
  covariate passed silently rather than stopping.
* Smaller: lazy-evaluation defaults reading `dfext`, `data.table` NSE scoping,
  a dummy-column lookup taking the wrong element.

## Under the hood

* `getExplainedVar()` is split into a helper per type, its growing `rbind`
  loops replaced by pre-allocated lists and its string rewriting by evaluation
  in a scoped environment.
* Examples use base R and `tempdir()`; `@family` / `@concept` tags organise the
  pkgdown reference.

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
