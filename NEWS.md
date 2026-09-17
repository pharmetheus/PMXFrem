# PMXFrem 2.1.0.9000

## New features

* **`createFREMParamFunction()` / `verifyFREMParamFunction()` — a parameter
  function generated from the FREM model itself.** `getForestDFFREM()` and
  `getExplainedVar()` both take a `functionList` of parameter functions, and
  writing one by hand means transcribing `$PK` and getting every `THETA()`,
  `ETA()` and covariate index right. `createFREMParamFunction(fremModel,
  parameters)` writes it for you, as **source text to read and edit** — nothing
  is evaluated.

  It transliterates the model's structural `$PK`, keeping non-FREM covariates
  such as allometric weight, and pruned to the statements the requested
  parameters actually depend on. For a FREM covariate parameter — one carrying
  a single `ETA()` after the skipped omegas — that `ETA()` reference is replaced
  in place, whatever encloses it, by `covthetas[k] + etas[i]`: `ETA(i)` is the
  reference `$PK` makes, and `k = i - numSkipOm` is the parameter's FREM index
  in the model. Both are the model's own numbering, so the same `covthetas` and
  `etas` vectors serve a request for two parameters and a request for all of
  them, and asking for a subset gives exactly the lines the full request gives.
  A parameter whose only `ETA()` is inside the skipped omegas keeps that eta
  and takes no covariate effect; every other `ETA()` goes to 0. One generated function serves both plots: call
  it with `etas = 0` for a forest plot and with non-zero `etas` for an
  explained-variability plot.

  A `secondary` argument appends derived quantities (AUC, Cmax, ...) to the
  return list, the same way `PMXForest::createParamFunction()` does: each entry
  is a line of R code, a path to an `.R` file of arbitrary code (an `mrgsolve`
  simulation, say), or `list(source = <string>, dose = 100, tau = 12, ...)`
  carrying that source plus constants bound ahead of it. The code is inlined
  verbatim inside `local()` and sees `basethetas`, `covthetas`, `dfrow` (also
  as `df`), `etas`, the call-site constants and every structural parameter by
  name.

  `verifyFREMParamFunction(x, ffemModel = <the FFEM model>)` is the check to
  run before trusting the result. It compares the structural part against the
  parameter function `PMXForest::createParamFunction()` writes from the FFEM
  version of the same model — a genuinely independent transliteration — and
  confirms that `covthetas[k]` and `etas[i]` scale parameter `k` by `exp(.)`
  and nothing else. Its probe indices come from PMXForest's parse of the
  reference, not from the object under test, and it derives `numSkipOm` from
  the reference model's own `$OMEGA` records: a check that shares the
  generator's assumptions cannot test them. It returns a single `TRUE` /
  `FALSE` usable in an `if`, with the per-parameter table attached as
  `attr(., "checks")`.

* **`addFremStructuralTheta()` / `addFremIIV()` — extend an established FREM
  model in place.** Adding a structural `$THETA` or an IIV to a FREM model by
  hand means renumbering every `THETA()`, `ETA()`, `MU_` and `COV` reference
  that follows it, including the ones inside the `;;;FREM CODE` block, and
  getting the new `$OMEGA` record into the skip region rather than after it.
  These do that.

  `addFremStructuralTheta(strFREMModel, thetaInit, ...)` inserts one structural
  `$THETA` at `numNonFREMThetas + 1`, immediately before the FREM covariate-mean
  thetas, and shifts the `MU_k = THETA(...)` block up by one. With
  `addEta = TRUE` it adds a matching IIV. A parameter that has no `$PK`
  assignment gets a definition inserted before the FREM `MU_` block, delimited
  by `;; Begin added THETA` / `;; End added THETA`; one that already has an
  assignment is modified in place. For a new parameter, `muReference = TRUE`
  (the default) writes the house form `TV<par> = THETA(j)`,
  `MU_k = LOG(TV<par>)`, `<par> = EXP(MU_k + ETA(k))` — deliberately not a
  direct `MU_k = THETA(j)`, which would match the pattern `generateFremModel()`
  uses to find the FREM block and would be spliced away by a later
  `updateFREMmodel()`.

  `addFremIIV(strFREMModel, parameter, omegaInit, ...)` is the primitive, also
  usable on its own: it takes eta index `numSkipOm + 1`, shifts every reference
  at or after it across all of the model's abbreviated-code records, inserts a
  matching `$OMEGA` immediately before the FREM block — located by counting
  etas, so it is right whether the skip omegas are written as `$OMEGA` or as
  `$OMEGA BLOCK(1)` — and attaches the eta to a `$PK` line as `* EXP(ETA(k))`
  (`link = "exp"`, the default), `+ ETA(k)` (`"add"`), or not at all
  (`"none"`).

  `thetaInit` and `omegaInit` have no defaults; they are modelling choices. The
  `.ext` and `.phi` are **not** migrated — only the control stream is rewritten,
  and the model has to be re-estimated. `fremModelInfo()` warns if you later
  pair the rewritten model with the old `.ext`, whether the model gained a
  theta, an eta, or both.

* **`fremModelInfo()` — the FREM structural integers, derived.** Works out
  `numNonFREMThetas`, `numSkipOm`, `numParCov`, `numFREMThetas` and `numSigmas`
  from a FREM model and its `.ext`, and says so when the two describe different
  structures. `fremParameterTable()`, `createFFEMmodel()`, `createFFEMdata()`,
  `calcEtas()` and `updateFREMmodel()` now accept `NULL` for
  `numNonFREMThetas` / `numSkipOm` and derive them through it; a supplied value
  that disagrees with the model warns and is kept. `createFREMmodel()` works
  from a base model, which has no FREM block to derive `numSkipOm` from, so it
  still needs `numSkipOm`; its `numNonFREMThetas` defaults to the base model's
  theta count.

* **`add.stamp` on the plotting functions.** `plotExplainedVar()`,
  `traceplot()`, `plotEtasCov()` and `plotCovDist()` gained
  `add.stamp = FALSE`. With `TRUE`, a caption recording the source directory
  and the time of generation is added through `PMXForest::addStamp()`.
  `traceplot()` returns a list of plots and stamps each one.

* **`oneHot` in `getForestDFFREM()`.** New `oneHot` and `oneHotSep` arguments:
  raw multi-level categorical columns in `dfCovs` (and `dfRefRow`) are one-hot
  encoded to the `<cov>_<level>` columns the FREM model uses, so the caller can
  pass the covariate as the data records it.

## Breaking changes

* **`setupDfCovsEV()`'s `additionalCovs` is renamed `conditionalCovs`.** The old
  name says when the argument was added rather than what it does: these are the
  covariates `getExplainedVar()` conditions on when computing the variability
  each FREM covariate explains. `additionalCovs` still works and gives an
  identical result, with a deprecation warning; supplying both is an error
  rather than a silent precedence rule.

* **`plotExplainedVar()` no longer takes `...`, and `add.stamp` is no longer
  read from the global environment.** The `add.stamp = TRUE` path called
  `PhRame::add_stamp()`; `PhRame` is internal and PMXFrem is public, so that
  path could only error for an external user. The stamp is kept and now goes
  through `PMXForest::addStamp()`, which takes no extra arguments — so `...`,
  whose only consumer was the `PhRame` call, is gone with it. Picking
  `add.stamp` up from a same-named variable in the global environment is also
  gone: a plot should not depend on an invisible global.

* **`calcFFEM()` no longer takes `...`.** It was never used in the body — a
  silent sink that swallowed mistyped argument names. `createFFEMdata()`
  correspondingly stops forwarding its `...` into `calcFFEM()`, since it already
  passes every argument explicitly; its `...` now goes to `getFileNames()`
  alone. `calcParameterEsts()` keeps its `...`, which *is* forwarded and which
  `fremParameterTable()` relies on.

## Bug fixes

* **`fremParameterTable()`'s RSE and CI were not reproducible.** They are
  computed from `n` sampled parameter vectors (default 175), and `seed`
  defaulted to `NULL`, so the sampling inherited whatever RNG state the caller
  happened to arrive with. Two consecutive calls in one session could differ by
  20% — `RSE (%)` of 1.58 then 1.38 for the same model — and the value depended
  on what had drawn random numbers earlier. `seed` now defaults to `1`, so the
  same inputs give the same number; pass `seed = NULL` for the old behaviour.
  **Your RSE and CI values will change once** as a result, and then stop moving.

  The function also no longer leaves the caller's random stream where it landed.
  The previous `set.seed()` silently advanced the global RNG, so producing a
  parameter table moved an unrelated simulation along; the stream is saved and
  restored.

* **`fremParameterTable()` reported a negative shrinkage as `0.00` in both of its
  documented modes.** `calcFremShrinkage()` clamps negative shrinkages to 0,
  and `fremParameterTable()` called it with that clamp, so its own reporting
  rule never saw a negative value. The default is documented to show a negative
  shrinkage as `1.0000e-10`, the way NONMEM does, and `rawShrinkage = TRUE` to
  show the calculated value; both showed `0.00`. On the bundled `run31max1-2`,
  ETA3's variance shrinkage is -26.44%. **A negative shrinkage in a parameter
  table now reads `1.0000e-10` by default, or its raw value with
  `rawShrinkage = TRUE`.** `calcFremShrinkage()` gains `clamp = TRUE`; its own
  default output is unchanged.

* **`getExplainedVar()` reported no explained variability for a single-covariate
  `dfCovs`.** A one-column data frame collapses to a numeric vector when a row
  is taken from it, so the covariate names came back empty, no covariate was
  ever active, and `COVVAR` was 0 while `TOTVAR` and `TOTCOVVAR` — which do not
  go through that step — were right. The same indexing also handed the
  parameter function something that was not a data frame.

* **`getExplainedVar()` could pass a too-short eta vector to the parameter
  function.** Two internal "all etas zero" calls sized the vector from the
  number of structural thetas rather than from the model's random effects. It
  is now `numSkipOm + numParCov`, the dimension the same function already uses
  for its eta samples.

## Under the hood

* **PMXForest (>= 1.3.0)** is the new floor. The parameter-function generator is
  built on `PMXForest::nmParsePK()` / `nmDeparse()`, and the stamp on
  `PMXForest::addStamp()`.

* **A shared one-hot encoder.** `addFREMcovariates()` binarises through
  `PMXForest::oneHotEncode()` instead of a private implementation. Column names,
  order, values and warnings are unchanged; the `<cov>_<level>` convention now
  has one implementation across the two packages.

* **`getForestDFFREM()` assembles its result once.** It built the
  per-parameter-vector result with a per-cell `data.frame()` inside a growing
  `bind_rows()` loop; it now fills typed column vectors and constructs the data
  frame once. The output is unchanged.

* **An argument-name audit across both packages.** Removing `calcFFEM()`'s `...`
  surfaced that `test-calcFFEM.R` called it as `etaFREM =` when the formal is
  `fremETA`, which the sink swallowed — so a case named "compute eta_prim" never
  computed one, and passed, because what it snapshotted is identical with and
  without the eta. The recorded snapshots also had the subject `ID` sitting in
  the eta vector, because `getPhi()` was passed unfiltered. Both are fixed and
  those snapshots re-recorded from correct input. Sweeping both packages for the
  same shape — a named argument that is not a formal of the callee, landing in a
  `...` the callee never uses — found one more: `createFFEMmodel(baseModdName =)`,
  a typo for `baseModName`, inside an `expect_error()` that passed for the wrong
  reason.

# PMXFrem 2.1.0

## New Features
* **Cholesky Decomposition Support**: Added the `omegaToData` argument to `createFFEMdata()` and `createFFEMmodel()`. When set to `TRUE`, this extracts the variance-covariance matrix elements as `V`-columns (e.g., `V11`, `V21`) directly into the dataset. It rewrites the NONMEM `$OMEGA` block as an identity matrix and adds equations ($V = L L^T$) in the `$PK`/`$PRED` blocks to map independent standard normal `ETA`s to correlated `MYETA`s.
* **Automated Fixing of Covariate Parameters**: Added the `fixTheta` argument (default `TRUE`) to core data assembly functions. The functions now automatically add the `FIX` flag to initial `$THETA` estimates for fully observed covariates. Covariates with missing data remain estimated, which improves overall EM algorithm stability.
* **Estimation Block Validation**: Added evaluation checks for the base NONMEM model's `$EST` block prior to FREM generation. The package now warns users about sub-optimal configurations, including the use of SAEM with missing covariates, absence of IMP/IMPMAP methods, `NITER` < 150, or incorrect `PHITYPE` settings.

## Under the Hood & Refactoring
* **Missing Value Handling**: Removed hardcoded `-99` values across the core data assembly functions (`createFREMmodel`, `updateFREMmodel`, `prepareNewCovariates`, `createFREMData`, `augmentFremData`, `calcEtas`, `addFREMcovariates`, `setupDfCovsEV`). These are replaced with a `missVal` argument, allowing the package to handle alternative sponsor data conventions for missingness (e.g., `-999`, `NA`).
* **Parameter Dimension Checks**: Added length checks in `calcFFEM()` to ensure that the number of provided parameter and covariate labels exactly matches the parsed `numParCov` and `numFREMThetas` derived from the model. This prevents vector-recycling errors and matrix-dimension mismatches.
* **Matrix Formatting**: Added the `forceSingleBlock` argument to `buildmatrix()` to override block-diagonal detection. This formats expanded `$OMEGA` blocks as a single dense block, preventing the matrix from splitting when structural zeros are added during minimal model creation.
* **Parsing Improvements**: Updated NONMEM `.ext` file parsing in `initializeModelParameters` to avoid factor-coercion issues in legacy R versions (version < 4.0). Prevented `write.table` from automatically applying scientific notation to `ITERATION` strings.
* **Parallel Execution Updates**: Explicitly mapped arguments (`omegaToData`, `numSkipOm`) into the `foreach` closures in `createFFEMdata()` to prevent variable scoping issues across different parallel backends.
* **Function Signature Updates**: Resolved recursive lazy evaluation caused by parameterizing default arguments, and corrected positional argument matching in internal functions.

## Documentation & Testing
* Updated `roxygen2` documentation and executable `@examples` for `createFFEMmodel` and `createFFEMdata` to comply with CRAN file I/O policies.
* Improved the `testthat` suite by scoping missingness tokens during mock data generation, using a recursive object stabilization helper (`stabilize()`), and updating test snapshots to reflect dynamic parameter labeling and Cholesky matrix outputs.

# PMXFrem 2.0.0

## New Features
* **Create FREM models:** Added `createFREMmodel()`, a robust function for FREM model generation that can replace PsNs frem command.
* Added `keepDoseOnlySubjects` toggle across the pipeline to allow retention of subjects without PK observations (defaults to `FALSE` for legacy compatibility).
* **Unified Parameter Engine:** Refactored `fremParameterTable()` to guarantee base parameters and covariate coefficients are derived from the exact same bootstrap sample space.
* **Covariate Coefficient Tables:** Added `coefficientTable_long` (API-ready) and `coefficientTable_wide` (document-ready) outputs.
* **Dynamic Uncertainty Formatting:** Added `uncertainty` (`"RSE"` or `"CI"`), `ciLevel`, and `sigDigs` arguments to `fremParameterTable()` with trailing zero preservation (`%#`).
* **Covariate Label Generator:** Added `generateCovNames()` for dynamic/interactive Forest Plot label generation.
* **Integrated Shrinkage Reporting:** `fremParameterTable()` now natively reports parameter shrinkages via the `includeShrinkage = TRUE` and `ffemModName` arguments. Users can control the reported metric (`shrinkageType`), decimal precision (`shkDigs`), and choose to report raw mathematical values or NONMEM-style floored values (`rawShrinkage`).
* **New Features:** Added diagnostic plotting functions `plotEtasCov()` and `plotCovDist()`. `plotEtasCov()` creates faceted scatter plots to compare FREM ETAs, ETA_PRIMs, and FFEM EBEs against covariates. `plotCovDist()` generates distribution plots for estimated covariates. Both automatically visualize and group data based on original covariate missingness.
* **New Vignettes:** Added Tier 1 (`quick-start.Rmd`) and Tier 2 (`walk-through.Rmd`) workflows. Deep-dive vignettes for `createFREMmodel`, standard diagnostics, forest plots, explained variability plots and `updateFREMmodel`.

## Architecture & Performance
* **Modularized Variance Engines:** The monolithic `getExplainedVar.R` script has been completely decoupled. The Delta Rule approximation (`type=0`) and empirical variance evaluations (`type=1,2,3`) are now isolated into pure, standalone helper functions within `R/utils-getExplainedVar.R`.
* **Eradicated `rbind` Memory Leaks:** Completely removed iterative `rbind` and `dplyr::bind_rows` calls from inside the deep ETA sampling and permutation loops in `getExplainedVar()`. Data frames are now accumulated via pre-allocated lists and bound exactly once per execution block, removing exponential memory reallocation and drastically reducing execution time for large pharmacometric cohorts.
* **Removed Regex AST Hacks:** Eliminated the computationally expensive and brittle `jxrtp47` string replacement loop. FFEM expressions are now natively and securely evaluated using isolated environment scoping (`eval(..., envir = list(data = datatmp))`).

## Deprecations & Guardrails
* **Stable Dataset Sorting:** Output datasets are now automatically sorted using a stable index (`ORIG_ROW_IDX`) and `FREMTYPE` to protect intra-subject sequences. The `cstrSortCols` argument in `updateFREMmodel` is deprecated and will be safely ignored with a warning.
* **Strict Covariate Validation:** To ensure data hygiene, dichotomous covariates coded as `1/2` will now trigger a validation error by default. Legacy PsN compatibility can be maintained by explicitly setting `allowNon01 = TRUE` or `bRecodeDichotomous = TRUE`.
* **API Cleanup:** Internal pipeline functions (e.g., `augmentFremData`, `prepareAndValidateData`) have been moved to the internal namespace to declutter the user-facing API.
* renames `setypdfCovs()` to `setupDfCovsEV()`

## Bug Fixes
* **Covariate Cross-Contamination (`TOTCOVVAR` Anomaly):** Fixed a critical regex bug (`length(grepl(...))`) in the missing data mapping function that caused covariates with overlapping substrings (e.g., `AGE` and `PAGE`) to be incorrectly overwritten with `-99`. This resolves an anomaly where `TOTCOVVAR` was artificially suppressed below `COVVAR` due to valid data being silently dropped.
* **Windows PSOCK Parallelization:** Resolved "Object not found" crashes on Windows machines when using `ncores > 1` in `getExplainedVar()`. By isolating the empirical variance engine, the local environment is now explicitly bundled and exported (`.export = c(ls(environment()), ...)`) to `foreach` workers, bypassing fragile static code analysis.
* **Orphaned Cluster Teardown:** Parallel backend clusters in `getExplainedVar()` are now safely terminated via an `on.exit(doParallel::stopImplicitCluster(), add = TRUE)` handler, preventing memory locks if the variance calculation throws a fatal error.
* **Lazy-Evaluation Signature Traps:** Removed dependencies on `dfext` from the default arguments in the `getExplainedVar()` function signature (e.g., `numFREMThetas`, `numSigmas`). These are now explicitly calculated in the function body *after* `dfext` is validated, preventing silent scoping crashes.
* **SD Scale Transformation:** Fixed `fremParameterTable()` logic so SD transformations occur *before* CI/RSE calculations, ensuring uncertainties match the reported scale.
* **ETA Mismatch in EV:** Fixed longitudinal subsetting (`!duplicated(data[[strID]])`) to prevent ETA length mismatches in `getExplainedVar()`.
* **Missing Covariate Failsafe:** Added explicit `stop()` errors for missing categorical covariates in `getExplainedVar()`.
* **NSE Scoping in data.table:** Fixed `j symbol` scoping errors by coercing inputs to base `data.frame` with strict `drop=FALSE` subsetting in variance calculations.
* **Exact List Extraction:** Replaced bracket subsetting with `[[cov]][1]` to fix vulnerability in dummy column mapping.

## Chores & Documentation
* **Deprecations:** Deprecated `cstrSortCols` in `updateFREMmodel()` (sorting is now native to PMXFrem v2).
* **CRAN Compliance:** Refactored examples to use base R (`read.csv`) and `tempdir()`.
* **Roxygen:** Injected extensive `@family` and `@concept` tags across all functions for `pkgdown` organization.
* **Documentation:** Added a new vignette (`walkthrough-createFREMmodel.Rmd`) demonstrating the end-to-end process of building a Full Random Effects Model and dataset from scratch using `createFREMmodel()`.

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
