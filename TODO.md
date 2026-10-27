# PMXFrem — design backlog

Items parked for future consideration. Not a substitute for GitHub issues; move an
item there when it becomes active work.

## Release plan: next release after PMXForest 1.4.0

Agreed 2026-10-26: release PMXForest 1.4.0 and the next PMXFrem as soon as
possible, PMXForest first (its plan is in PMXForest's `TODO.md`). PR #78
re-exports `generateCovNames()` from PMXForest and requires
`PMXForest (>= 1.4.0)`; its CI fails until the `v1.4.0` tag exists, which is
expected, not a fault in the PR.

1. Create an epic branch from `main`, bump `DESCRIPTION` to `2.2.0.9000`, and
   retarget #78 to it (it targets `main` today).
2. Land there: T34-T36 (bugs), T39 (`$TABLE`), T38 (vignette); T33 if
   PMXForest 1.4.0 exports the worker startup. PRs into the epic branch get no
   CI, so test locally against PMXForest's `epic/1.4.0`.
3. Before merging #78, fix its NEWS entry: `print_template = TRUE` now prints
   PMXForest's new template (`name_map` / `unit_map` / `level_map`) instead of
   `group_map` / `label_map`, so "work as before" is not true for it. PMXFrem's
   old `generateCovNames()` tests otherwise all pass against PMXForest's
   version (21 of 24 assertions; the 3 failures are the template).
4. Once `v1.4.0` is tagged: epic -> `main` PR, CI green, dev release, tag -
   each on Niclas's go-ahead. Proposed version 2.3.0 (new behaviour, not only
   fixes); the number is Niclas's call.

Decided 2026-10-26 (Niclas), after the planning pass:

- Version **2.3.0**, branch `epic/2.3.0`; one release, no 2.2.1.
- T34: a data file with more columns than `$INPUT` items is accepted (the
  first N named, a message names the rest); fewer is an error. A `DROP` item
  named in `cstrKeepCols` or `covariates` is an error. Filter on `IGNORE` /
  `ACCEPT` before removing dropped columns.
- T35: a covariate counts as missing only for a subject with no non-missing
  value. Align phase 1 with phase 2 in the same PR: a first 0/1 covariate is
  named `SEX`, not `SEX_1`, and phase 1 uses the first non-missing value.
- T36: an initial value on or outside a bound is nudged inside, with a
  message naming the THETA. The one-covariate path starts from the `.ext`
  final estimates too, as the multi-covariate path does.
- T37: keep the single-character `IGNORE` warning (no implementation of it);
  silent when the first column is numeric, and once per call.
- T39: `$TABLE` items the FREM data does not carry are dropped with a message.
  `FILE=`: swap the run number when it ends in the base run number and
  `fremModName` is `run<N>` (`xptab30` -> `xptab31`), otherwise append
  `_<fremModName>`; `FREMTYPE` goes at the end.
- #78: Claude retargets, rebases and corrects its NEWS on Ari's branch, and
  says so in the PR. T33 keeps the `PMXFrem.freshWorkers` option working.
- T38: the `DROP` example is derived from `run30` inside the vignette.
- NEWS: one consolidated PR at the end, reviewed on a doc-review page.

Also found while planning, beyond T34-T40: recoding writes `-99` rather than
`missVal` (`R/prepareNewCovariates.R:85`, `R/createFREMData.R:175`); T37's
cause is that the minimal model writes `$DATA <file> IGNORE=@`
(`R/createMinimalFremModel.R:168`), so phase 2 warns even when the base
model has none; `tests/testthat/test-utils-data_prep.R:21` encodes the old
`DROP` behaviour and is rewritten in T34. `make check-format` ends with
`git checkout -- .`, which discards uncommitted edits - commit first.

Later, not this release: PMXForest 1.4.0's `filterByModel()` identifies
observations and applies `$DATA` filters the way NONMEM does. Reusing it in
`createFREMmodel()` instead of PMXFrem's own `$INPUT` / `IGNORE` / dose-only
handling (where T34 lives) would give one implementation - but PMXForest's
friction item 3 questions `filterByModel()` itself, so settle that first.

## Deep-dive vignette — extending a FREM model with THETAs and IIVs

Now unblocked: T3 / T4 landed in PR #62, so the signatures and output shape are
settled. Walk through `addFremStructuralTheta()` with `addEta` on and off,
`addFremIIV()` used directly, what the renumber pass does to `ETA` / `MU_` /
`COV` / `THETA` indices, why the added definition uses the `TV<par>` /
`MU_k = LOG(TV<par>)` house form rather than a direct `MU_k = THETA(j)`, and
why the `.ext` / `.phi` are not migrated (the model must be re-estimated).

## T9 — a small library of secondary-parameter files (PMXForest-private)

Ship a handful of ready-made secondary files under `inst/secondary/` as
**examples / starters**, not a general solution (parameter-name mapping across
models can't be done generically — see the design discussion). Written against
canonical names (`.CL` / `.V` / `.KA` / `.F` / `.dose` / `.tau` / `.n`) with the
config list supplying expressions for the non-default ones, e.g.
`list(source = "cmax_1cmt_oral_ss.R", dose = 100, tau = 12, KA = "1/(MAT - D1)")`.

Prefer closed-form where it exists (pure R, no mrgsolve, exact tests):
- `cmax_cmin_1cmt_iv_ss.R` — analytical steady-state Cmax/Cmin (two files, or
  one file per quantity since a secondary returns one value).
- `cmax_cmin_1cmt_infusion_ss.R` — analytical.
- `cmax_cmin_1cmt_oral_ss.R` — Cmin analytical; Cmax via a short `uniroot` on
  `dC/dt = 0`, not a dense-grid `optimize`.
- keep `secondary-cmax-mrgsolve.R` as the multi-compartment / complex-regimen
  fallback.

Each file states its assumptions (steady state, linear, single input) loudly.
Document that non-standard models (transit absorption, TMDD, non-linear CL,
time-varying regimens) need a hand-written file. Add exact tests for the
closed-form ones; a slow mrgsolve-backed test behind `Suggests`.

## T12 — modernise the `getForestDF*` parallel backend

The `getForestDFSCM()` / `getForestDFFREM()` / `getForestDFemp()` family (and
`createFFEMdata()`) use `foreach` + `%dopar%` on `doParallel`, with a hard-coded
`registerDoParallel(cores = ncores)` and manual `.packages = cstrPackages` /
`.export = cstrExports`.

Move to `future` (`future.apply::future_lapply` / `furrr`, or `doFuture` as a
drop-in for the existing `foreach %dopar%`). Benefits: automatic global +
package detection (drop `cstrPackages` / `cstrExports` and their "works locally,
fails on the cluster / on Windows" failure mode - see T14), one code path for
all OSes including Windows, parallel-safe RNG via `future.seed = TRUE`, and the
user chooses the backend with `plan()` (local `multicore` / `multisession`, or
an HPC scheduler via `future.batchtools`) instead of the package hard-coding it.
Keep `ncores` as a convenience that sets up a transient `plan()` with an
`on.exit()` restore; document `plan()` as the real control. Supersedes T14.

**Moving to `future` does not by itself keep the workers on the session's
package versions** (checked 2026-09-30, future 1.32.0). A package the session
loaded from a folder not on `.libPaths()` - PMXRenv's `library.versioned()`,
`library(lib.loc = )` - resolves to another installed copy on `multisession`
workers exactly as on doParallel's Windows workers: with the session on a
versioned 1.2.11, `future_lapply()` and `doFuture` + `foreach` workers both
loaded the 2.1.0.9000 on `.libPaths()`. And it would widen the exposure:
`multisession` starts fresh sessions on every OS, where doParallel forks on
macOS/Linux, and `future` refuses `multicore` under RStudio. So carry
`.fremStartWorkers()`'s folder lookup and version check across - pass the
folders as `rscript_libs` to `plan(multisession)` and keep the check.

## T19 — make mutation testing a routine rather than a hand exercise (both packages)

Every claim in a commit message this release of the form "mutation-tested:
removing X fails N" was produced by hand: copy the source file aside, break one
line, run one test file, read the count, restore. It works, and it has been
worth it — it found four tests that asserted nothing and would have gone on
passing forever:

- `**` right-associativity, asserted with `2`, `2`, `2`, where both
  associativities give 16;
- the unary-minus constant fold, asserted with `fold("-2") == "-2"`, which holds
  with or without the fold;
- `"NA in a covariate column is dropped"`, which used a categorical covariate,
  where `sort()` drops `NA` on its own;
- `"a malformed per-covariate setting is rejected"`, which used a length-1
  logical that cannot reach the length check it was named for.

Doing it by hand has one failure mode worth designing against. **A mutation
whose pattern does not match silently does not apply, and the run then reports
zero failures — which reads exactly like "the test is vacuous" when the test may
be perfectly good.** That happened twice in this release: once on a `sed`
pattern that did not match the real text, once on an operator table entry
spelled differently from the guess. Both times the first reading was wrong and
only a re-check caught it.

So whatever is built, the harness must **assert the file actually changed**
before running anything, and print the diff it applied. That single check is
most of the value.

Worth scoping before building:

- Look for a maintained R mutation-testing package first; the ecosystem is thin
  and the last time this came up nothing obvious was in use here. Do not write a
  framework if one exists.
- Otherwise a small `make mutate FILE= PATTERN= WITH= TESTS=` target is probably
  enough: apply, assert changed, run that test file, report, restore from git.
  It does not need to enumerate mutants automatically — the value so far has
  come from targeted mutations chosen because a specific assertion looked weak.
- Note that a mutation which legitimately changes no behaviour is a *result*,
  not a gap. One in this release — dropping a lookbehind that the alternation
  order already made redundant — correctly failed nothing, and the right
  response was to fix the comment claiming it was load-bearing. A harness cannot
  tell those apart, so its output still needs reading rather than gating CI on
  it.

---

## T21 — an FFEM covariate's reference value is 0, and nothing knows it

`PMXForest::createParamFunction()` refuses an FFEM model until the FREM
covariates are pinned:

    No reference value could be derived from run1715ffem.mod for: CLFREMCOV,
    V2FREMCOV, MATFREMCOV, V3FREMCOV.

`covRef = list(CLFREMCOV = 0, V2FREMCOV = 0, MATFREMCOV = 0, V3FREMCOV = 0)`
makes it work, and the result checks out - typical CL comes back as THETA(1)
exactly. But 0 is not a guess there: it is what the parametrisation means. The
covariate enters as a bare additive term inside the same `EXP()` as the ETA,

    CL = EXP(MU_7 + (ETA(7) + CLFREMCOV))

so the typical subject, carrying no covariate effect, has it at 0.

PMXForest could derive that - "a covariate appearing only as a bare additive
term inside an `EXP()` alongside an `ETA` has reference 0" - but this belongs
here rather than there. PMXFrem generates these columns and names them, so it
knows which they are without pattern-matching; and PMXForest has just been
burned once by an inference rule that read a structure backwards, silently
(T20's sibling: a branch assigning the identity value was taken as the
reference category when it was the departure from one).

Options:

- `createFREMParamFunction()` supplies `covRef` for the FREM columns itself,
  since it knows their names. Cheapest, and keeps the guessing out of
  PMXForest.
- A helper that builds the `covRef` list from a FREM model, for callers using
  `PMXForest::createParamFunction()` directly on an FFEM model.

Either way the user should not have to know that the reference is 0, or type
four names to say so.

## T22 — 211 lines over 120 characters, and the linter that would catch them

`line_length_linter` is disabled in `.lintr`. PMXForest keeps it at 120 and
passes; PMXFrem has 211 lines over, with a long tail — 100 in 120-139, but also
11 over 200 and one at 306. No limit short of meaningless reaches green, and
hand-wrapping 211 lines is a large mechanical diff with real risk of breaking an
expression, which is not what to do immediately before an independent review.

Reduce them in a dedicated PR, then re-enable the linter at 120 and converge
with PMXForest. Worst offenders first: `awk 'length>200' R/*.R`.

## T23 — a Suggests dependency is invisible to CI unless listed twice

`ci.yml` installs `dependencies: '"hard"'`, which does not install `Suggests` at
all. Test-only dependencies therefore have to be named **both** in `Suggests`
and in the workflow's `extra-packages` block, and nothing enforces the pair.
`readr` was dropped from `Imports` (correctly — `R/` does not use it) and seven
tests died with "there is no package called 'readr'", taking a whole file with
them, and it took a CI log to find out.

Either set `dependencies: '"all"'` — which pulls in GGally, kableExtra, rmarkdown
and R.rsp, so it is slower and has more failure surface — or add a check that
every package a test calls with `::` appears in `extra-packages`. Worth deciding
deliberately rather than remembering.

## T25 — `stabilize()` rewrites NM-TRAN operators, not just numbers

`stabilize_text_snapshot()` (tests/testthat/helper-stabilize.R) finds every
number-like substring and reformats it, which in a control stream turns

    IF(FOOD.EQ.1) MATFOOD = 1
    IF(FOOD.EQ.0) MATFOOD = ( 1 + THETA(6))

into `IF(FOOD.EQ0.1)` / `IF(FOOD.EQ0)` - it reads `.1` as the number and eats
the `.` that closes `.EQ.`. While snapshots were base64 this was invisible;
now that model text is snapshotted as text it is not, and
`expect_model_snapshot()` works around it by not stabilising the model at all.

Two things to settle. The regex should not treat a `.` bounded by letters as
part of a number, so `.EQ.`, `.NE.`, `.GT.` survive. And every other text
snapshot that goes through `stabilize()` should be checked for the same
mangling - `_snaps/createFFEMmodel.md` and `_snaps/generateFremModel.md` hold
model text too.

## T26 — `_snaps/calcEtas.md` is still 871 kB of base64

The three largest serialized snapshots were replaced with readable JSON;
`calcEtas.md` was left, and is now the biggest file under `_snaps` by a factor
of ten. It snapshots eta tables, which `columnDigest()` plus a head would cover
the same way. Worth the same treatment, with the same mutation check
afterwards.

## T27 — `expect_forest_sampling_sane()`'s `relBand` was not derived from data

The helper bounds each `_REL_` column to `[1e-3, 1e3]` on the reasoning that a
ratio to a reference is of order 1. That is a judgement, not a measurement: it
was chosen to catch a x1000 units slip and checked only against the two calls
that use it. Run it over a range of real forest results - several models, wide
covariate ranges - and either narrow the band to what those actually occupy or
record why it has to stay this wide.

## T28 — two verifier/generator paths no bundled fixture can reach

From the round-B mutation audit; both are real gaps, neither fixable from
tests in this package alone.

- `verifyFREMParamFunction()` gates the splice checks on
  `scale == "exp" & spliceable`. The `scale` half is shadowed on every fixture:
  `PMXForest::createParamFunction()` never places an additive eta in `etaMap`,
  so a non-log-normal parameter is already excluded by `spliceable`. Removing
  the `scale` gate changes no test. The code comments say the scale decides
  it; on current fixtures the reference does. Either give PMXForest's
  `nmEtaMap()` the additive idiom or reword the comments.
- `getExplainedVar()`'s placeholder eta vectors are now
  `rep(0, numSkipOm + numParCov)` instead of `rep(0, 3 * length(thetas))`. No
  bundled model has `numSkipOm + numParCov > 3 * numNonFREMThetas`, so no test
  distinguishes the two. A small FREM fixture with many skip omegas and few
  structural thetas would.


## T32 — `fremParameterTable()` gives one non-finite RSE on OpenBLAS

On this machine (R 4.2.3, OpenBLAS 0.3.32) `test-fremParameterTable.R:155` fails
four times - "every RSE should be finite" - on unchanged `epic/2.1.1`, while the
same commit passes CI (R 4.2.2, the runner's BLAS) and passed on the previous
machine. `fremParameterTable(runno = 31, bsFile = bs31.dir/raw_results_run31.csv,
includeRSE = TRUE, ...)` returns `NA` for the RSE of parameter-table row 11 (an
OMEGA) with "NaNs produced"; the thread count makes no difference
(`OPENBLAS_NUM_THREADS` 1 and 8 agree). Likely a variance that is zero in exact
arithmetic coming out slightly negative under this BLAS before a `sqrt()`, but
not yet traced to the line. Worth finding: a user on OpenBLAS gets the same `NA`.

## T34 — `createFREMmodel()` stops on any `=DROP` item in `$INPUT`

From user feedback on 2.2.0 (T34-T40). `prepareAndValidateData()`
(`R/utils-data_prep.R:53-65`) removes the `DROP` items from the `$INPUT` names
and then requires the data file to have exactly that many columns, so every
dropped column is counted as a mismatch:

    The number of columns in the data file (34) does not match the number of
    items in the $INPUT record (33).

Reproduced with the bundled `run30` by changing `SMOK` to `SMOK=DROP`. Dropping
trailing date and subject-ID columns this way is routine, so this blocks most
real models; the workaround is to delete the columns from the data first.

- Name the columns positionally from all `$INPUT` items, then remove the
  dropped ones from `df`. Check whether `IGNORE`/`ACCEPT` may refer to a
  dropped item in NONMEM; if so, filter before removing them.
- Treat `=SKIP` as `DROP` (it currently passes only because it is not
  recognised), and match `=DROP` / `=SKIP` as the value, not `grepl("DROP")`,
  which would also drop a column named e.g. `DROPOUT`.
- Check whether NONMEM accepts a data file with more columns than `$INPUT`
  items; if it does, the equality check is stricter than NONMEM.
- No bundled model uses `DROP`, which is why the tests never saw this. Add a
  fixture that does.

## T35 — phase 1 fixes a categorical first covariate's THETA despite missing values

The rule is to fix a covariate's THETA only when the covariate has no missing
values. Phase 2 (`prepareNewCovariates()`) applies it to both kinds; phase 1
(`createFREMmodel_phase1()`, lines 112-143) applies it only to a continuous
first covariate and uses `shouldFixTheta = fixTheta` for a categorical one.
With the bundled data (`RACEL` missing for 42 subjects):

- `covariates = c("AGE", "RACEL")`: `TV_RACEL_2`, `TV_RACEL_3` estimated.
- `covariates = c("RACEL", "AGE")`: both written `FIX`.

So the result depends on the order of `covariates`. Phase 1 also checks
missingness on each subject's first record only, phase 2 on all rows; use one
rule. Write the test first: it should go red on the reordered call.

## T36 — the generated FREM model loses the base model's `$THETA` bounds

`generateFremModel()` (`R/generateFremModel.R:259-261`) rewrites every
`$THETA` as `$THETA <value> [FIX] ; comment`, so the bounds in the base model
are gone - in `run30`, `(0,7.62228)` on `TVCL` becomes `6.17206`, and the
`(-1, …, 3)` bounds on the covariate effects are dropped too. A structural
parameter can then be estimated negative. Carry each base THETA's lower/upper
bound through, with the initial value from the `.ext` clamped inside them.
This affects `updateFREMmodel()` on an existing FREM model as well.

## T37 — the single-character `IGNORE` warning: twice, and for `IGNORE=@`

`filterDataFromModel()` warns about `IGNORE=@` / `IGNORE=C`, and is called
from both `prepareAndValidateData()` (phase 1, on the base model) and
`updateFREMmodel()` (phase 2, on the minimal model). The minimal model writes
its own `$DATA <file> IGNORE=@` (`R/createMinimalFremModel.R:168`), so phase 2
warns on every call - even when the base model has no single-character
`IGNORE` at all - and a base model with `IGNORE=@` warns twice. Warn once per
call, and not for an `IGNORE` PMXFrem wrote itself. Also don't warn when the
data's first column is numeric: then `@` or `#` can only skip the header,
which is how nearly every model uses it.

## T38 — `createFREMmodel()` vignette: what users had to find out for themselves

Part 3 does not cover several things the feedback asked about. Add:

- **What the base model needs.** MU referencing, `$ESTIMATION` set up for
  FREM (IMP/IMPMAP, `NITER`, `PHITYPE=1` - the pre-flight warnings), and how to
  choose `numSkipOm`.
- **`cstrKeepCols` takes `$INPUT` names**, not the data file's header names
  (`prepareAndValidateData()` renames the columns to the `$INPUT` names).
  Also say so in `@param cstrKeepCols`.
- **Design decisions, stated as such:** covariate THETAs are fixed when the
  covariate has no missing values (`fixTheta`); subjects with no observations
  are left out (`keepDoseOnlySubjects`), as agreed with the PsN developers for
  newer PsN versions. Once T39 lands, say how the `$TABLE` is carried over.
- **Recoding moves the covariate's mean THETA.** With `bRecodeDichotomous =
  TRUE`, a 1/2-coded covariate's `TV_` THETA is its mean on the 0/1 scale, one
  lower than a PsN model on the original coding gives. Make the generated
  comment say what `SEX_2 = 1` means.
- **The example call.** Drop `numNonFREMThetas = 7` (the prose already says it
  is read from the `.ext`, and the call contradicts it), and stop hiding the
  warnings with `warning = FALSE` - users will get them.
- **The example data** has no `DROP` columns and none of the standard date and
  subject-ID columns that real data sets carry. Use one that does, once T34 is
  fixed.

## T39 — carry the base model's `$TABLE` records into the FREM model

Decided: keep the base model's `$TABLE` records. Today
`createMinimalFremModel()` (`R/createMinimalFremModel.R:186`) strips every
one of them. Points to handle:

- Items that are data columns of the base `$INPUT` but not of the FREM
  `$INPUT` (anything outside `cstrKeepCols` and the covariates) would stop
  NM-TRAN. `run30`'s table lists `NO`, `STUDYID`, `TAD` and others the FREM
  data set does not carry. Drop those items, and say which.
- Rename `FILE=` so the FREM run does not overwrite the base run's tables
  (`run30` writes `xptab30`).
- Add `FREMTYPE`, so the covariate rows can be told apart from the
  observations.
- `updateFREMmodel()` on an existing FREM model must keep its `$TABLE` too.

## T40 — a report that `createFREMmodel()` needs PMXForest attached

A user had to load PMXForest as well as PMXFrem. Not reproduced: the package
calls PMXForest only as `PMXForest::`, and `createFREMmodel()` runs with
PMXForest loaded but not attached. The report used `PMXRenv`'s versioned
library loading, so the likely cause is that PMXForest was not on the library
path PMXRenv set up. Get the error message before changing anything; if it is
PMXRenv, the fix belongs in the installation instructions, not the vignette.

## T41 — `IGNORE` / `ACCEPT` lists: `.EQ.` compares text in NONMEM, numbers here

Found while doing T34. The NONMEM 7.5 help (`$DATA`) says that with `=`,
`==`, `/=`, `.EQ.` and `.NE.` "the value in the data record and the value in
the list are compared as character strings"; only `.EQN.` / `.NEN.` (and the
ordering operators) compare numerically. `filterDataFromModel()`'s
`translate_condition()` turns `.EQ.` and `.EQN.` alike into R's `==` on the
numeric column read by `fread()`, so a data value written `1.0` matches
`BLQ.EQ.1` here but not in NONMEM, and the FREM data set's cohort can differ
from the model's. Check with a fixture whose file holds `1.0`, then compare as
text where NONMEM does (needs the column as written - read it as character).

## T42 — `$THETA (value)xN` is not expanded

The NONMEM 7.5 help (`$THETA`) allows `(2)x4` for four THETAs.
`.fremCountThetaValues()` - and `.fremThetaBounds()`, which walks the same
tokens - counts it as one value plus a stray `4`, so every later THETA comment
and bound shifts. Rare in practice; expand `(…)xN` before counting.

## T43 — `createFFEMmodel` snapshots fail on Windows: the temp-path placeholder

Three `test-createFFEMmodel.R` snapshots fail on a Windows machine only (CI on
Linux passes). `stabilize()` (`tests/testthat/helper-stabilize.R`) should
replace the temporary data path with `[placeholder_path]`, but the path's
backslashes are stripped first (`C:UsersniclasjAppData…`), so it no longer
matches. Normalise the path the same way on both sides.

## T44 — `$INPUT label=synonym`: which name the data column gets

`.fremInputItems()` (like the code before it) names a column by the label
before `=`. The NONMEM help (`$INPUT`, sample `CP=DV`) makes the synonym
equally valid, and a reserved synonym gives the item its meaning - so with
`CP=DV` PMXFrem names the column `CP` and does not find `DV`. Decide which name
to use (or accept both), with a test on `CP=DV`.

## T45 — recoding writes `-99`, not `missVal`

`prepareNewCovariates()` (line 85) and `createFREMData()` (line 175) write a
literal `-99` for a missing value when recoding a dichotomous covariate, so a
non-default `missVal` is not honoured there.

## T46 — tests with `ncores > 1` fail on Windows under `load_all()`

`createFFEMdata`, `getExplainedVar` and `getForestDFFREM` each have a test
with `ncores > 1`. On Windows the workers are fresh R sessions; they load the
installed PMXFrem, the session runs the source tree, and the version check in
`.fremStartWorkers()` stops them - correctly. CI (Linux, forked workers) is
unaffected. Skip those tests when PMXFrem is not loaded from an installed
library, as `test-parallelWorkers.R` already does for its end-to-end test.

## T47 — say in the generated model what a recoded covariate's level means

Split off from T38 (it is a code change, T38 is the vignette). A recoded
dichotomous covariate appears in the FREM model's `$ERROR` block as
`;        SEX_2 1` under its `FREMTYPE`, which does not say that `SEX_2 = 1`
means "SEX was 2". Make the comment say so - but `getCovNames()` reads these
comment lines back to find the covariates, so the change needs a round-trip
test (`createFREMmodel()` then `getCovNames()` / `updateFREMmodel()` on the
result) before and after, and a check of the snapshots that hold model text.

## Done

- **T33** — PMXFrem starts its parallel workers with PMXForest 1.4.0's
  `startParallelWorkers()`. `.fremStartWorkers()` is now a wrapper passing
  `PMXFrem` as the package to load and the `PMXFrem.freshWorkers` option as
  `fresh`, so the callers, the tests and the option are unchanged. The
  end-to-end tests in `test-parallelWorkers.R` now exercise PMXForest's copy.
- **T31** — `generateCovNames()` belongs in PMXForest. Moved there in
  PMXForest 1.4.0 and re-exported here, so `PMXFrem::generateCovNames()` and
  the attached call keep working. DESCRIPTION and the `ci.yml` pins require
  PMXForest 1.4.0.
- **T15** — `ci.yml` red for months. All four jobs (lint, format, unit tests,
  `R CMD check`) pass on `epic/2.3.0` (run 40337335537); the example, styler
  and snapshot failures it listed were fixed in the 2.2.0 cycle.
- **T17** — snapshot drift. Settled by policy rather than by finding the
  cause: `getForestDFFREM`'s tests snapshot only the reproducible columns,
  with a tolerance, and the parameter tables go through `stabilize()`. They
  pass on CI and on this Windows machine.
- **T18** — `setupDfCovsEV()` takes `conditionalCovs`; `additionalCovs` is
  kept as a deprecated alias.
- **T20** — `verifyFREMParamFunction()` now takes a separate structural
  reference model, and refuses a FREM model as that reference.
- **T24** — `createFREMData()` / `createFREMmodel()` apply the base model's
  `IGNORE` / `ACCEPT` (T34), drop subjects without observations before
  summarising the covariates, and keep the data's original order
  (`ORIG_ROW_IDX`).

- **T29** — `plotEtasCov()` line width on older ggplot2. It passed `linewidth`,
  which ggplot2 < 3.4.0 ignores with a warning, drawing the default width - CI's
  snapshot has 3.3.6. It now chooses `linewidth` or `size` by the installed
  version, so it is correct and silent on both without raising the ggplot2
  requirement (production's version could not be observed). Verified with the
  new test against ggplot2 3.3.6 in a scratch library - 3 failures before the
  fix, 0 after - and against 3.4.2. Open in PMXForest: `forestPlot()` passes
  `linewidth` in `guide_legend(override.aes = ...)` (R/forestPlot.R:369), which
  an older ggplot2 would ignore the same way.

- **T30** — PMXForest pinned for CI. Tried as `Remotes: pharmetheus/PMXForest@v1.3.0`
  and reverted: `remotes::install_deps()` honours it and the self-hosted install
  steps had no GitHub token (HTTP 401), which would also have broken
  `pr-publish-to-dev.yml` and `cd.yml`. Resolved in `ci.yml` instead (a092966,
  817e980): `check-r-package` moved to GitHub-hosted runners, where `setup-r`
  installs qpdf, and both it and `unit-test` install
  `pharmetheus/PMXForest@v1.3.0` through `extra-packages`. First fully green
  `ci.yml` run on the branch: 35205263637.

- **T5** — direct `getCovNames(createFREMmodel() output)` test — PR #40 (merged).
- **T16** — `generateFremModel()` comment alignment. Kept the override (a
  user's `; 3. IIV on CL` is better than the generated `BSV_BASE3` fallback)
  but made the positional mapping robust: `$THETA` advances by the number of
  values a line carries rather than one per line, and `$OMEGA` advances on
  lines carrying values rather than on lines containing a `;`. Block rows are
  counted by triangular number, not by line, because a row may wrap over
  several physical lines with the comment only on the last — the case run31
  itself exercises from `BSV_RACEL_3` on. A standalone note, an uncommented
  record or a multi-value record no longer shifts every later label.
  Well-formed input is unaffected (the round-trip and wrapped-row tests pass
  against both the old and the new code).
- **T3 / T4** — `addFremIIV()` and `addFremStructuralTheta()`, PR #62 (merged).
  Insert-in-place with a full `ETA` / `MU_` / `COV` / `THETA` renumber pass;
  `numSkipOm` / `numNonFREMThetas` derived via `fremModelInfo()`; `thetaInit` /
  `omegaInit` required (modelling choices, no default). A parameter absent from
  `$PK` gets a delimited definition created, one already present is modified in
  place. The MU-referenced form goes through `TV<par>` /
  `MU_k = LOG(TV<par>)` rather than a direct `MU_k = THETA(j)`, which would
  match the pattern `generateFremModel()` uses to locate the FREM block and so
  be spliced away by a later `updateFREMmodel()`. `.ext` / `.phi` are **not**
  migrated — the model must be re-estimated.
- **T1** — `createFREMParamFunction()` / `verifyFREMParamFunction()`. v1:
  PMXForest PR #23 (exported `nmParsePK()` / `nmDeparse()` / `nmFormatNum()`) +
  PMXFrem PR #45. v2: non-log-normal parameters via `fremEtaScale`
  (`"exp"` vs `"other"`), with `COVSPLICE` / `ETASPLICE` / `PASS` reported as
  `NA` where the splice check does not apply — PR #53. All merged. A NONMEM
  `$TABLE`-based check was considered and **won't do** (no bundled FREM model
  tables `CL` / `V` / ...; not worth a fixture).
- **T2** — `fremModelInfo()` rolled out to every entry point that can derive:
  `getExplainedVar()` / `getForestDFFREM()` (PR #41), `@examples` for both
  syntaxes (PR #43), `fremParameterTable()` / `createFFEMmodel()` /
  `createFFEMdata()` / `calcEtas()` (PR #44), and the model-mutation pair
  `updateFREMmodel()` / `createFREMmodel()` (PR #54) — the latter stopping
  loudly when the `;;;FREM CODE` markers / `$OMEGA BLOCK(N)` disagree with the
  `.ext`. All merged. `calcFFEM()` deliberately left alone: no notion of a
  model, low-level, and every caller already resolves the counts first. Not
  applicable to `plotCovDist()` / `plotEtasCov()` / `calcParameterEsts()` /
  `removeFremCovariates()` / `initializeModel*()`. Derivation (verified on
  `run31` → 7/2/3 and `run22-3` → 13/2/4):
  ```
  numFREMThetas    = length(getCovNames(modFile)$covNames)
  numNonFREMThetas = (#THETA columns in ext) - numFREMThetas
  numTotEta        = solve k(k+1)/2 = (#OMEGA columns in ext)
  blockN           = N in the final "$OMEGA BLOCK(N)" of modFile
  numParCov        = blockN - numFREMThetas
  numSkipOm        = numTotEta - blockN   # robust to a structural BLOCK first
  numSigmas        = #SIGMA columns in ext
  ```
- **T11** — coverage audit. **PMXForest 97.43%** (already above the 95% bar, no
  new tests needed) and **PMXFrem 86.64% → 95.01%** (PMXFrem PR #63). The
  `covr` run also surfaced two real bugs that `devtools::test()` never reached
  — `verifyParamFunction()` crashing on an explicitly named parameter, and
  `nmResolveSecondary()` silently dropping an unnamed constant — fixed in
  PMXForest PR #31. All merged.
- **T8** — `secondary` config-list support (PMXForest PR #27, PMXFrem PR #48) +
  two-part `Part3-deep-dive-secondary-parameters.Rmd` vignette split
  (PMXForest PR #28). All merged.
- **T7** — FREM vignettes reworked to the derived `fremModelInfo()` form
  (`createFFEMmodel` / `fremParameterTable` / `getForestDFFREM` /
  `getExplainedVar` / `calcEtas` no longer take the integers by hand) — PMXFrem
  PR #50 (merged). `updateFREMmodel` / `createFREMmodel` stay explicit
  (T2-residual).
- **T10** — PMXFrem `Part3-deep-dive-secondary-parameters.Rmd` (two parts:
  closed-form runs, mrgsolve part shown) — PMXFrem PR #49 (merged).
- **T6** — `chore/session-handoff` branch deleted from PMXForest-private
  (2026-09-04). `SESSION-HANDOFF.md` was never on `epic/v1.3.0`; its content
  was fully absorbed.
- **T14** — Windows/PSOCK `.export` + `on.exit` teardown for `getForestDFSCM` /
  `getForestDFemp` (PMXForest PR #29), `getForestDFFREM` / `createFFEMdata`
  (PMXFrem PR #58). Both merged.
- **T13** — result-assembly rewrite (typed vectors + single `data.frame()`
  build, single `bind_rows()` instead of a pairwise-combine loop) for
  `getForestDFSCM` / `getForestDFemp` (PMXForest PR #30) and `getForestDFFREM`
  (PMXFrem PR #59). Both merged; output verified unchanged.
