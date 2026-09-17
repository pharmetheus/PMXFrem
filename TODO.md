# PMXFrem — design backlog

Items parked for future consideration. Not a substitute for GitHub issues; move an
item there when it becomes active work.

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

## T15 — `ci.yml` has been failing on every branch for months

`.github/workflows/ci.yml` (`lint-check`, `format-check`, `unit-test`,
`check-r-package`) is red on **every** run in recent history - back to at least
2026-06 - including `epic/2.1.0`, the branch that shipped the 2.1.0 release:

```
failure  epic/2.1.0                  2026-08-19
failure  UpdateCalcFFEM              2026-08-11
failure  fixDoc                      2026-06-08
failure  renameVignettes2            2026-06-01
```

This is the package's only real code check, so merges into `main` currently
carry no automated verification at all.

Diagnosed on run `34234687979`. Note `gh run view --log` truncates before the
failing step; the complete logs come from the zip endpoint:
`gh api repos/<owner>/<repo>/actions/runs/<id>/logs > logs.zip`.

Three independent failures, best fixed as separate PRs **in this order** (the
styler pass reformats everything, so it must not be tangled with real edits):

1. **`check-r-package` — `2 ERRORs, 5 WARNINGs, 6 NOTEs`.** Both ERRORs are one
   line: the `calcFFEM` example uses `%>%` with nothing exporting it —
   `dfPhi <- getPhi(phiFile) %>% select(starts_with("ETA"))` →
   `could not find function "%>%"`. Add `library(dplyr)` to the example (or use
   `|>`). Cheap WARNINGs worth taking at the same time: `::` imports not
   declared (`PhRame`, `tidyr`); `readr` in Imports but unused; missing Rd links
   (`getFileName`, apparently a typo for `getFileNames`, and `add_stamp` /
   `ggplot2::ggsave` in `plotExplainedVar.Rd`); codoc mismatches where the man
   pages are stale against `R/` (`getExplainedVar` gained `missVal` in code but
   not in docs).
2. **`format-check` — styler wants to reformat 93 files.** 43 `R/` files, ~38
   test files, 8 vignettes, `README.Rmd`, even `SimNeb/bs31.dir/.Rprofile`. The
   diffs are trivial whitespace (trailing spaces in roxygen), i.e. the check was
   added after the code was written and never run. One `styler::style_pkg()`
   commit fixes it, but it must be its own PR or it buries everything else.
3. **`unit-test` — 4 snapshot failures** (`FAIL 4 | WARN 0 | SKIP 1 | PASS 604`),
   not crashes: `test-fremParameterTable.R:117,137` (RSE %, e.g. 1.67→1.30,
   45.0→43.8) and `test-getForestDFFREM.R:47,72` (POINT / quantiles differing in
   the 3rd-4th significant figure), all under variant `4.2.2`. Both areas are
   driven by `PMXForest::getSamples()` bootstrap sampling. **Cause not
   determined** — see T17.

   Do not re-record these until the cause is known: accepting whatever the
   current environment produces is how a wrong value gets cemented. The
   `Eta_prim` snapshots in `test-calcFFEM.R` did exactly that, defending a
   vector with the subject `ID` in it (fixed in PR #72).

**Dependency resolution (separate, and it gates item 3).** `ci.yml:128` installs
`pharmetheus/PMXForest` - the *public* repo's default branch. Anything depending
on unreleased PMXForest work (e.g. `oneHotEncode()`, on `epic/v1.3.0` /
1.2.15.9007) will fail there once `epic/2.1.1` opens a PR into `main`. Same
class of problem as `pkgdown.yml`, which reads `rpkgs.pmx.one/r4.2-*/latest` and
so pulled PMXForest 1.2.15 rather than the 1.2.15.9007 published to the
*development* source.

Note `ci.yml` only triggers on `pull_request` into `main`, so PRs into an epic
branch are checked by `build-pkgdown` alone.

Related, found while reading the same code: `plotExplainedVar()` calls
`PhRame::add_stamp()` (`R/plotExplainedVar.R`) on its `add.stamp = TRUE` path,
and `PhRame` is neither in `Imports` nor publicly installable - so that path can
only error. Same category as the `save.script` block removed in v1.2.11.

## T17 — establish the cause of the snapshot drift, then choose a policy

Blocks item 3 of T15. Two candidate causes, not yet distinguished:

1. **Dependency version.** CI installs `pharmetheus/PMXForest` (public default
   branch); locally we run `epic/v1.3.0`. *Weaker than it first looked*: the
   `getSamples()` change in the dev line is the **SIR `raw_results` fix**, which
   only alters the SIR code path — and both failing tests pass a *bootstrap*
   file (`bs31.dir/raw_results_run31.csv`), which takes the other branch. So
   this may well not be it.
2. **Environment.** Hardware / BLAS / library versions producing numerical
   differences that `variant = r_version_variant` does not capture, since that
   only keys on the R version.

**The experiment that settles it** (deliberately not run yet — it replaces the
installed PMXForest, so do it when convenient): install the *public* PMXForest,
re-run `test-fremParameterTable.R` and `test-getForestDFFREM.R`, and see whether
the values move. If they do, it is the dependency and the fix is pinning /
publishing, not touching snapshots. If they do not, it is the environment.

**Then pick a policy.** Options, in the order I would prefer them:

- **Assert with tolerance instead of snapshotting.** `expect_equal(...,
  tolerance = 1e-3)` on the actual numbers. Snapshots demand bit-identical
  output for quantities that are inherently approximate; a tolerance is robust
  across machines *and* still catches a real regression. Best fit for these four.
- **Pin the dependency**, if the experiment says that is the cause.
- **`skip` these tests off the reference environment** rather than letting them
  fail and be ignored — an explicit skip shows in the test output; an ignored
  failure trains everyone to disregard red.

**Rejected:** "let CI generate the reference snapshots and ignore local
failures". It removes the local signal entirely for these functions — and with
`ci.yml` red for months, that means no signal at all — and routinely accepting
whatever the reference environment emits is precisely how the `Eta_prim` bug
survived.

## T18 — rename `additionalCovs` to `conditionalCovs` in `setupDfCovsEV()`

PMXForest renamed its `additionalCovs` argument to `conditionalCovs` for 1.3.0,
because the name did not say what the argument is for: these are the covariates
the *other* covariates are conditioned on - fed state rather than fasted,
patients rather than healthy volunteers - and they sit at their reference on
every row that is not about them.

`PMXFrem::setupDfCovsEV()` has an argument of the same old name. It is its own
argument and is **not** forwarded to PMXForest, so nothing is broken today and
this is purely about the two packages reading the same way. Worth doing in the
same release as any other `setupDfCovsEV()` change rather than on its own.

Not a deprecation: check first whether `setupDfCovsEV()` has appeared in a
public PMXFrem release with that argument. If it has, forward the old name with
a warning rather than removing it outright; if it has not, rename cleanly, as
PMXForest did. An outdated call errors loudly on an unused argument either way,
so nothing fails silently.

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

## T20 — verifyFREMParamFunction()'s structural check no longer has a reference

PMXForest 1.3.0 refuses a FREM model outright: `createParamFunction()` stops
when `FREMTYPE` is in `$INPUT`, because a FREM model's `$PK` translates
faithfully while describing none of the covariates the model was built for, and
a plausible-looking wrong answer is worse than no answer.

`verifyFREMParamFunction()` is the one caller that wanted exactly that
translation. Its first check - "Structural match", comparing the FREM parameter
function at `covthetas = 0, etas = 0` against the SCM-style typical-value
function - has no reference to compare against any more, so it errors.

Options, roughly in order of preference:

- Build the comparison function from `PMXForest::nmParsePK()` directly. PMXFrem
  already uses that parser in `createFREMParamFunction()`
  (`R/createFREMParamFunction.R:199`), so the `$PK` tree is available without
  going through `createParamFunction()` at all, and the refusal does not apply
  to the parser.
- Drop the structural check and keep the other two (covariate splice, random-
  effect splice). Cheapest, but it is the check that catches a wrong `$PK`
  transliteration, which is the thing most worth catching.
- Ask PMXForest for an explicit opt-in argument. Least preferred: it reopens the
  door the refusal was added to close, for one internal caller.

Note the warning-muffling workaround committed on `fix/ethnic-label-order` was
reverted when the warning became an error - there is nothing to muffle.

Do this before PMXFrem 2.1.1 goes out, since PMXForest 1.3.0 ships first.

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

## T24 — `createFREMData()` needs to filter and sort the way NONMEM would

Carried over from a handover note that shipped in the package as a top-level
`README.txt` (now deleted — it was stale, and every file at the top level goes
out with the tarball and the public mirror).

`createFREMmodel()` can already generate a minimal model equivalent to PsN's.
`createFREMData()` still needs to be more careful with the data:

- apply the base model's `IGNORE` / `ACCEPT` statements before anything else.
  `utils-filter_data` implements this and is not wired in.
- drop subjects with no observations *before* computing the covariate means and
  variances — they contribute nothing and currently shift both.
- keep the original data set's sort order in the FREM data set.

The work was done on a `create_FREMmodel` branch off `refactor_updateFREMmodel3`.

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

## Done

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
