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

## Done

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
