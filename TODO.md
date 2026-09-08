# PMXFrem — design backlog

Items parked for future consideration. Not a substitute for GitHub issues; move an
item there when it becomes active work.

## T1 — `createFREMParamFunction()` / `verifyFREMParamFunction()`  *(v1 delivered)*

Named `createFREMParamFunction()` / `verifyFREMParamFunction()` (not
`createParamFunction`) so there is no collision with `PMXForest::createParamFunction`.

- PMXForest PR #23 (merged): exported `nmParsePK()` / `nmDeparse()` / `nmFormatNum()`
  — the `$PK` parser front end.
- PMXFrem PR #45 (this branch): `createFREMParamFunction(fremModel, parameters,
  ...)` (or `runno` / `modName` / `modDevDir`) transliterates **the FREM model's**
  `$PK`, deriving `numSkipOm` / `numNonFREMThetas` via `fremModelInfo()`. The
  single `ETA()` of a FREM covariate parameter is replaced in place (whatever
  encloses it) by `covthetas[k] + etas[numSkipOm + k]`. A parameter with no
  `ETA()` is returned as-is (no covariate effect); more than one -> as-is + a
  warning. The body is pruned to the transitive dependencies of `parameters`, so
  the FREM covariate block drops and `basethetas` is the first
  `numNonFREMThetas`. `verifyFREMParamFunction()` checks the structural part
  against `PMXForest::createParamFunction()` on the same FREM model, plus the
  `covthetas` / `etas` scaling.

v2:
- **Done** — non-log-normal parameters. `createFREMParamFunction()` records
  `fremEtaScale` (`"exp"` vs `"other"`); `verifyFREMParamFunction()` reports
  `COVSPLICE` / `ETASPLICE` / `PASS` as `NA` for a non-log-normal parameter
  (structural check still runs). PR #53.
- **Won't do** — a NONMEM `$TABLE`-based check. No bundled FREM model tables
  `CL` / `V` / ...; not worth adding a fixture for it.

## T2 — roll `fremModelInfo()` out to the remaining entry points

Done so far:
- PR #41 — `fremModelInfo()` + `getExplainedVar()` / `getForestDFFREM()`
  (the latter via `runno` / `modName` / `modDevDir`).
- PR #43 — `@examples` show both syntaxes.
- PR #44 — `fremParameterTable()`, `createFFEMmodel()`, `createFFEMdata()`,
  `calcEtas()` (+ derived-syntax `@examples`).

Decided against:
- `calcFFEM()` — **leave as is.** It has no notion of a model, is a low-level
  workhorse with little direct user exposure, and every caller already resolves
  `numNonFREMThetas` / `numSkipOm` before calling it. Wiring it in would need a
  new model-file argument for marginal benefit. Revisit only if a concrete need
  appears.

Done:
- `updateFREMmodel()` / `createFREMmodel()` (PR #54). `updateFREMmodel()`
  derives `numNonFREMThetas` / `numSkipOm` from the FREM model + `.ext` via
  `fremModelInfo()`, and **stops loudly** when the `;;;FREM CODE` markers /
  `$OMEGA BLOCK(N)` disagree with the `.ext` (explicit values override).
  `createFREMmodel()` reads `numNonFREMThetas` from the base model's `.ext`;
  `numSkipOm` stays a required input (a base model has no FREM structure to
  deduce it from) and is passed straight through to `updateFREMmodel()`.

Not applicable:
- `plotCovDist()`, `plotEtasCov()` (no such args),
  `calcParameterEsts()` (low-level; its caller `fremParameterTable()` resolves),
  `removeFremCovariates()` (operates on an already-resolved `currentState`),
  `initializeModel()` / `initializeModelParameters()` (internal; fed resolved
  values by callers).

Derivation (verified on `run31` → 7/2/3 and `run22-3` → 13/2/4):

```
numFREMThetas    = length(getCovNames(modFile)$covNames)
numNonFREMThetas = (#THETA columns in ext) - numFREMThetas
numTotEta        = solve k(k+1)/2 = (#OMEGA columns in ext)
blockN           = N in the final "$OMEGA BLOCK(N)" of modFile
numParCov        = blockN - numFREMThetas
numSkipOm        = numTotEta - blockN        # robust to a structural BLOCK before the FREM block
numSigmas        = #SIGMA columns in ext
```

## T3 — helper: add an IIV eta to an established FREM model

The FREM `$OMEGA BLOCK(N)` must stay the trailing omega structure `[skip | par |
cov]`. A new IIV eta can only go into the skip region (before the block), which
increments `numSkipOm` and shifts every `ETA(k)` / `MU_k` index from the insertion
point on (`$PK`, `$ERROR`, and the `COV_k = MU_k + ETA(k)` block). Helper: insert
the `$OMEGA` record + run a systematic ETA/MU renumber pass. The covariate-add
direction of this machinery already exists in `updateFREMmodel()` /
`generateFremModel()`.

## T4 — helper: add a structural `$THETA` to an established FREM model

`calcFFEM()` assumes structural thetas `1..numNonFREMThetas` then FREM covariate
means contiguously. A new structural theta must be inserted at position
`<= numNonFREMThetas + 1`; that shifts every `MU_j = THETA(numNonFREMThetas + j)`
reference in `$PK` and increments `numNonFREMThetas`. Helper: insert + renumber the
`MU_j = THETA(...)` refs.

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

## T11 — test coverage audit

Run `covr::package_coverage()` (both PMXFrem and PMXForest). If overall package
coverage is **< 95%**, add tests to bring it up: prioritise exported functions
and the branches most likely to regress (error paths, the derived-vs-explicit
`fremModelInfo()` paths, the `secondary` / `verify` edge cases). Record the
before/after number.

**PMXForest: 97.43%** (2026-09-04) — above threshold, no new tests needed.
The `covr` run exercises branches `devtools::test()` never reached in this
whole development effort, and surfaced two real bugs along the way (both list-
indexing bugs: `x[["name-not-present"]]` and `x[""]`, both throw/misbehave
instead of returning nothing) — fixed, PMXForest PR #31 (open for review).
PMXFrem coverage not yet run.

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

## T13 — `getForestDF*` result assembly (backend-neutral, do first)

The three `getForestDF*` functions build the result with a per-cell
`data.frame()` + `dfres <- bind_rows(dfres, ...)` in a loop, at both the inner
(`internalCalc`) and outer (combine) level. Rewrite to accumulate atomic
vectors (or per-cell lists) and build the data frame **once**
(`data.table::rbindlist` / a single `data.frame()`), as
`utils-getExplainedVar.R` already does.

Measured on a representative shape (300 parameter rows x 20 covariate rows x 2
functions x 3 params -> ~36k result rows): **10.7 s -> 0.13-0.19 s, ~50-80x**
for the assembly step. Only the assembly speeds up - if `functionList` is
expensive (e.g. an `mrgsolve` sim per cell) total time barely moves - but for
closed-form parameter functions it is most of the sequential runtime, and it
also shrinks the payload returned from parallel workers. No dependency or API
change; low risk.

## T14 — Windows / PSOCK robustness for the `getForestDF*` family (interim)

PMXFrem 2.0.0 fixed `getExplainedVar()`: `.export = c(ls(environment()), ...)`
to bundle the local environment for `foreach` PSOCK workers (Windows "object
not found" crashes), and `on.exit(doParallel::stopImplicitCluster(), add =
TRUE)` for teardown on error. `getForestDFSCM()` / `getForestDFemp()`
(PMXForest) and `getForestDFFREM()` (PMXFrem) still use bare
`.export = cstrExports` (default `NULL`, relying on `foreach`'s shallow static
analysis of the `internalCalc` closure) and a bare `stopImplicitCluster()` at
the end (leaks the cluster if the function errors). `createFFEMdata()` is worse
- `foreach(k = ...) %dopar% { ... }` with no `.export` / `.packages` at all.

Apply the same two fixes to all four. This is an **interim** measure: T12
removes the need for it entirely (`future` detects globals/packages and manages
the backend). Do T14 only if T12 is not going to land soon.

---

## Done

- **T5** — direct `getCovNames(createFREMmodel() output)` test — PR #40 (merged).
- **T2 (first PR)** — `fremModelInfo()` + `getExplainedVar()` / `getForestDFFREM()`
  wiring — PR #41 (merged). Rollout to the rest tracked above under T2.
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
