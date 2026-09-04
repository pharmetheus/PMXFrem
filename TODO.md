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

v2 / still open:
- `verifyFREMParamFunction()`'s `covthetas` / `etas` scaling checks assume the
  parameter is log-normal (`exp` scaling). An additive-eta or logit parameter
  emits correctly from `createFREMParamFunction()` but would fail those checks —
  make `verify` transform-aware, or scope it.
- Derived metrics (AUC, t½) — left to the user, as in PMXForest.
- A NONMEM `$TABLE`-based check in `verifyFREMParamFunction()` once a FREM model
  fixture that tables `CL`/`V`/... exists (none in `inst/extdata` today).

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

Still to do:
- `updateFREMmodel()`, `createFREMmodel()` — model *mutation* on a FREM / base
  model. Both already parse the omega structure (`parseBaseModel()` /
  `initializeModelParameters()`); the right fix is to use that parse to fill
  `numSkipOm` / `numNonFREMThetas`, not `fremModelInfo()` (`createFREMmodel()`
  starts from a *base* model with no `;;;FREM CODE` markers and no covariate
  thetas yet).
- Not applicable: `plotCovDist()`, `plotEtasCov()` (no such args),
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

## T6 — remove `chore/session-handoff` (PMXForest-private)

Branch + `SESSION-HANDOFF.md`, once its context is fully absorbed.

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
