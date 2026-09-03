# PMXFrem — design backlog

Items parked for future consideration. Not a substitute for GitHub issues; move an
item there when it becomes active work.

## T1 — `createFREMParamFunction()` / `verifyFREMParamFunction()`  *(v1 delivered)*

Named `createFREMParamFunction()` / `verifyFREMParamFunction()` (not
`createParamFunction`) so there is no collision with `PMXForest::createParamFunction`.

- PMXForest PR #23 (merged): exported `nmParsePK()` / `nmDeparse()` / `nmFormatNum()`
  — the `$PK` parser front end.
- PMXFrem PR (this branch): `createFREMParamFunction(baseModel, parameters,
  numSkipOm, ...)` transliterates the base model's `$PK`, splicing
  `covthetas[k] + etas[numSkipOm + k]` into the exponent of each named parameter.
  `verifyFREMParamFunction()` checks structural match vs
  `PMXForest::createParamFunction()`, plus `covthetas` / `etas` scaling.

The `ETA()` reference is replaced in place regardless of what encloses it, so
`exp(mu + ETA)`, `TV * exp(ETA)`, `TV + ETA` etc. all work. A named parameter
with no `ETA()` in `$PK` (not a FREM covariate parameter) or more than one is an
error.

v2 / still open:
- `verifyFREMParamFunction()`'s `covthetas` / `etas` scaling checks assume the
  parameter is log-normal (`exp` scaling). An additive-eta or logit parameter
  emits correctly from `createFREMParamFunction()` but would fail those checks —
  make `verify` transform-aware, or scope it.
- Derived metrics (AUC, t½) — left to the user, as in PMXForest.
- Auto-derive `numSkipOm` from a FREM model via `fremModelInfo()` instead of the
  plain argument.
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

## T7 — update the vignettes for `fremModelInfo()`

The FREM vignettes still pass `numNonFREMThetas` / `numSkipOm` (and `covNames`)
by hand everywhere. Once the T2 rollout is done, rework the vignettes to show
the derived form (locate the model via `runno` / `modName` / `modDevDir`, or
call `fremModelInfo()` directly) and keep at most one explicit example for
reference.

---

## Done

- **T5** — direct `getCovNames(createFREMmodel() output)` test — PR #40 (merged).
- **T2 (first PR)** — `fremModelInfo()` + `getExplainedVar()` / `getForestDFFREM()`
  wiring — PR #41 (merged). Rollout to the rest tracked above under T2.
