# PMXFrem — design backlog

Items parked for future consideration. Not a substitute for GitHub issues; move an
item there when it becomes active work.

## T1 — `createParamFunction()` / `verifyParamFunction()` for PMXFrem

Generate an R function that mirrors what `createFFEMmodel()` writes as NONMEM: the
base model's structural `$PK`, with each covariate-associated parameter wrapped as
`P_k = TV_k(basethetas, structural covariates) * exp(covthetas[k] + etas[numSkipOm + k])`.
Takes `covthetas` (from `calcFFEM()`, per uncertainty sample) and `etas` as
arguments. One function serves both uses: `etas = 0` for `getForestDFFREM()` forest
plots, `etas != 0` for `getExplainedVar()` explained-variability plots (the eta
layout is uniform across EV types 0–3 — see `CLAUDE.md`).

Open design points:
- Reuse / extend PMXForest's `nmParsePK` for the structural `$PK` transcription, or a
  FREM-specific parser (FREM base models are usually MU-referenced).
- v1 = log-normal parameters only; v2 handles logit/additive transforms.
- Return the structural parameters only; derived metrics (AUC, t½) stay
  user-composed, as in PMXForest.
- Ship as the pair `createParamFunction()` + `verifyParamFunction()`.
- `etas` default = zero vector; index via `if (length(e) >= i) e[i] else 0` because
  the EV eta-zero reference calls pass `rep(0, 3 * numNonFREMThetas)`.
- Can build on `fremModelInfo()` (T2) for the structural integers.

## T2 — roll `fremModelInfo()` out to the remaining entry points

Done so far:
- PR #41 — `fremModelInfo()` + `getExplainedVar()` / `getForestDFFREM()`
  (the latter via `runno` / `modName` / `modDevDir`).
- PR #43 — `@examples` show both syntaxes.
- (next PR) — `fremParameterTable()`, `createFFEMmodel()`, `createFFEMdata()`,
  `calcEtas()`.

Still to do:
- `calcFFEM()` — the low-level workhorse. It takes `dfext` but no model file;
  needs an optional `modFile` / `runno` / `modName` / `modDevDir` added. Most
  callers already pass resolved counts, so lower value / higher blast radius —
  do last, on its own.
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
