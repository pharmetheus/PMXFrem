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

## T2 — auto-derive `numNonFREMThetas` / `numSkipOm`  *(in progress)*

Internal `fremModelInfo(modFile, dfext)` resolver. Derivation (verified on `run31`):

```
numFREMThetas    = length(getCovNames(modFile)$covNames)
totalThetas      = #THETA columns in dfext
numNonFREMThetas = totalThetas - numFREMThetas
numTotEta        = solve k(k+1)/2 = #OMEGA columns in dfext
blockN           = N in the final "$OMEGA BLOCK(N)" of modFile
numParCov        = blockN - numFREMThetas
numSkipOm        = numTotEta - blockN        # more robust than token-counting $OMEGA lines
numSigmas        = #SIGMA columns in dfext
```

Scope for the first PR: resolver + wire into `getExplainedVar()` and
`getForestDFFREM()` only (adds optional `modFile` to `getForestDFFREM()`). Args
become optional (`NULL` -> derive); when supplied they are an override, and a
mismatch with the derived value **warns and keeps the explicit value**. Follow-up
PR rolls the pattern out to `calcFFEM`, `fremParameterTable`, `createFFEMmodel`,
`createFFEMdata`, `calcEtas`, `calcParameterEsts`, etc.

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

## T5 — direct `getCovNames(createFREMmodel() output)` test  *(next)*

`createFREMmodel()` reaches its final model through the
`updateFREMmodel()` / `generateFremModel()` path, so it inherits marker
compliance, but there is no test asserting `getCovNames()` round-trips its output.
`updateFREMmodel` and `createMinimalFremModel` already have such tests.

## T6 — remove `chore/session-handoff` (PMXForest-private)

Branch + `SESSION-HANDOFF.md`, once its context is fully absorbed.
