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

`fremModelInfo()` and the wiring into `getExplainedVar()` / `getForestDFFREM()`
landed in PR #41 (merged). `getForestDFFREM()` takes `runno` / `modName` /
`modDevDir` (resolved via `getFileNames()`, like `getExplainedVar()`); the two
counts default to `NULL` and are derived, with an explicit disagreeing value
warned-and-kept.

Still to do — apply the same optional-arg + derive-or-validate pattern to:
`calcFFEM()`, `fremParameterTable()`, `createFFEMmodel()`, `createFFEMdata()`,
`calcEtas()`, `calcParameterEsts()`, `plotCovDist()`, `plotEtasCov()`,
`removeFremCovariates()`, `initializeModel()`, `updateFREMmodel()`,
`createFREMmodel()` (`numNonFREMThetas` only — no `.ext` yet at build time).

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
