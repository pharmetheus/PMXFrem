# PMXFrem — design backlog

Items parked for future consideration. Not a substitute for GitHub issues; move an
item there when it becomes active work.

## T3 — helper: add an IIV eta to an established FREM model

The FREM `$OMEGA BLOCK(N)` must stay the trailing omega structure `[skip | par |
cov]`. A new IIV eta can only go into the skip region (before the block), which
increments `numSkipOm` and shifts every `ETA(k)` / `MU_k` index from the insertion
point on (`$PK`, `$ERROR`, and the `COV_k = MU_k + ETA(k)` block). Helper: insert
the `$OMEGA` record + run a systematic ETA/MU renumber pass. The covariate-add
direction of this machinery already exists in `updateFREMmodel()` /
`generateFremModel()`.

**In review** — implemented as `addFremIIV()`, PMXFrem PR #62 (draft).

## T4 — helper: add a structural `$THETA` to an established FREM model

`calcFFEM()` assumes structural thetas `1..numNonFREMThetas` then FREM covariate
means contiguously. A new structural theta must be inserted at position
`<= numNonFREMThetas + 1`; that shifts every `MU_j = THETA(numNonFREMThetas + j)`
reference in `$PK` and increments `numNonFREMThetas`. Helper: insert + renumber the
`MU_j = THETA(...)` refs.

**In review** — implemented as `addFremStructuralTheta()`, PMXFrem PR #62
(draft), which calls `addFremIIV()` for the `addEta = TRUE` path.

## Deep-dive vignette — extending a FREM model with THETAs and IIVs

Blocked on T3 / T4 landing: it needs the final signatures and output shape to
document. Walk through adding a structural parameter with and without IIV,
what the renumber pass does to `ETA` / `MU_` / `COV` / `THETA` indices, and why
the `.ext` / `.phi` are not migrated (the model must be re-estimated).

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
carry no automated verification at all. Two things to sort out:

1. **Why it fails.** Not yet diagnosed; the GitHub log API truncates before the
   failing step, so it needs looking at in the web UI or by reproducing the job
   steps locally (`make test`, `styler`, `lintr@3.0.2`, `R CMD check`).
2. **Dependency resolution.** `ci.yml:128` installs `pharmetheus/PMXForest` -
   the *public* repo's default branch. Anything depending on unreleased
   PMXForest work (e.g. `oneHotEncode()`, on `epic/v1.3.0` / 1.2.15.9007) will
   fail there once `epic/2.1.1` opens a PR into `main`. Same class of problem as
   `pkgdown.yml`, which reads `rpkgs.pmx.one/r4.2-*/latest` and so pulled
   PMXForest 1.2.15 rather than the 1.2.15.9007 published to the *development*
   source.

Note `ci.yml` only triggers on `pull_request` into `main`, so PRs into an epic
branch are checked by `build-pkgdown` alone.

## T16 — `generateFremModel()` should not trust user-written comments

`generateFremModel()` computes **correct** `$THETA` / `$OMEGA` labels from
`basenames_th` / `basenames_om` / `covnames$covNames`
(`generateFremModel.R:63-72`, `85-94`) and then immediately **throws them away**,
overwriting each one with whatever comment text happens to sit on the
corresponding line of the input model (`73-83`, `96-106`). The re-emitted model
therefore inherits the user's comments verbatim, including any that are stale
or wrong.

The two override loops are positional, and they break in **opposite**
directions under ordinary formatting variation:

- **`$THETA` (76-83):** `idx` increments for **every line** of the record, so it
  assumes exactly one theta per line. A blank line, a standalone comment, or a
  multi-value record (`$THETA 1 2 3`) misaligns every label after it.
- **`$OMEGA` (96-106):** `idx` increments **only for lines containing `;`**, so
  it assumes every omega carries a comment and nothing else in the region does.
  A standalone note inside `$OMEGA` consumes a slot and shifts every later label
  down; an omega with no comment shifts them up.

Nothing parses these labels back (they are only pasted onto output lines at
`190` / `202`), so today this is **cosmetic** - but they are what a human reads
in the regenerated control stream, and they are wrong in a way that looks
authoritative. There is no warning.

Options:

1. Drop the override entirely and always emit the generated labels - they are
   already computed and are correct by construction.
2. Keep the override but only accept a trailing comment on a line that actually
   carries a parameter value, ignoring standalone comment lines and counting
   values rather than lines.
3. Leave as is and document the "one parameter per line, each commented"
   assumption.

Related: `addFremIIV()` / `addFremStructuralTheta()` (PR #62) insert a record in
the middle of the block and leave the following `; N.` numbers stale for exactly
this reason. Option 1 or 2 makes that self-correcting.

---

## Done

- **T5** — direct `getCovNames(createFREMmodel() output)` test — PR #40 (merged).
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
