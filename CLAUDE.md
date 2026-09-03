# PMXFrem — notes for Claude

Support functions for FREM (Full Random Effects Model) analyses in NONMEM:
pre-/post-processing, FREM/FFEM model generation, forest plots, explained-variability plots.

## FREM/FFEM index conventions are load-bearing — verify, don't infer

`numNonFREMThetas`, `numFREMThetas`, `numSkipOm`, `numParCov` drive all eta/theta
indexing in `getForestDFFREM()`, `getExplainedVar()`, `calcFFEM()`. A wrong index
produces plausible-but-wrong numbers, not an error.

- `calcFFEM()$FullVars` is `(numSkipOm + numParCov)` square, ordered
  `[skip etas … , structural-param etas …]` — see `R/calcFFEM.R:236-244`
  (`FULLVARS`). The FREM covariate latent variables are **not** in it; they are
  projected into `Coefficients` / `Expr`, and `COEFF_VAR` (the FFEM residual
  parameter Ω) is the bottom-right block.
- `getExplainedVar()` passes the `etas` argument to the `functionList` functions
  with that **same layout in every type (0–3)**. Parameter *k*'s structural
  random effect is `etas[numSkipOm + k]`. Confirmed by instrumenting the runtime,
  not just reading call sites:
  - type 0 (`.calc_fo_variance`): `etas = rep(0, numSkipOm + numParCov)`,
    perturbed element-wise by `numDeriv::grad`; `covmatrix = FullVars`.
  - type 1: `etas` = phi-file rows, `dfPhi[, 3:(2 + numParCov + numSkipOm)]`.
  - type 2/3: `etas` = `t(ETAsamples) %*% chol(FullVars)`,
    `ETAsamples` is `(numParCov + numSkipOm) × numETASamples`.
  - eta-zero reference calls pass `rep(0, 3 * numNonFREMThetas)` — longer, all
    zero, harmless; don't assume `length(etas) == numSkipOm + numParCov`.
- One `functionList` function therefore serves both use cases: call with
  `etas = 0` for forest plots (`getForestDFFREM`), `etas != 0` for
  explained-variability plots (`getExplainedVar`).
- `getForestDFFREM()` / `getForestDFSCM()` (PMXForest) `functionList` signature is
  `function(basethetas, covthetas, dfrow, ...)` — `covthetas[k]` is the
  FFEM-projected covariate coefficient for parameter *k*, already computed by
  `calcFFEM()`. This is **not** the same as a PMXForest SCM `paramFunction`
  (`function(thetas, df, ...)`, all thetas, covariate model written out in the
  function body).
- `getExt()` sanitises `OMEGA(1,1)` → `OMEGA.1.1.`, `SIGMA(1,1)` → `SIGMA.1.1.`.

When reasoning about any of these, read `R/calcFFEM.R` / run `dim()` / `str()` —
do not infer shapes or offsets from surrounding call sites.

## Testing conventions

- `Config/testthat/edition: 3`. Run with `NOT_CRAN=true` to exercise the
  `skip_on_cran()` blocks.
- **No `vdiffr`.** Plot tests assert on data, not rendered pixels: extract
  `ggplot2::ggplot_build(p)$data`, keep version-independent columns, pass through
  `standardize_plot_data()` + `stabilize()` (see `tests/testthat/helper-stabilize.R`),
  then either `expect_snapshot_value(style = "serialize")` or direct `expect_*`
  assertions. `test-tracePlot.R` uses direct assertions + an independent
  recomputation from the fixture; `test-plotExplainedVar.R` uses serialized
  snapshots.
- Serialized-value snapshots are per-R-version: `tests/testthat/_snaps/<major.minor>/`.
  Committed variants are `4.2.2` and `4.4.2`. Running on another R version
  creates a new `_snaps/<ver>/` dir — do not commit it.

## Branching

- Development happens on `epic/<version>` branches (currently `epic/2.1.1`).
  Feature/fix branches PR **into the epic branch**, not `main`.
- The remote has a branch literally named `test`, so `test/<anything>` is an
  invalid ref name for a new branch (`directory file conflict` on push). Use
  `fix/…`, `feature/…`, etc.
