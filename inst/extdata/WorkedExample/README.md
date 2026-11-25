# Worked example: a stepwise covariate search and FREM on one simulated data set

The files of the article "Worked example: a stepwise covariate search and FREM applied to one
simulated data set" on the PMXFrem website. The data are simulated, and the article reads the
NONMEM and PsN output below; nothing is estimated when the article is built.

- `dataset3-r2.csv`: the analysis data, 507 subjects in two studies. `LNWT` is log weight.
  Height is -99 (missing) in 133 subjects.
- `true-height.csv`: the height of every subject in the simulation, used only for the true
  effects of the simulation model.
- `truth-functions.R`: the simulation model in R, and the true covariate effects derived from it.
- `run200.ext`, `run201.ext`, `run202.ext`: the base model without covariates, with a diagonal
  covariance matrix, a covariance of CL and V, and a full block for CL, V and MAT.
- `scm200.dir/scmlog.txt`: the log of the stepwise covariate search (PsN scmplus).
- `run203.mod`, `run203.ext`, `goftab203`: the final model of the search, estimated with FOCE-I,
  and its goodness-of-fit table.
- `run205.ext`: the final model of the search with liver function (NCI grade) on V added.
- `run210.mod`, `run210.ext`: the FREM base model, with food status on the mean absorption time.
- `run211.mod`, `run211.ext`, `run211.phi`: the FREM model, estimated with IMPMAP (the .phi file
  holds the final step only).
- `run212.mod`, `goftab212`: the FFEM model of the FREM fit, run without estimation, and its
  goodness-of-fit table.
- `bs203n30.dir`, `bs211n30.dir`: the raw results of a bootstrap of 30 samples of run203 and of
  run211.
