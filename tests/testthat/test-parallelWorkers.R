## Fresh worker sessions (what doParallel starts on Windows) load packages by
## searching .libPaths(). A package the session loaded from a folder that is
## not on .libPaths() - PMXRenv's versioned library, library(lib.loc = ) -
## then resolves to a different copy on the workers. These tests reproduce that
## with a one-function package installed twice: an old version in a
## "qualified" library the workers see, and a newer one in a "versioned"
## folder only the session loads from.

## Install fremToyPkg `version` into library folder `lib`.
installToyPkg <- function(lib, version) {
  src <- file.path(withr::local_tempdir(), "fremToyPkg")
  dir.create(file.path(src, "R"), recursive = TRUE)
  writeLines(c(
    "Package: fremToyPkg", paste0("Version: ", version),
    "Title: Toy Package", "Description: A toy package for tests.",
    "Author: PMXFrem tests", "Maintainer: PMXFrem tests <noreply@example.com>",
    "License: GPL-3"
  ), file.path(src, "DESCRIPTION"))
  writeLines("export(toyVersion)", file.path(src, "NAMESPACE"))
  writeLines(sprintf('toyVersion <- function() "%s"', version), file.path(src, "R", "toy.R"))
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  out <- system2(file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", "--no-docs", "--no-test-load", paste0("--library=", shQuote(lib)), shQuote(src)),
    stdout = TRUE, stderr = TRUE
  )
  if (!file.exists(file.path(lib, "fremToyPkg", "DESCRIPTION"))) {
    stop("could not install fremToyPkg ", version, ":\n", paste(out, collapse = "\n"))
  }
  invisible(lib)
}

## The qualified/versioned setup: workers see `qualified` (1.0.0) through
## R_LIBS; the session loads 2.0.0 from `versioned`, which is not on
## .libPaths().
versionedSetup <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  qualified <- installToyPkg(file.path(root, "qualified"), "1.0.0")
  versioned <- installToyPkg(file.path(root, "versioned", "fremtoypkg", "2.0.0"), "2.0.0")
  withr::local_envvar(R_LIBS = qualified, .local_envir = env)
  if (isNamespaceLoaded("fremToyPkg")) unloadNamespace("fremToyPkg")
  loadNamespace("fremToyPkg", lib.loc = versioned)
  withr::defer(if (isNamespaceLoaded("fremToyPkg")) unloadNamespace("fremToyPkg"), envir = env)
  list(qualified = qualified, versioned = versioned)
}

test_that("fresh workers load a package from the folder the session loaded it from", {
  skip_on_cran()
  paths <- versionedSetup()
  expect_equal(fremToyPkg::toyVersion(), "2.0.0")
  expect_false(any(startsWith(paths$versioned, .libPaths())))

  ## The premise: an ordinary fresh worker finds the qualified copy.
  cl <- parallel::makePSOCKcluster(1)
  plain <- parallel::clusterEvalQ(cl, fremToyPkg::toyVersion())[[1]]
  parallel::stopCluster(cl)
  expect_equal(plain, "1.0.0")

  ## With .fremStartWorkers() they get the session's copy.
  stopWorkers <- .fremStartWorkers(2, pkgs = "fremToyPkg", fresh = TRUE)
  withr::defer(stopWorkers())
  got <- foreach::foreach(i = 1:2, .packages = "fremToyPkg") %dopar% fremToyPkg::toyVersion()
  expect_equal(unlist(got), c("2.0.0", "2.0.0"))
})

test_that("setting up the workers does not itself load PMXFrem there", {
  skip_on_cran()
  ## Were the setup function to carry PMXFrem's namespace with it, each worker
  ## would load PMXFrem from its own library path before the setup could put
  ## the session's folders first - the very mismatch this exists to prevent.
  paths <- versionedSetup()
  stopWorkers <- .fremStartWorkers(1, pkgs = "fremToyPkg", fresh = TRUE)
  withr::defer(stopWorkers())
  loaded <- parallel::clusterEvalQ(attr(stopWorkers, "cluster"), loadedNamespaces())[[1]]
  expect_true("fremToyPkg" %in% loaded)
  expect_false("PMXFrem" %in% loaded)
})

test_that("a worker that cannot load the session's version stops with both versions named", {
  skip_on_cran()
  paths <- versionedSetup()

  ## The session keeps 2.0.0 in memory, but the folder it came from now holds
  ## 1.5.0 - what happens when a library is updated under a running session.
  ## R reads a function body from disk on first use, so use it before the
  ## files change underneath it.
  expect_equal(fremToyPkg::toyVersion(), "2.0.0")
  unlink(file.path(paths$versioned, "fremToyPkg"), recursive = TRUE)
  installToyPkg(paths$versioned, "1.5.0")
  expect_equal(fremToyPkg::toyVersion(), "2.0.0")

  err <- expect_error(
    .fremStartWorkers(2, pkgs = "fremToyPkg", fresh = TRUE),
    "fremToyPkg"
  )
  expect_match(conditionMessage(err), "2.0.0", fixed = TRUE)
  expect_match(conditionMessage(err), "1.5.0", fixed = TRUE)
  expect_match(conditionMessage(err), "ncores = 1", fixed = TRUE)
})

test_that("a package the workers cannot load at all is reported as such", {
  skip_on_cran()
  expect_error(
    .fremStartWorkers(1, pkgs = "fremNoSuchPkg", fresh = TRUE),
    "fremNoSuchPkg"
  )
})

test_that("forked workers are registered as before", {
  skip_on_os("windows")
  stopWorkers <- .fremStartWorkers(2, fresh = FALSE)
  withr::defer(stopWorkers())
  expect_equal(foreach::getDoParName(), "doParallelMC")
  expect_equal(foreach::getDoParWorkers(), 2)
})

test_that("getExplainedVar gives the same result on fresh workers as on one core", {
  skip_on_cran()
  ## Fresh workers load the installed PMXFrem, so the comparison is only
  ## meaningful when this session runs an installed copy - as under R CMD
  ## check - and not the source tree through load_all().
  skip_if_not(
    file.exists(file.path(getNamespaceInfo("PMXFrem", "path"), "Meta", "package.rds")),
    "PMXFrem is not loaded from an installed library"
  )
  modDevDir <- system.file("extdata/SimNeb", package = "PMXFrem")
  dfData <- read.csv(file.path(modDevDir, "DAT-2-MI-PMX-2-onlyTYPE2-new.csv"))
  dfData <- dfData[dfData$BLQ == 0 & !duplicated(dfData$ID), ]
  dfCovs <- setupDfCovsEV(file.path(modDevDir, "run31.mod"))
  fl <- list(function(basethetas, covthetas, dfrow, etas, ...) basethetas[2] * exp(covthetas[1] + etas[3]))
  ev <- function(ncores) {
    getExplainedVar(
      type = 1, data = dfData, dfCovs = dfCovs, numNonFREMThetas = 7, numSkipOm = 2,
      functionList = fl, functionListName = "CL", cstrCovariates = c("All", names(dfCovs)),
      modDevDir = modDevDir, runno = 31, ncores = ncores, quiet = TRUE, seed = 123
    )
  }
  withr::local_options(PMXFrem.freshWorkers = TRUE)
  expect_equal(ev(2), ev(1))
})
