# The visual content of the traceplots used to be guarded with vdiffr
# (`expect_doppelganger`). Those snapshots compared rendered SVG and broke on
# every svglite / freetype / OS difference without any change in the plotted
# data. They are replaced here by data-level checks: the structure of the
# returned ggplot objects plus an independent recomputation of the values that
# each panel must contain. Nothing below renders an image, so there is no
# environment sensitivity to regress on.

test_that("traceplot returns correctly structured OFV / Theta / Omega ggplot objects", {

  md <- system.file("extdata", "SimNeb/", package = "PMXFrem")

  # ---- return contract for the include* switches ----
  retList3 <- traceplot(30, modDevDir = md)
  expect_type(retList3, "list")
  expect_named(retList3, c("OFV", "Theta", "Omegas"))
  expect_true(all(vapply(retList3, ggplot2::is.ggplot, logical(1))))

  retList2 <- traceplot(30, modDevDir = md, includeOFV = FALSE)
  expect_named(retList2, c("Theta", "Omegas"))

  retList1 <- traceplot(30, modDevDir = md, includeOmega = FALSE, includeOFV = FALSE)
  expect_named(retList1, "Theta")

  # ---- independent recomputation of what the panels should contain ----
  # traceplot() normalises every THETA / OMEGA column by its final value but
  # leaves OBJ on the raw scale; startIter defaults to 10.
  myext  <- getExt(extFile = file.path(md, "run30.ext"), set = 1)
  finpar <- myext[myext$ITERATION == -1000000000, ]
  iters  <- myext[myext$ITERATION > 10, ]
  iters  <- iters[order(iters$ITERATION), ]
  normed <- function(col) iters[[col]] / as.numeric(finpar[[col]])

  # ---- OFV panel ----
  expect_identical(
    vapply(retList3$OFV$layers, function(l) class(l$geom)[1], character(1)),
    c("GeomRect", "GeomLine", "GeomPoint")            # rect = shaded acceptance region
  )
  ofvBuild <- ggplot2::ggplot_build(retList3$OFV)
  ofvLine  <- ofvBuild$data[[2]]
  ofvLine  <- ofvLine[order(ofvLine$x), ]
  expect_equal(nrow(ofvLine), nrow(iters))
  expect_true(all(ofvLine$x > 10))
  expect_equal(ofvLine$y, iters$OBJ, tolerance = 1e-6)   # OFV stays on the raw scale
  ofvRect <- ofvBuild$data[[1]]
  expect_equal(nrow(ofvRect), 1L)
  expect_lt(ofvRect$ymin, ofvRect$ymax)

  # ---- Theta panels ----
  expect_identical(
    vapply(retList3$Theta$layers, function(l) class(l$geom)[1], character(1)),
    c("GeomLine", "GeomHline")
  )
  thetaBuild  <- ggplot2::ggplot_build(retList3$Theta)
  thetaParams <- as.character(thetaBuild$layout$layout$Parameter)
  expect_setequal(thetaParams, paste0("THETA", 1:7))

  thetaHline <- thetaBuild$data[[2]]
  expect_equal(nrow(thetaHline), length(thetaParams))   # one reference line per panel
  expect_true(all(thetaHline$yintercept == 1))          # normalised -> final value sits at 1

  thetaLine <- thetaBuild$data[[1]]
  expect_equal(nrow(thetaLine), length(thetaParams) * nrow(iters))
  expect_true(all(thetaLine$x > 10))
  expect_length(unique(thetaLine$colour), length(thetaParams))
  # numeric check: the THETA1 panel must be THETA1 normalised by its final value
  theta1Panel <- thetaBuild$layout$layout$PANEL[thetaBuild$layout$layout$Parameter == "THETA1"]
  expect_equal(sort(thetaLine$y[thetaLine$PANEL == theta1Panel]),
               sort(normed("THETA1")), tolerance = 1e-6)

  # ---- Omega panels ----
  expect_identical(
    vapply(retList3$Omegas$layers, function(l) class(l$geom)[1], character(1)),
    "GeomLine"
  )
  omegaBuild  <- ggplot2::ggplot_build(retList3$Omegas)
  omegaParams <- as.character(omegaBuild$layout$layout$Parameter)
  expect_true(length(omegaParams) > 0)
  expect_true(all(grepl("^OMEGA\\.[0-9]+\\.[0-9]+\\.$", omegaParams)))
  omegaLine <- omegaBuild$data[[1]]
  expect_true(all(omegaLine$x > 10))
  expect_true(all(is.finite(omegaLine$y)))
})

test_that("traceplot thetaNum selects only the requested THETA panels", {

  md <- system.file("extdata", "SimNeb/", package = "PMXFrem")

  retListX <- traceplot(30, modDevDir = md, thetaNum = c(2, 3),
                        includeOFV = FALSE, includeOmega = FALSE)
  expect_named(retListX, "Theta")
  expect_setequal(
    as.character(ggplot2::ggplot_build(retListX$Theta)$layout$layout$Parameter),
    c("THETA2", "THETA3")
  )
})


test_that("traceplot covers remaining logic branches", {

  # --- Setup: Create temporary .ext files for specific test cases ---
  temp_dir <- file.path(tempdir(), "test-traceplot-comprehensive")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

  create_file <- function(path, content) {
    writeLines(text = content, con = path)
  }

  # Case 1: An ext file with a column of all zeros to test the `myTrash` block
  content_zeros <- c(
    "TABLE NO. 1",
    "ITERATION THETA1 ZERO_COL OMEGA.1.1. OBJ",
    "0 1 0 0.1 100",
    "1 1.1 0 0.11 90",
    "11 1.15 0 0.115 85",
    "-1000000000 1.2 0 0.12 80"
  )
  file_with_zeros <- file.path(temp_dir, "zeros.ext")
  create_file(file_with_zeros, content_zeros)

  # Case 2: A multi-table file to test set = "last"
  content_multi <- c(
    "TABLE NO. 1", "ITERATION OBJ", "-1000000000 100",
    "TABLE NO. 2", "ITERATION OBJ", "0 55", "11 54", "-1000000000 50"
  )
  file_multi_table <- file.path(temp_dir, "multi.ext")
  create_file(file_multi_table, content_multi)


  # --- Tests ---

  # Test for the `myTrash` block
  res_zeros <- traceplot(extFileName = file_with_zeros, includeOFV = FALSE, includeOmega = FALSE)
  # Check that ZERO_COL was removed from the data used for plotting
  expect_false("ZERO_COL" %in% names(res_zeros$Theta$data))


  # Test for the `omegaNum` argument
  res_omegaNum <- traceplot(runno = 30, modDevDir = system.file("extdata", "SimNeb/", package="PMXFrem"),
                            omegaNum = 1, includeOFV = FALSE, includeTheta = FALSE)
  plot_build_omega <- ggplot2::ggplot_build(res_omegaNum$Omegas)
  expect_equal(as.character(plot_build_omega$layout$layout$Parameter), "OMEGA.1.1.")

})

# tests/testthat/test-traceplot.R

# Helper function to check for a specific geom layer in a ggplot object
has_geom <- function(plot, geom_name) {
  if (!inherits(plot, "ggplot")) {
    return(FALSE)
  }
  layers <- sapply(plot$layers, function(l) class(l$geom)[1])
  return(geom_name %in% layers)
}

test_that("traceplot OFV shaping works as expected", {
  model_dir <- system.file("extdata/SimNeb/", package = "PMXFrem")
  
  # --- Test 1: Shaded region is added by default ---
  plots_default <- traceplot(runno = 31, modDevDir = model_dir)
  
  # The OFV plot should exist and contain a "GeomRect" layer for the shading
  expect_true("OFV" %in% names(plots_default))
  expect_true(has_geom(plots_default$OFV, "GeomRect"))
  
  
  # --- Test 2: Shaded region can be disabled ---
  plots_disabled <- traceplot(runno = 31, modDevDir = model_dir, includeShapedOFV = FALSE)
  
  # The OFV plot should NOT contain a "GeomRect" layer
  expect_true("OFV" %in% names(plots_disabled))
  expect_false(has_geom(plots_disabled$OFV, "GeomRect"))
})


test_that("traceplot OFV shaping arguments throw correct errors", {
  model_dir <- system.file("extdata/SimNeb/", package = "PMXFrem")
  
  # --- Test 3: Error for invalid p-value ---
  expect_error(
    traceplot(runno = 31, modDevDir = model_dir, pvalue = -0.1),
    regexp = "p-value" # Check for the relevant part of the error message
  )
  expect_error(
    traceplot(runno = 31, modDevDir = model_dir, pvalue = 1.1),
    regexp = "p-value"
  )
  
  # --- Test 4: Error for invalid degrees of freedom ---
  expect_error(
    traceplot(runno = 31, modDevDir = model_dir, df = -1),
    regexp = "degreess of freedom"
  )
  
  # --- Test 5: Error for invalid number of iterations for mean calculation ---
  expect_error(
    traceplot(runno = 31, modDevDir = model_dir, meanShapeLastIter = 0),
    regexp = "last iterations used for the mean"
  )
})