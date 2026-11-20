## fremEqLatex() on the bundled run31 FREM model: 7 structural thetas, 2
## skipped omegas, 3 parameters with covariate effects (CL, V, MAT) and 18
## FREM covariates, among them the indicators RACEL_2, RACEL_3, NCIL_1, NCIL_2.

.eqDir <- system.file("extdata/SimNeb", package = "PMXFrem")
.eqExt <- getExt(file.path(.eqDir, "run31.ext"))
.eqCovs <- getCovNames(file.path(.eqDir, "run31.mod"))$covNames
.eqPars <- c("CL", "V", "MAT")

.eq <- function(...) {
  fremEqLatex(runno = 31, modDevDir = .eqDir, parNames = .eqPars, quiet = TRUE, ...)
}

## The number as fremEqLatex() writes it: `digits` significant digits, no sign.
.eqNum <- function(x, digits = 3) {
  trimws(formatC(signif(abs(x), digits), format = "fg", digits = digits))
}

test_that("fremEqLatex() gives one block of display math per parameter, named by parNames", {
  eqs <- .eq()
  expect_type(eqs, "character")
  expect_named(eqs, .eqPars)
  for (p in .eqPars) {
    expect_true(startsWith(
      eqs[[p]],
      paste0("\\[\n\\begin{aligned}\n\\text{COV}_{\\text{", p, "}} &= ")
    ))
    expect_true(endsWith(eqs[[p]], "\n\\end{aligned}\n\\]"))
  }
})

test_that("fremEqLatex() writes coefficient times covariate minus mean", {
  cl <- .eq()[["CL"]]
  ## CL: WT -0.00591 (mean 86.86), HT -0.0287 (mean 169.65), BSA 2.75 (2.01)
  expect_match(cl, "&= -0.00591\\,(\\text{WT} - 86.9) - 0.0287\\,(\\text{HT} - 170)", fixed = TRUE)
  expect_match(cl, " + 2.75\\,(\\text{BSA} - 2.01)", fixed = TRUE)
})

test_that("fremEqLatex() takes each term from the right row and column of calcFFEM()", {
  eqs <- .eq()
  ffem <- calcFFEM(.eqExt,
    numNonFREMThetas = 7, numSkipOm = 2, covNames = .eqCovs,
    parNames = .eqPars, quiet = TRUE
  )
  final <- .eqExt[.eqExt$ITERATION == -1000000000, ]
  means <- unlist(final[paste0("THETA", 7 + seq_along(.eqCovs))])
  for (p in seq_along(.eqPars)) {
    for (k in seq_along(.eqCovs)) {
      negative <- ffem$Coefficients[p, k] < 0
      sign <- if (k == 1) {
        paste0("&= ", if (negative) "-")
      } else {
        if (negative) " - " else " + "
      }
      term <- paste0(
        sign, .eqNum(ffem$Coefficients[p, k]), "\\,(\\text{", gsub("_", "}_{\\text{", .eqCovs[k], fixed = TRUE),
        if (grepl("_", .eqCovs[k])) "}}" else "}", " - ", .eqNum(means[k]), ")"
      )
      expect_match(eqs[[p]], term, fixed = TRUE, info = paste(.eqPars[p], .eqCovs[k]))
    }
  }
})

test_that("fremEqLatex() gives the same equations from explicit arguments", {
  explicit <- fremEqLatex(
    dfext = .eqExt, numNonFREMThetas = 7, numSkipOm = 2,
    covNames = .eqCovs, parNames = .eqPars, quiet = TRUE
  )
  expect_identical(explicit, .eq())
})

test_that("fremEqLatex() with availCov gives the FFEM with those covariates only", {
  wt <- .eq(availCov = "WT")
  ## The coefficient of WT changes sign once the other covariates are left out
  expect_match(wt[["CL"]], "&= 0.0056\\,(\\text{WT} - 86.9)\n", fixed = TRUE)
  expect_no_match(wt[["CL"]], "HT", fixed = TRUE)

  two <- .eq(availCov = c("AGE", "WT"))
  ffem <- calcFFEM(.eqExt,
    numNonFREMThetas = 7, numSkipOm = 2, covNames = .eqCovs,
    parNames = .eqPars, availCov = c("AGE", "WT"), quiet = TRUE
  )
  ## The terms follow the order of covNames, WT before AGE
  expect_match(two[["V"]], paste0(
    "&= ", if (ffem$Coefficients[2, 1] < 0) "-", .eqNum(ffem$Coefficients[2, 1]),
    "\\,(\\text{WT} - 86.9) ", if (ffem$Coefficients[2, 2] < 0) "-" else "+", " ",
    .eqNum(ffem$Coefficients[2, 2]), "\\,(\\text{AGE} - 43.8)\n"
  ), fixed = TRUE)
})

test_that("fremEqLatex() with an empty availCov gives an effect of 0", {
  eqs <- .eq(availCov = character(0))
  expect_match(eqs[["MAT"]], "\\text{COV}_{\\text{MAT}} &= 0\n", fixed = TRUE)
})

test_that("fremEqLatex() stops on an availCov name that is not a FREM covariate", {
  expect_error(.eq(availCov = c("WT", "WTT")), "WTT")
})

test_that("fremEqLatex() puts perLine terms on each line", {
  breaks <- function(x) lengths(regmatches(x, gregexpr("\\\\\n  & \\quad ", x, fixed = TRUE)))
  expect_equal(breaks(.eq()[["CL"]]), 5) # 18 terms, 3 per line
  expect_equal(breaks(.eq(perLine = 4)[["CL"]]), 4)
  expect_equal(breaks(.eq(perLine = 18)[["CL"]]), 0)
  expect_error(.eq(perLine = 0), "perLine")
})

test_that("fremEqLatex() rounds to digits significant digits", {
  expect_match(.eq(digits = 2)[["CL"]], "&= -0.0059\\,(\\text{WT} - 87) - 0.029\\,(", fixed = TRUE)
  expect_error(.eq(digits = 0), "digits")
})

test_that("fremEqLatex() adds a negative covariate mean", {
  ext <- .eqExt
  ext$THETA8[ext$ITERATION == -1000000000] <- -86.8633
  eqs <- fremEqLatex(
    dfext = ext, numNonFREMThetas = 7, numSkipOm = 2,
    covNames = .eqCovs, parNames = .eqPars, quiet = TRUE
  )
  expect_match(eqs[["CL"]], "&= -0.00591\\,(\\text{WT} + 86.9) - 0.0287\\,(", fixed = TRUE)
})

test_that("fremEqLatex() labels covariates with name_map and levels with level_map", {
  eqs <- .eq(
    name_map = c(WT = "Body weight", RACEL = "Race"),
    level_map = list(RACEL = c("2" = "Black", "3" = NA))
  )
  cl <- eqs[["CL"]]
  expect_match(cl, "(\\text{Body weight} - 86.9)", fixed = TRUE)
  expect_match(cl, "(\\text{Race}_{\\text{Black}} - ", fixed = TRUE)
  ## A level without a label, or with NA, is shown by its code
  expect_match(cl, "(\\text{Race}_{\\text{3}} - ", fixed = TRUE)
  expect_match(cl, "(\\text{NCIL}_{\\text{1}} - ", fixed = TRUE)
  expect_match(cl, "(\\text{HT} - 170)", fixed = TRUE)
})

test_that("fremEqLatex() uses a name_map entry for a whole indicator name", {
  cl <- .eq(name_map = c(RACEL_2 = "Black"))[["CL"]]
  expect_match(cl, "(\\text{Black} - ", fixed = TRUE)
  expect_no_match(cl, "\\text{RACEL}_{\\text{2}}", fixed = TRUE)
})

test_that("fremEqLatex() splits a name only before a level number", {
  covs <- sub("^LBWT$", "LB_WT", .eqCovs)
  cl <- fremEqLatex(
    dfext = .eqExt, numNonFREMThetas = 7, numSkipOm = 2,
    covNames = covs, parNames = .eqPars, quiet = TRUE
  )[["CL"]]
  expect_match(cl, "(\\text{LB\\_WT} - 57.5)", fixed = TRUE)
})

test_that("fremEqLatex() escapes the characters LaTeX reserves", {
  cl <- .eq(name_map = c(WT = "a_b & 1% {c} #2 $3 ~ ^ \\d"))[["CL"]]
  expect_match(
    cl,
    "(\\text{a\\_b \\& 1\\% \\{c\\} \\#2 \\$3 \\textasciitilde{} \\textasciicircum{} \\textbackslash{}d} - 86.9)",
    fixed = TRUE
  )
  par <- fremEqLatex(
    dfext = .eqExt, numNonFREMThetas = 7, numSkipOm = 2, covNames = .eqCovs,
    parNames = c("CL_1", "V", "MAT"), availCov = "WT", quiet = TRUE
  )
  expect_match(par[[1]], "\\text{COV}_{\\text{CL\\_1}} &= ", fixed = TRUE)
})

test_that("fremEqLatex() prints the equations unless quiet = TRUE", {
  eqs <- .eq(availCov = "WT")
  printed <- capture.output(
    shown <- fremEqLatex(
      runno = 31, modDevDir = .eqDir, parNames = .eqPars, availCov = "WT"
    )
  )
  expect_identical(shown, eqs)
  expect_identical(printed, strsplit(paste(eqs, collapse = "\n\n"), "\n")[[1]])
  expect_silent(.eq(availCov = "WT"))
})

test_that("fremEqLatex() says which input is missing", {
  expect_error(fremEqLatex(quiet = TRUE), "dfext")
  expect_error(fremEqLatex(dfext = .eqExt, covNames = .eqCovs, quiet = TRUE), "numNonFREMThetas")
  expect_error(fremEqLatex(dfext = .eqExt, numNonFREMThetas = 7, quiet = TRUE), "covNames")
})
