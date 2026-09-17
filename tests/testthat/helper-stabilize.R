#' Stabilize a character vector for snapshot testing
#'
#' Finds all number-like substrings in a character vector, converts them
#' to numeric, formats them to a consistent precision, and substitutes them
#' back into the strings.
#'
#' @param text_vector The character vector to stabilize.
#' @param sig_figs The number of significant figures to keep.
#'
#' @return The stabilized character vector.
#' Stabilize a character vector for snapshot testing (Robust Version)
#'
#' Deconstructs each string into number and non-number parts, processes the
#' numbers, and reconstructs the string to perfectly preserve structure.
#'
#' @param text_vector The character vector to stabilize.
#' @param sig_figs The number of significant figures to keep.
#'
#' @return The stabilized character vector.
stabilize_text_snapshot <- function(text_vector, sig_figs = 8) {
  # A robust regex to find numbers (including integers, decimals, and sci-notation)
  num_regex <- "[+-]?\\d*\\.?\\d+(?:[Ee][+-]?\\d+)?"

  sapply(text_vector, FUN = function(line) {
    if (!is.character(line) || is.na(line)) {
      return(line)
    }

    # 1. Find the positions of all numbers
    matches <- gregexpr(num_regex, line)
    if (matches[[1]][1] == -1) {
      return(line) # No numbers found, return line as-is
    }

    # 2. Extract the number strings and the non-number "scaffolding" separately
    number_strings <- regmatches(line, matches)[[1]]
    scaffolding <- regmatches(line, matches, invert = TRUE)[[1]]

    # 3. Process only the number strings
    processed_numbers <- sapply(number_strings, function(num_str) {
      num <- as.numeric(num_str)
      # Use sprintf for reliable, non-padded formatting
      sprintf(paste0("%.", sig_figs, "g"), num)
    })

    # 4. Weave the scaffolding and processed numbers back together
    # The result is an alternating sequence:
    # scaffold[1], processed_num[1], scaffold[2], processed_num[2], ...
    result <- character(length(scaffolding) + length(processed_numbers))
    result[seq(1, by = 2, length.out = length(scaffolding))] <- scaffolding
    result[seq(2, by = 2, length.out = length(processed_numbers))] <- processed_numbers

    paste(result, collapse = "")
  }, USE.NAMES = FALSE)
}



#' Smart wrapper to stabilize any object for snapshot testing
#'
#' Automatically selects the correct stabilization method based on the input type.
#' - If `x` is a character vector, it stabilizes numbers within the text.
#' - If `x` is any other object (list, data.frame, numeric), it recursively
#'   rounds all numeric elements.
#'
#' @param x The object to stabilize.
#' @param digits The number of decimal places for numeric objects.
#' @param sig_figs The number of significant figures for text-based numbers.
#'
#' @return The stabilized object.
#' Recursively stabilize any object for snapshot testing
#'
#' This function intelligently handles any R object by applying the correct
#' stabilization rule at every level of a nested structure.
#' - Numeric vectors/matrices are rounded.
#' - Character vectors have numbers within them rounded and formatted.
#' - Lists and data.frames are recursively processed element by element or column by column.
#'
#' @param x The object to stabilize.
#' @param digits The number of decimal places for numeric objects.
#' @param sig_figs The number of significant figures for text-based numbers.
#'
#' @return The stabilized object.
stabilize <- function(x, digits = 8, sig_figs = 8) {
  # Base case 1: If it's a numeric vector or matrix, round it.
  if (is.numeric(x)) {
    return(round(x, digits))
  }

  # Base case 2: If it's a character vector, stabilize the text.
  if (is.character(x)) {
    return(stabilize_text_snapshot(x, sig_figs = sig_figs))
  }

  # Recursive step for data.frames: apply stabilize to each column.
  if (is.data.frame(x)) {
    # The x[] is important to preserve the data.frame class
    x[] <- lapply(x, stabilize, digits = digits, sig_figs = sig_figs)
    return(x)
  }

  # Recursive step for lists: apply stabilize to each element.
  if (is.list(x)) {
    return(lapply(x, stabilize, digits = digits, sig_figs = sig_figs))
  }

  # Fallback for any other type (factors, dates, etc.)
  return(x)
}

# Add this to your helper-stabilize.R file
# You can delete the old clean_condition_message function.

#' Extracts the core message(s) from a formatted testthat condition snapshot
#' @param text The formatted character string from the snapshot.
#' @return The core message text, stripped of version-specific headers.
extract_core_messages <- function(text) {
  # Split the full text into individual lines
  lines <- strsplit(text, "\n")[[1]]

  # Keep only the lines that are the actual message, filtering out headers
  # from both old and new testthat/rlang versions.
  core_lines <- grep("^(<|Error|Warning|!|\\s*$)", lines, value = TRUE, invert = TRUE)

  # Trim whitespace and paste back together
  paste(trimws(core_lines), collapse = "\n")
}

#' Standardize ggplot_build() data for snapshotting
#'
#' Handles changes in ggplot2's internal data structures across versions,
#' like the renaming of the 'linewidth' aesthetic to 'size'.
#'
#' @param plot_data_list The list of data frames from `ggplot_build(p)$data`.
#' @return A list of data frames with standardized column names.
standardize_plot_data <- function(plot_data_list) {
  lapply(plot_data_list, function(df) {
    # In ggplot2 v3.4.0, `size` aesthetic for lines was renamed to `linewidth`.
    # We standardize it back to `size` for snapshot consistency.
    if ("linewidth" %in% names(df)) {
      names(df)[names(df) == "linewidth"] <- "size"
    }
    df
  })
}

# Add this to helper-stabilize.R

#' Stabilize random temp file paths in model code snapshots
#'
#' Finds the $DATA line in a model file and replaces the random filepath
#' with a consistent placeholder. It also stabilizes the rest of the object.
#'
#' @param result_list A list returned from updateFREMmodel, containing $data and $model.
#' @return A stabilized list suitable for snapshotting.
stabilize_model_paths <- function(result_list) {
  # First, stabilize the data components recursively as usual
  stable_list <- stabilize(result_list)

  # Then, specifically fix the random path in the model code
  model_code <- stable_list$model
  if (!is.null(model_code)) {
    data_line_index <- grep("^\\$DATA", model_code)
    if (length(data_line_index) > 0) {
      # Use gsub to replace the random path with a placeholder
      stable_list$model[data_line_index] <- gsub(
        pattern = "(\\$DATA\\s+).*( IGNORE=@)",
        replacement = "\\1[placeholder_path]\\2",
        x = model_code[data_line_index]
      )
    }
  }

  return(stable_list)
}

#' Stabilize a result whose row order is not part of the contract
#'
#' `getForestDFFREM()` emits one row per covariate level, and the level order
#' comes from `PMXForest::getCovStats()`. PMXForest 1.3.0 began sorting binary
#' levels - deliberately, because the old order depended on which subject
#' appeared first in the data - which reordered the ETHNIC rows and failed every
#' snapshot that encoded the old order.
#'
#' Row order is that function's concern, not this package's, and the tests
#' already assert the labels separately. So sort the rows on every column before
#' comparing: the assertion is then about the *content*, and a dependency
#' reordering its output no longer looks like a regression.
#'
#' @param x A data.frame.
#' @param ... Passed to [stabilize()].
#' @return `x`, stabilized, with rows in a deterministic order and row names
#'   dropped.
stabilizeRows <- function(x, ...) {
  x <- stabilize(x, ...)
  if (is.data.frame(x) && nrow(x) > 1) {
    key <- lapply(x, function(col) as.character(col))
    x <- x[do.call(order, key), , drop = FALSE]
    rownames(x) <- NULL
  }
  x
}

## Columns of a getForestDFFREM() result that are derived from the sampled
## parameter vectors. Established empirically, by running the same call with two
## different draws and seeing which columns move.
##
## These cannot be asserted exactly across machines. getSamples() draws through
## MASS::mvrnorm(), which calls eigen(Sigma, symmetric = TRUE); eigenvector
## signs and the treatment of near-degenerate eigenvalues are LAPACK-dependent,
## so the same seed on two OpenBLAS builds gives different draws. That, not the
## R version, is what the old _snaps/4.2.2 and _snaps/4.4.2 variants were
## really recording.
forestSampledCols <- c(
  "POINT", "POINT_NOVAR_REL_REFFUNC", "POINT_REL_REFFUNC", "POINT_REL_REFFINAL",
  "Q1", "Q1_REL_REFFUNC", "Q1_REL_REFFINAL", "Q1_NOVAR_REL_REFFUNC",
  "Q2", "Q2_REL_REFFUNC", "Q2_REL_REFFINAL", "Q2_NOVAR_REL_REFFUNC"
)

#' Assert what a sampled Forest-plot result must satisfy on any machine
#'
#' The exact values are not reproducible (see [forestSampledCols]), but several
#' things about them are. An earlier version of this asserted only the quantile
#' ordering, and asserted it with `x[["Q1"]] <= x[["POINT"]]` - which on a frame
#' missing those columns is `NULL <= numeric(0)`, i.e. `logical(0)`, and
#' `all(logical(0))` is `TRUE`. It therefore passed on a frame with none of the
#' columns, on a zero-row frame, and on one whose every value had been
#' multiplied by 1000. The preconditions below exist so that cannot happen
#' again: nothing is asserted until the columns are known to be there and to
#' hold rows.
#'
#' @param x A `getForestDFFREM()` / `getForestDFSCM()` result.
#' @param lo,point,hi Names of the quantile and point-estimate columns.
#' @param rows The number of rows the caller expects. Required - a truncated
#'   result is exactly the failure a shape check should catch, and it cannot be
#'   inferred from the object itself.
#' @param relBand The band a `_REL_` column has to lie in. These are ratios to
#'   a reference, so they are of order 1; a result whose units have slipped
#'   leaves the band while the quantile ordering stays intact.
expect_forest_sampling_sane <- function(x, lo = "Q1", point = "POINT",
                                        hi = "Q2", rows,
                                        relBand = c(1e-3, 1e3)) {
  testthat::expect_s3_class(x, "data.frame")
  testthat::expect_equal(nrow(x), rows)

  need <- c(lo, point, hi)
  missing <- setdiff(need, names(x))
  testthat::expect_true(length(missing) == 0L,
    info = paste("missing column(s):", paste(missing, collapse = ", "))
  )
  present <- intersect(forestSampledCols, names(x))
  testthat::expect_true(length(present) >= length(need),
    info = paste(
      "expected the sampled columns to be present; found",
      length(present), "of", length(forestSampledCols)
    )
  )

  for (cc in present) {
    testthat::expect_true(all(is.finite(x[[cc]])),
      info = paste(cc, "should be finite everywhere")
    )
  }
  testthat::expect_true(all(x[[lo]] <= x[[point]] + 1e-8),
    info = "the lower quantile should not exceed the point estimate"
  )
  testthat::expect_true(all(x[[point]] <= x[[hi]] + 1e-8),
    info = "the point estimate should not exceed the upper quantile"
  )
  ## Degenerate results satisfy every ordering above: an interval of zero
  ## width, or the same value in every row. Neither is a plausible sampled
  ## forest result, and a mutation audit found both passed.
  testthat::expect_true(any(x[[hi]] > x[[lo]] + 1e-8),
    info = "at least one interval should have non-zero width"
  )
  testthat::expect_true(stats::sd(x[[point]]) > 0,
    info = "the point estimate should vary across covariate levels"
  )

  ## A ratio-to-reference column is of order 1 whatever the parameter's units.
  for (cc in grep("_REL_", present, value = TRUE)) {
    testthat::expect_true(
      all(x[[cc]] > relBand[1] & x[[cc]] < relBand[2]),
      info = sprintf(
        "%s ranges %.3g .. %.3g, outside the [%g, %g] a ratio to a reference
         should occupy", cc, min(x[[cc]]), max(x[[cc]]), relBand[1], relBand[2]
      )
    )
  }
  invisible(x)
}


#' Reduce a fremParameterTable() result to the part that is reproducible
#'
#' With `includeRSE = TRUE` almost everything the function returns is derived
#' from `n` parameter vectors drawn through `MASS::mvrnorm()`, which calls
#' `eigen()` and is therefore LAPACK-dependent - established empirically by
#' running the same call under two seeds and seeing which elements move:
#'
#'   deterministic      parameterTable$Type, $Parameter, $Estimate
#'   sample-dependent   parameterTable$`RSE (%)`, Samples, Condition,
#'                      coefficientTable_long, coefficientTable_wide
#'
#' Only the first is safe to snapshot across machines. The rest is checked by
#' [expect_rse_sane()] for what it must satisfy anywhere.
dropSampledRSE <- function(x) {
  if (!is.null(x$parameterTable)) {
    x$parameterTable <- x$parameterTable[
      , setdiff(names(x$parameterTable), "RSE (%)"),
      drop = FALSE
    ]
  }
  for (nm in c(
    "Samples", "Condition", "coefficientTable_long",
    "coefficientTable_wide"
  )) {
    x[[nm]] <- NULL
  }
  x
}

#' Assert the sampled parts of a fremParameterTable() result are well formed
#'
#' The exact numbers are not reproducible off one machine; these properties are.
#' The previous version accepted a 900% RSE and a `Samples` frame holding two
#' rows when 175 had been requested, so it is worth being explicit about what
#' each assertion is for.
#'
#' @param x A `fremParameterTable()` result.
#' @param n The number of parameter vectors that were requested. The draws are
#'   the whole basis of the RSE, so a short `Samples` means the number reported
#'   was computed from something other than what was asked for.
#' @param maxRSE The largest RSE that is plausible for these models, in percent.
expect_rse_sane <- function(x, n, maxRSE = 100) {
  rse <- suppressWarnings(as.numeric(as.character(x$parameterTable[["RSE (%)"]])))
  testthat::expect_true(length(rse) > 0L,
    info = "the parameter table should have an RSE column with values in it"
  )
  testthat::expect_true(all(is.finite(rse)), info = "every RSE should be finite")
  testthat::expect_true(all(rse >= 0), info = "an RSE cannot be negative")
  testthat::expect_true(all(rse <= maxRSE),
    info = sprintf(
      "largest RSE is %.1f%%, over the %g%% that is plausible here",
      max(rse), maxRSE
    )
  )
  testthat::expect_true(any(rse > 0), info = "not every RSE should be zero")
  testthat::expect_true(length(unique(rse)) > 1,
    info = "every parameter reporting the same RSE is not a plausible result"
  )

  ## the draws themselves: as many as were asked for, and all finite
  testthat::expect_s3_class(x$Samples, "data.frame")
  testthat::expect_equal(nrow(x$Samples), n)
  num <- vapply(x$Samples, is.numeric, logical(1))
  testthat::expect_true(all(vapply(x$Samples[num], function(c) all(is.finite(c)), TRUE)),
    info = "every sampled parameter value should be finite"
  )
  ## draws that are all the same vector - the estimates row repeated n times -
  ## have the right shape and describe no uncertainty at all
  testthat::expect_true(
    any(vapply(x$Samples[num], function(c) stats::sd(c) > 0, TRUE)),
    info = "the sampled parameter vectors should actually vary"
  )
  invisible(x)
}


#' A small, readable stand-in for snapshotting a whole data frame
#'
#' `expect_snapshot_value(df, style = "serialize")` writes base64 - 28 MB of it
#' in one case here - and nobody can read a diff in that, so accepting a
#' changed snapshot is an unconditional yes. This returns instead a per-column
#' summary that fits on a screen: enough to notice a change anywhere in the
#' frame, and legible enough that noticing one means something.
#'
#' The summary moves if a value moves (mean and sd), if a value is added or
#' removed (n, nDistinct), if the range shifts (min, max), or if a column
#' changes type or name.
#'
#' Those are all invariant to reordering, which is exactly the failure a FREM
#' data set is most exposed to: covariates attached to the wrong subject after
#' a merge, or records reordered within a subject, which NONMEM reads in
#' sequence. A mutation audit showed a shuffled WT column, two subjects' AGE /
#' WT / SEX swapped, and one subject's records reversed all left the moments
#' and the first rows untouched. So each column also carries `hash`, an
#' order-sensitive hash of its values formatted to `digits` significant
#' figures - formatted rather than raw, so the same data hashes the same on
#' another platform. The moments say which column changed and roughly how;
#' the hash says that it changed at all.
#'
#' @param x A data frame.
#' @param digits Significant figures for the numeric summaries.
#' @return A data frame with one row per column of `x`.
columnDigest <- function(x, digits = 6) {
  x <- as.data.frame(x)
  sig <- function(v) if (is.finite(v)) signif(v, digits) else v
  rows <- lapply(names(x), function(nm) {
    col <- x[[nm]]
    num <- is.numeric(col)
    data.frame(
      column = nm,
      type = class(col)[1],
      n = length(col),
      nMissing = sum(is.na(col)),
      nDistinct = length(unique(col)),
      mean = if (num) sig(mean(col, na.rm = TRUE)) else NA_real_,
      sd = if (num) sig(stats::sd(col, na.rm = TRUE)) else NA_real_,
      min = if (num) sig(min(col, na.rm = TRUE)) else NA_real_,
      max = if (num) sig(max(col, na.rm = TRUE)) else NA_real_,
      hash = rlang::hash(
        if (num) sprintf(paste0("%.", digits, "g"), col) else as.character(col)
      ),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


#' Snapshot a data frame as something a reviewer can read
#'
#' The per-column digest plus the first few rows, both in `json2` style so the
#' `_snaps` file is text rather than base64.
#'
#' @param x A data frame.
#' @param head Number of leading rows to include.
expect_frame_snapshot <- function(x, head = 8) {
  x <- as.data.frame(x)
  ## cran = TRUE: these snapshot deterministic things - a control stream and a
  ## per-column digest - so there is no reason to skip them outside
  ## test_local(). Skipping also aborts the block, which silently drops every
  ## assertion after the snapshot under test_dir() and R CMD check.
  testthat::expect_snapshot_value(columnDigest(x), style = "json2", cran = TRUE)
  testthat::expect_snapshot_value(
    stabilize(utils::head(x, head)),
    style = "json2", cran = TRUE
  )
  invisible(x)
}


#' Snapshot an updateFREMmodel() result so the control stream is readable
#'
#' The result carries `$model`, a character vector of the rewritten control
#' stream - which is the thing these tests are about and is small enough to
#' read - and `$data`, a FREM data set of tens of thousands of rows, which is
#' what made the serialized snapshot 26 MB. The model goes in as text; the data
#' goes in as [columnDigest()] plus its first rows.
expect_model_snapshot <- function(x, head = 8) {
  ## The model goes in as written. stabilize() rewrites every number-like
  ## substring, which turns NM-TRAN's IF(FOOD.EQ.1) into IF(FOOD.EQ0.1) - a
  ## snapshot of something the function never produced, and a confusing thing
  ## to hand a reviewer. Only the $DATA path needs neutralising, and it is a
  ## temporary directory rather than a number.
  model <- x$model
  if (!is.null(model)) {
    i <- grep("^\\$DATA", model)
    if (length(i)) {
      model[i] <- sub("(\\$DATA\\s+)\\S+", "\\1[placeholder_path]", model[i])
    }
    testthat::expect_snapshot_value(model, style = "json2", cran = TRUE)
  }
  if (!is.null(x$data)) {
    testthat::expect_snapshot_value(columnDigest(x$data), style = "json2", cran = TRUE)
    testthat::expect_snapshot_value(
      stabilize(utils::head(as.data.frame(x$data), head)),
      style = "json2", cran = TRUE
    )
  }
  invisible(x)
}
