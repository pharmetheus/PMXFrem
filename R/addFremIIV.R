#' Add an IIV (ETA) to an established FREM model
#'
#' @description
#' Inserts one new inter-individual-variability random effect into a FREM model,
#' in the "skip" region that precedes the FREM `$OMEGA BLOCK(N)`. The new
#' `ETA()` takes index `numSkipOm + 1`; every `ETA()`, `MU_` and `COV`
#' reference at or after that index in `$PK` / `$ERROR` (including the
#' `;;;FREM CODE` block) is shifted up by one so the model stays consistent, and
#' a matching simple `$OMEGA` record is inserted immediately before the FREM
#' block. `numSkipOm` therefore increases by one; `numParCov`, the FREM
#' `BLOCK(N)` and the FREM covariate structure are untouched.
#'
#' This is the primitive used by [addFremStructuralTheta()]; call it directly
#' only when you want to add a random effect without a new structural `$THETA`.
#'
#' @details
#' The existing `.ext` / `.phi` are **not** migrated - a model with a new random
#' effect must be re-estimated. Only the control stream is rewritten.
#'
#' `link` controls how the new `ETA()` enters the target `$PK` line:
#' \describe{
#'   \item{`"exp"`}{`<param> = (<rhs>) * EXP(ETA(k))` - log-normal IIV, the
#'     FREM convention.}
#'   \item{`"add"`}{`<param> = <rhs> + ETA(k)` - additive IIV.}
#'   \item{`"none"`}{the `$PK` line is left alone; the renumber pass and the new
#'     `$OMEGA` record are still applied. Use this when the caller writes the
#'     `ETA()` reference itself (e.g. [addFremStructuralTheta()] with
#'     `muReference = TRUE`).}
#' }
#'
#' @param strFREMModel Path to the FREM model file, or a character vector of its
#'   lines. A path is required if `numNonFREMThetas` / `numSkipOm` are to be
#'   derived (they are read from the model + its `.ext` via [fremModelInfo()]).
#' @param parameter Name of the `$PK` variable that receives the new `ETA()`.
#'   Required unless `link = "none"`. Must be assigned on exactly one `$PK` line.
#' @param omegaInit Initial variance for the new `$OMEGA` record. Required - no
#'   default, since the value is a modelling choice (a small value such as
#'   `0.01` or `0.04` is typical for a fresh IIV).
#' @param link One of `"exp"` (default), `"add"` or `"none"` - see Details.
#' @param numNonFREMThetas,numSkipOm Structural integers. If `NULL` (default)
#'   they are derived from the model + `.ext`; supply them to override the
#'   derivation (or when `strFREMModel` is a character vector).
#' @param extFile Path to the `.ext` used for the derivation. Defaults to the
#'   model path with a `.ext` extension.
#' @param label Comment for the new `$OMEGA` record. Defaults to
#'   `"IIV on <parameter>"`.
#' @param newModel Output path when `strFREMModel` is a path and
#'   `bWriteMod = TRUE`. Defaults to the input path with `_iiv` inserted before
#'   the extension.
#' @param bWriteMod Logical. Write the new model to `newModel`? Ignored when
#'   `strFREMModel` is a character vector. Default `TRUE`.
#' @param quiet Logical. Suppress the progress message. Default `TRUE`.
#'
#' @return Invisibly, a list with
#'   \item{model}{the new model as a character vector}
#'   \item{etaIndex}{the index of the inserted `ETA()` (`numSkipOm + 1`)}
#'   \item{numSkipOm}{the updated skip count (old `+ 1`)}
#'   \item{numNonFREMThetas}{unchanged, passed through for convenience}
#'   \item{file}{the path written, or `NULL`}
#'
#' @seealso [addFremStructuralTheta()], [updateFREMmodel()]
#' @family FREM model management
#' @concept frem_model_management
#' @export
addFremIIV <- function(strFREMModel,
                       parameter = NULL,
                       omegaInit,
                       link = c("exp", "add", "none"),
                       numNonFREMThetas = NULL,
                       numSkipOm = NULL,
                       extFile = NULL,
                       label = NULL,
                       newModel = NULL,
                       bWriteMod = TRUE,
                       quiet = TRUE) {
  link <- match.arg(link)

  if (missing(omegaInit) || length(omegaInit) != 1L || !is.numeric(omegaInit) ||
    is.na(omegaInit) || omegaInit <= 0) {
    stop("addFremIIV(): `omegaInit` must be a single positive number - the ",
      "initial variance of the new $OMEGA (e.g. 0.01). It is required; a ",
      "fresh IIV cannot be given a sensible default.",
      call. = FALSE
    )
  }
  if (link != "none" && (is.null(parameter) || !nzchar(parameter))) {
    stop("addFremIIV(): `parameter` (the $PK variable to attach the new ETA to) ",
      "is required unless link = \"none\".",
      call. = FALSE
    )
  }

  isPath <- length(strFREMModel) == 1L && !grepl("\n", strFREMModel)
  lines <- if (isPath) readLines(strFREMModel, warn = FALSE) else strFREMModel

  ## ---- structural integers -------------------------------------------------
  if (is.null(numNonFREMThetas) || is.null(numSkipOm)) {
    if (!isPath) {
      stop("addFremIIV(): pass `numNonFREMThetas` and `numSkipOm` when ",
        "`strFREMModel` is a character vector.",
        call. = FALSE
      )
    }
    if (is.null(extFile)) {
      extFile <- paste0(tools::file_path_sans_ext(strFREMModel), ".ext")
    }
    if (!file.exists(extFile)) {
      stop("addFremIIV(): supply `numNonFREMThetas` and `numSkipOm`, or place ",
        basename(extFile), " next to the model so they can be derived.",
        call. = FALSE
      )
    }
    .info <- fremModelInfo(
      modFile = strFREMModel, dfext = extFile,
      numNonFREMThetas = numNonFREMThetas,
      numSkipOm = numSkipOm
    )
    if (is.null(numNonFREMThetas)) numNonFREMThetas <- .info$numNonFREMThetas
    if (is.null(numSkipOm)) numSkipOm <- .info$numSkipOm
    numTotEta <- .info$numTotEta
  } else {
    numTotEta <- .fremCountTotEta(lines)
  }

  kNew <- numSkipOm + 1L

  ## ---- renumber ETA()/MU_/COV at or after kNew in $PK and $ERROR ----------
  lines <- .fremRenumberEta(lines, fromIdx = kNew, maxIdx = numTotEta)

  ## ---- inject the ETA() reference into the target $PK line ---------------
  if (link != "none") {
    lines <- .fremAttachEta(lines,
      parameter = parameter, etaIdx = kNew,
      link = link
    )
  }

  ## ---- insert the new simple $OMEGA at eta index kNew ---------------------
  if (is.null(label)) {
    label <- if (!is.null(parameter)) paste0("IIV on ", parameter) else "added IIV"
  }
  newOmLine <- sprintf("$OMEGA  %s  ; %d. %s", format(omegaInit), kNew, label)
  at <- .fremOmegaInsertAt(lines, numSkipOm)
  lines <- c(lines[seq_len(at)], newOmLine, lines[(at + 1L):length(lines)])

  ## ---- write -----------------------------------------------------------
  outFile <- NULL
  if (isPath && bWriteMod) {
    outFile <- if (!is.null(newModel)) {
      newModel
    } else {
      paste0(
        tools::file_path_sans_ext(strFREMModel), "_iiv.",
        tools::file_ext(strFREMModel)
      )
    }
    writeLines(lines, outFile)
  }

  if (!quiet) {
    message(
      "addFremIIV(): inserted ETA(", kNew, ") (", label,
      "), $OMEGA init ", format(omegaInit),
      "; numSkipOm ", numSkipOm, " -> ", kNew,
      if (!is.null(outFile)) paste0("; written to ", outFile) else "", "."
    )
  }

  invisible(list(
    model = lines,
    etaIndex = kNew,
    numSkipOm = kNew,
    numNonFREMThetas = numNonFREMThetas,
    file = outFile
  ))
}


#' Where to insert a new $OMEGA so that it defines ETA(numSkipOm + 1)
#'
#' Returns the line number to insert *after*. The new record belongs
#' immediately before the first $OMEGA record that defines ETA(numSkipOm + 1),
#' which is found by counting etas, not by looking for "$OMEGA BLOCK(".
#'
#' Anchoring on the first BLOCK( is only equivalent when the skip omegas are
#' bare $OMEGA records. This package's own updateFREMmodel() writes them as
#' $OMEGA BLOCK(1) (inst/extdata/SimNeb/run31_new.mod), and there that anchor
#' lands at index 1: every existing IIV shifts down one record and the new eta
#' inherits whatever record it displaced was initialised to.
#'
#' @keywords internal
#' @noRd
.fremOmegaInsertAt <- function(lines, numSkipOm) {
  recs <- .fremOmegaRecords(lines)
  if (nrow(recs) == 0L) {
    stop("addFremIIV(): the model has no $OMEGA record to insert next to.",
      call. = FALSE
    )
  }
  ## etas defined by all records *before* each record
  before <- cumsum(c(0L, recs$n))[seq_len(nrow(recs))]
  i <- which(before == numSkipOm)[1]
  if (is.na(i)) {
    stop("addFremIIV(): no $OMEGA record starts at ETA(", numSkipOm + 1L,
      "), so the new record cannot be placed. The $OMEGA records define ",
      paste(recs$n, collapse = " + "), " = ", sum(recs$n),
      " eta(s); numSkipOm is ", numSkipOm,
      ". Check numSkipOm against the control stream.",
      call. = FALSE
    )
  }
  recs$start[i] - 1L
}


#' The $OMEGA records of a control stream, and how many etas each defines
#'
#' One row per record: `start` / `end` line numbers and `n`, the number of etas.
#' BLOCK(n) -> n; BLOCK(n) SAME or a bare SAME -> the previous record's size;
#' DIAGONAL(n) -> n; a bare $OMEGA -> the count of numeric values it carries
#' across its continuation lines, with `(v)xN` repetition expanded.
#'
#' Comments and the record-level options NM-TRAN allows here are stripped
#' first, so `$OMEGA BLOCK(1) 1e-04 FIX ; 2. IIV on D1` counts as one eta and
#' not as three numbers.
#'
#' @keywords internal
#' @noRd
.fremOmegaRecords <- function(lines) {
  isRec <- grepl("^\\s*\\$[A-Za-z]", lines)
  isOm <- grepl("^\\s*\\$OM", lines, ignore.case = TRUE)
  starts <- which(isOm)
  empty <- data.frame(
    start = integer(0), end = integer(0), n = integer(0)
  )
  if (length(starts) == 0L) {
    return(empty)
  }
  recStarts <- which(isRec)
  ends <- vapply(starts, function(s) {
    nxt <- recStarts[recStarts > s]
    if (length(nxt)) nxt[1] - 1L else length(lines)
  }, integer(1))

  n <- integer(length(starts))
  for (k in seq_along(starts)) {
    txt <- paste(sub(";.*$", "", lines[starts[k]:ends[k]]), collapse = " ")
    txt <- sub("^\\s*\\$[A-Za-z]+", "", txt)
    up <- toupper(txt)
    blk <- regmatches(up, regexpr("BLOCK\\s*\\(\\s*[0-9]+\\s*\\)", up))
    dia <- regmatches(up, regexpr("DIAGONAL\\s*\\(\\s*[0-9]+\\s*\\)", up))
    if (length(blk) == 1L) {
      n[k] <- as.integer(gsub("[^0-9]", "", blk))
    } else if (length(dia) == 1L) {
      n[k] <- as.integer(gsub("[^0-9]", "", dia))
    } else if (grepl("\\bSAME\\b", up)) {
      ## BLOCK SAME with no size: same dimension as the previous record
      n[k] <- if (k > 1L) n[k - 1L] else 0L
    } else {
      n[k] <- .fremCountOmegaValues(txt)
    }
  }
  data.frame(start = starts, end = ends, n = n)
}


#' Largest ETA index used anywhere in a model's lines
#' @keywords internal
#' @noRd
.fremCountTotEta <- function(lines) {
  # (?<![A-Za-z]) so ETA( inside THETA( / BETA( / ZETA( is not counted
  m <- regmatches(lines, gregexpr("(?<![A-Za-z])ETA\\(\\s*[0-9]+\\s*\\)", lines, perl = TRUE))
  idx <- as.integer(gsub("[^0-9]", "", unlist(m)))
  if (length(idx) == 0L) 0L else max(idx)
}


#' Shift ETA()/MU_/COV indices >= fromIdx up by one, in $PK and $ERROR only
#'
#' Processed in descending index order and anchored so `ETA(2)` is never seen
#' inside `ETA(23)` and `MU_2` / `COV2` never inside `MU_23` / `COV23`.
#' @keywords internal
#' @noRd
.fremRenumberEta <- function(lines, fromIdx, maxIdx) {
  if (maxIdx < fromIdx) {
    return(lines)
  }

  recStart <- grep("^\\s*\\$[A-Za-z]", lines)
  region <- rep(FALSE, length(lines))
  for (s in recStart) {
    if (grepl("^\\s*\\$(PK|ERROR|PRED)\\b", lines[s], ignore.case = TRUE)) {
      e <- recStart[recStart > s]
      e <- if (length(e)) e[1] - 1L else length(lines)
      region[s:e] <- TRUE
    }
  }

  idx <- which(region)
  seg <- lines[idx]
  for (k in seq(maxIdx, fromIdx)) {
    kk <- k + 1L
    # (?<![A-Za-z]) so ETA( inside THETA( / BETA( / ZETA( is never matched
    seg <- gsub(sprintf("(?<![A-Za-z])ETA\\(\\s*%d\\s*\\)", k), sprintf("ETA(%d)", kk), seg, perl = TRUE)
    seg <- gsub(sprintf("(?<![A-Za-z0-9_])MU_%d(?![0-9])", k), sprintf("MU_%d", kk), seg, perl = TRUE)
    seg <- gsub(sprintf("(?<![A-Za-z0-9_])COV%d(?![0-9])", k), sprintf("COV%d", kk), seg, perl = TRUE)
  }
  lines[idx] <- seg
  lines
}


#' Attach ` * EXP(ETA(k))` or ` + ETA(k)` to a parameter's $PK assignment
#' @keywords internal
#' @noRd
.fremAttachEta <- function(lines, parameter, etaIdx, link) {
  pat <- sprintf("^(\\s*)%s(\\s*)=(\\s*)(.*)$", .fremEscape(parameter))
  hit <- grep(pat, lines)
  # keep only $PK hits (defensive: a same-named var could appear in $ERROR)
  recStart <- grep("^\\s*\\$[A-Za-z]", lines)
  pkStart <- recStart[grepl("^\\s*\\$PK\\b", lines[recStart], ignore.case = TRUE)]
  if (length(pkStart)) {
    pkEnd <- recStart[recStart > pkStart[1]]
    pkEnd <- if (length(pkEnd)) pkEnd[1] - 1L else length(lines)
    hit <- hit[hit >= pkStart[1] & hit <= pkEnd]
  }
  if (length(hit) == 0L) {
    stop("addFremIIV(): no $PK assignment of '", parameter, "' was found.",
      call. = FALSE
    )
  }
  if (length(hit) > 1L) {
    stop("addFremIIV(): '", parameter, "' is assigned on more than one $PK line (",
      paste(hit, collapse = ", "), "); cannot decide where the ETA goes.",
      call. = FALSE
    )
  }

  i <- hit[1]
  m <- regmatches(lines[i], regexec(pat, lines[i]))[[1]]
  lead <- m[2]
  rhsAll <- m[5]
  # split a trailing comment off the RHS
  cpos <- regexpr(";", rhsAll, fixed = TRUE)
  if (cpos > 0) {
    rhs <- sub("\\s+$", "", substr(rhsAll, 1, cpos - 1))
    comment <- substr(rhsAll, cpos, nchar(rhsAll))
  } else {
    rhs <- sub("\\s+$", "", rhsAll)
    comment <- ""
  }

  newRhs <- switch(link,
    exp = sprintf("(%s) * EXP(ETA(%d))", rhs, etaIdx),
    add = sprintf("%s + ETA(%d)", rhs, etaIdx)
  )

  lines[i] <- sprintf(
    "%s%s = %s%s", lead, parameter, newRhs,
    if (nzchar(comment)) paste0("  ", comment) else ""
  )
  lines
}

#' Escape a string for use as a literal in a regex
#' @keywords internal
#' @noRd
.fremEscape <- function(x) gsub("([.^$*+?()\\[\\]{}|\\\\])", "\\\\\\1", x, perl = TRUE)
