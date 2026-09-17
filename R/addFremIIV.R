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
#' @examples
#' # run31.mod has numSkipOm = 2, so a new IIV takes ETA(3) and everything
#' # from ETA(3) up shifts by one. The .ext is not migrated: re-estimate.
#' td <- tempfile()
#' dir.create(td)
#' file.copy(system.file("extdata/SimNeb/run31.mod", package = "PMXFrem"), td)
#' file.copy(system.file("extdata/SimNeb/run31.ext", package = "PMXFrem"), td)
#'
#' res <- addFremIIV(file.path(td, "run31.mod"),
#'   parameter = "FREL", omegaInit = 0.04, bWriteMod = FALSE
#' )
#' res$etaIndex # 3
#' res$numSkipOm # 3: one more than before
#'
#' # the new $OMEGA lands after the skip records, before the FREM block
#' grep("^\\$OMEGA", res$model, value = TRUE)[1:4]
#'
#' unlink(td, recursive = TRUE)
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
  .fremStopOnAbbrReplace(lines, "addFremIIV")

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
    ## the larger of the ext's count and the model's own references: with an
    ## ext from before an earlier mutation the ext count is short, and the
    ## renumber would stop below the highest reference, duplicating MU_/COV
    numTotEta <- max(.info$numTotEta, .fremCountTotEta(lines))
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
  ## $OME / $OMEG / $OMEGA, but not the $OMEGAP / $OMEGAPD prior records,
  ## which describe a prior and define none of the model's etas.
  isOm <- grepl("^\\s*\\$OME", lines, ignore.case = TRUE) &
    !grepl("^\\s*\\$OMEGAP", lines, ignore.case = TRUE)
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
  repsSeen <- rep(1L, length(starts))
  for (k in seq_along(starts)) {
    txt <- paste(sub(";.*$", "", lines[starts[k]:ends[k]]), collapse = " ")
    txt <- sub("^\\s*\\$[A-Za-z]+", "", txt)
    up <- toupper(txt)
    blk <- regmatches(up, regexpr("BLOCK\\s*\\(\\s*[0-9]+\\s*\\)", up))
    dia <- regmatches(up, regexpr("DIAGONAL\\s*\\(\\s*[0-9]+\\s*\\)", up))
    ## NONMEM 7.3's SAME(m) stands for m repeats of the preceding block, so
    ## the record defines m times the block's dimension, not one.
    sameRep <- regmatches(up, regexpr("SAME\\s*\\(\\s*[0-9]+\\s*\\)", up))
    reps <- if (length(sameRep) == 1L) {
      as.integer(gsub("[^0-9]", "", sameRep))
    } else {
      1L
    }
    isSame <- grepl("\\bSAME\\b", up)
    if (length(blk) == 1L) {
      n[k] <- as.integer(gsub("[^0-9]", "", blk)) * if (isSame) reps else 1L
    } else if (length(dia) == 1L) {
      n[k] <- as.integer(gsub("[^0-9]", "", dia))
    } else if (isSame) {
      ## BLOCK SAME with no size: the previous record's dimension. When that
      ## record was itself a SAME(m), its per-repeat size is n / m.
      prev <- if (k > 1L) n[k - 1L] else 0L
      prevReps <- if (k > 1L) repsSeen[k - 1L] else 1L
      n[k] <- (prev %/% prevReps) * reps
    } else {
      n[k] <- .fremCountOmegaValues(txt)
    }
    repsSeen[k] <- reps
  }
  data.frame(start = starts, end = ends, n = n)
}


#' Largest ETA index used anywhere in a model's lines
#' @keywords internal
#' @noRd
.fremCountTotEta <- function(lines) {
  # Comment text is not a reference. A commented-out ETA(99) used to set
  # numTotEta to 99, and with it numParCov and the generated function's
  # default etas length.
  lines <- sub(";.*$", "", lines)
  # (?<![A-Za-z]) so ETA( inside THETA( / BETA( / ZETA( is not counted
  m <- regmatches(lines, gregexpr("(?i)(?<![A-Za-z])ETA\\(\\s*[0-9]+\\s*\\)", lines, perl = TRUE))
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
    ## Every record that can hold abbreviated code and therefore an ETA(),
    ## MU_ or COV reference. $DES in particular: an index left behind there
    ## is still in range, so a count check sees nothing, but it now names a
    ## different random effect than it did before the insertion.
    if (grepl("^\\s*\\$(PK|PRED|ERROR|DES|AES|MIX|INFN)\\b", lines[s],
      ignore.case = TRUE
    )) {
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
    ## (?i) because NM-TRAN does not care about case in abbreviated code.
    ## Renumbering only the upper-case spelling left the other one behind,
    ## and the reference that *was* renumbered then pointed at a variable
    ## nothing assigns - a control stream NM-TRAN rejects. The replacement is
    ## canonical upper case.
    seg <- gsub(sprintf("(?i)(?<![A-Za-z])ETA\\(\\s*%d\\s*\\)", k), sprintf("ETA(%d)", kk), seg, perl = TRUE)
    seg <- gsub(sprintf("(?i)(?<![A-Za-z0-9_])MU_%d(?![0-9])", k), sprintf("MU_%d", kk), seg, perl = TRUE)
    seg <- gsub(sprintf("(?i)(?<![A-Za-z0-9_])COV%d(?![0-9])", k), sprintf("COV%d", kk), seg, perl = TRUE)
  }
  lines[idx] <- seg

  ## $TABLE ETAn columns name etas too. Left alone, the ETA3 column in the
  ## output table would hold whatever eta was inserted at 3. ETAS(1:LAST) is
  ## a range, not a column name, and is not matched.
  tab <- integer(0)
  recStartT <- grep("^\\s*\\$[A-Za-z]", lines)
  for (st in recStartT[grepl("^\\s*\\$TAB", lines[recStartT], ignore.case = TRUE)]) {
    en <- recStartT[recStartT > st]
    en <- if (length(en)) en[1] - 1L else length(lines)
    tab <- c(tab, st:en)
  }
  if (length(tab)) {
    tseg <- lines[tab]
    for (k in seq(maxIdx, fromIdx)) {
      tseg <- gsub(sprintf("(?i)(?<![A-Za-z0-9_])ETA%d(?![0-9A-Za-z_(])", k),
        sprintf("ETA%d", k + 1L), tseg,
        perl = TRUE
      )
    }
    lines[tab] <- tseg
  }
  lines
}


#' Attach ` * EXP(ETA(k))` or ` + ETA(k)` to a parameter's $PK assignment
#' @keywords internal
#' @noRd
.fremAttachEta <- function(lines, parameter, etaIdx, link) {
  ## Found through .fremAssignLines(): the parameter's $PK - or $PRED - record,
  ## matched case-insensitively as NM-TRAN does.
  pat <- sprintf("(?i)^(\\s*)(%s)(\\s*)=(\\s*)(.*)$", .fremEscape(parameter))
  found <- .fremAssignLines(lines, parameter)
  rec <- if (nrow(found)) found$record[1] else "$PK"
  ## Any guarded assignment refuses - not only when there is no plain one.
  ## `FREL = ...` followed by `IF(FORM.EQ.2) FREL = ...` used to take the eta
  ## on the first line and lose it, silently, for every FORM = 2 subject.
  if (any(found$guarded)) {
    stop("addFremIIV(): '", parameter, "' is assigned conditionally in ", rec,
      " (line", if (sum(found$guarded) > 1L) "s " else " ",
      paste(found$line[found$guarded], collapse = ", "),
      "); attaching an ETA to one branch would change only that branch. ",
      "Edit the branches by hand, or attach the ETA to a parameter that ",
      "is assigned once.",
      call. = FALSE
    )
  }
  hit <- found$line[!found$guarded]
  if (length(hit) == 0L) {
    stop("addFremIIV(): no ", rec, " assignment of '", parameter, "' was found.",
      call. = FALSE
    )
  }
  if (length(hit) > 1L) {
    stop("addFremIIV(): '", parameter, "' is assigned on more than one ", rec,
      " line (", paste(hit, collapse = ", "), "); cannot decide where the ETA goes.",
      call. = FALSE
    )
  }

  i <- hit[1]
  m <- regmatches(lines[i], regexec(pat, lines[i], perl = TRUE))[[1]]
  lead <- m[2]
  lhsName <- m[3]
  rhsAll <- m[6]
  # split a trailing comment off the RHS
  cpos <- regexpr(";", rhsAll, fixed = TRUE)
  if (cpos > 0) {
    rhs <- sub("\\s+$", "", substr(rhsAll, 1, cpos - 1))
    comment <- substr(rhsAll, cpos, nchar(rhsAll))
  } else {
    rhs <- sub("\\s+$", "", rhsAll)
    comment <- ""
  }

  ## Stacking a second random effect on a parameter that already has one is
  ## sometimes intended and sometimes a slip; nothing used to say which.
  existing <- .fremCountTotEta(rhs)
  if (existing > 0L) {
    warning("addFremIIV(): the $PK assignment of '", parameter,
      "' already references ETA(). The new ETA(", etaIdx,
      ") is added alongside it, giving the parameter two independent random ",
      "effects.",
      call. = FALSE
    )
  }

  newRhs <- switch(link,
    exp = sprintf("(%s) * EXP(ETA(%d))", rhs, etaIdx),
    add = sprintf("%s + ETA(%d)", rhs, etaIdx)
  )

  lines[i] <- sprintf(
    "%s%s = %s%s", lead, lhsName, newRhs,
    if (nzchar(comment)) paste0("  ", comment) else ""
  )
  lines
}

#' Escape a string for use as a literal in a regex
#' @keywords internal
#' @noRd
.fremEscape <- function(x) gsub("([.^$*+?()\\[\\]{}|\\\\])", "\\\\\\1", x, perl = TRUE)
