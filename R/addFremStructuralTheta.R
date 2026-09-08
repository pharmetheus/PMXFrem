#' Add a structural `$THETA` (and optionally an IIV) to an established FREM model
#'
#' @description
#' Inserts one new structural (non-FREM) `$THETA` at index `numNonFREMThetas + 1`,
#' immediately before the FREM covariate-mean thetas. Every `THETA()` reference
#' at or after that index in `$PK` / `$ERROR` / `$THETA` is shifted up by one -
#' in a FREM model that is the `MU_k = THETA(numNonFREMThetas + c)` block - so the
#' model stays consistent. `numNonFREMThetas` increases by one.
#'
#' With `addEta = TRUE` the function also adds a matching IIV via
#' [addFremIIV()] and wires the two together. This is the usual entry point:
#' adding a structural parameter with a typical value, an MU reference and
#' between-subject variability in one step.
#'
#' @details
#' The existing `.ext` / `.phi` are **not** migrated - the model must be
#' re-estimated. Only the control stream is rewritten.
#'
#' Whether `parameter` already exists in `$PK` decides what happens:
#'
#' **A new parameter** (no `$PK` assignment) gets a definition inserted just
#' before the FREM `MU_k = THETA(...)` block, delimited by
#' `;; Begin added THETA` / `;; End added THETA` so it is visually separate from
#' the FREM covariate block. With `j = numNonFREMThetas + 1` (the new theta) and
#' `k = numSkipOm + 1` (the new eta):
#' \describe{
#'   \item{`addEta = TRUE`, `muReference = TRUE`}{`TV<parameter> = THETA(<j>)`,
#'     `MU_<k> = LOG(TV<parameter>)`, `<parameter> = EXP(MU_<k> + ETA(<k>))` -
#'     log-normal and MU-referenced, the form the FREM models themselves use for
#'     `CL` / `V` / `MAT`.}
#'   \item{`addEta = TRUE`, `muReference = FALSE`}{`<parameter> = THETA(<j>) *
#'     EXP(ETA(<k>))` (log-normal, no MU line).}
#'   \item{`addEta = FALSE`}{`<parameter> = THETA(<j>)` - a structural parameter
#'     with no between-subject variability.}
#' }
#'
#' **An existing parameter** is modified in place instead: its `$PK` assignment
#' gains a `* THETA(<j>)` factor, and with `addEta = TRUE` also an
#' `* EXP(ETA(<k>))` term. Nothing is inserted, so no markers appear.
#'
#' The MU-referenced form deliberately goes through `TV<parameter>` rather than
#' writing `MU_<k> = THETA(<j>)` directly. [generateFremModel()] locates the
#' FREM block as everything from the first `MU_<n> = THETA(` line to the last
#' `COV<n> = MU_` line and regenerates it; a direct `MU_<k> = THETA(<j>)` here
#' would match that pattern, and being *before* the FREM block would move the
#' start of the replaced range - so a later [updateFREMmodel()] would silently
#' delete this definition. The `LOG(TV...)` form cannot collide.
#'
#' Markers are only ever placed in `$PK`, never in `$THETA` / `$OMEGA`, where
#' `generateFremModel()` reads comments positionally and a standalone comment
#' line would shift every subsequent parameter label.
#'
#' @param strFREMModel Path to the FREM model file, or a character vector of its
#'   lines.
#' @param thetaInit Initial value for the new `$THETA`. Required - a scalar
#'   (e.g. `0.1`), a length-3 numeric `c(low, init, up)`, or a string written
#'   verbatim after `$THETA` (e.g. `"(0, 1.2)"` or `"0.5 FIX"`).
#' @param parameter Name of the `$PK` variable the new theta belongs to. If it
#'   has no `$PK` assignment yet a definition is created for it; if it already
#'   exists, that assignment is modified in place. Required when
#'   `addEta = TRUE`; optional otherwise (with no `parameter` only the `$THETA`
#'   record is added and the caller wires it in by hand).
#' @param addEta Logical. Also add an accompanying IIV (via [addFremIIV()])?
#'   Default `FALSE`.
#' @param muReference Logical. Only used when `addEta = TRUE` - see Details.
#'   Default `TRUE`.
#' @param omegaInit Initial variance for the new `$OMEGA`. Required when
#'   `addEta = TRUE`.
#' @param label Comment for the new `$THETA` record. Defaults to
#'   `"TV_<parameter>"` or `"added structural THETA"`.
#' @param numNonFREMThetas,numSkipOm Structural integers. `NULL` (default) to
#'   derive them from the model + `.ext`; supply to override.
#' @param extFile Path to the `.ext` for the derivation. Defaults to the model
#'   path with a `.ext` extension.
#' @param newModel Output path when `strFREMModel` is a path and
#'   `bWriteMod = TRUE`. Defaults to the input path with `_theta` inserted before
#'   the extension.
#' @param bWriteMod Logical. Write the new model? Default `TRUE`.
#' @param quiet Logical. Suppress progress messages. Default `TRUE`.
#'
#' @return Invisibly, a list with `model` (character vector), `thetaIndex`,
#'   `etaIndex` (or `NA`), `numNonFREMThetas`, `numSkipOm` (both updated) and
#'   `file`.
#'
#' @seealso [addFremIIV()], [updateFREMmodel()]
#' @family FREM model management
#' @concept frem_model_management
#' @export
addFremStructuralTheta <- function(strFREMModel,
                                   thetaInit,
                                   parameter        = NULL,
                                   addEta           = FALSE,
                                   muReference      = TRUE,
                                   omegaInit        = NULL,
                                   label            = NULL,
                                   numNonFREMThetas = NULL,
                                   numSkipOm        = NULL,
                                   extFile          = NULL,
                                   newModel         = NULL,
                                   bWriteMod        = TRUE,
                                   quiet            = TRUE) {

  if (missing(thetaInit) || length(thetaInit) == 0L ||
      (is.numeric(thetaInit) && any(is.na(thetaInit)))) {
    stop("addFremStructuralTheta(): `thetaInit` is required - a scalar, a ",
         "length-3 c(low, init, up), or a verbatim string. No default: the ",
         "initial value is a modelling choice.", call. = FALSE)
  }
  if (addEta) {
    if (is.null(parameter) || !nzchar(parameter)) {
      stop("addFremStructuralTheta(): `parameter` is required when addEta = TRUE.",
           call. = FALSE)
    }
    if (is.null(omegaInit)) {
      stop("addFremStructuralTheta(): `omegaInit` is required when addEta = TRUE ",
           "(the initial variance of the new $OMEGA, e.g. 0.01).", call. = FALSE)
    }
  }

  isPath <- length(strFREMModel) == 1L && !grepl("\n", strFREMModel)
  lines  <- if (isPath) readLines(strFREMModel, warn = FALSE) else strFREMModel

  ## ---- structural integers ---------------------------------------------
  if (is.null(numNonFREMThetas) || is.null(numSkipOm)) {
    if (!isPath) {
      stop("addFremStructuralTheta(): pass `numNonFREMThetas` and `numSkipOm` ",
           "when `strFREMModel` is a character vector.", call. = FALSE)
    }
    if (is.null(extFile)) {
      extFile <- paste0(tools::file_path_sans_ext(strFREMModel), ".ext")
    }
    if (!file.exists(extFile)) {
      stop("addFremStructuralTheta(): supply `numNonFREMThetas` and `numSkipOm`, ",
           "or place ", basename(extFile), " next to the model.", call. = FALSE)
    }
    .info <- fremModelInfo(modFile = strFREMModel, dfext = extFile,
                           numNonFREMThetas = numNonFREMThetas,
                           numSkipOm        = numSkipOm)
    if (is.null(numNonFREMThetas)) numNonFREMThetas <- .info$numNonFREMThetas
    if (is.null(numSkipOm))        numSkipOm        <- .info$numSkipOm
    numTotThetas <- .info$numTotThetas
  } else {
    numTotThetas <- .fremCountTotTheta(lines)
  }

  jNew <- numNonFREMThetas + 1L

  ## ---- renumber THETA() refs >= jNew in $PK / $ERROR / $THETA ----------
  lines <- .fremRenumberTheta(lines, fromIdx = jNew, maxIdx = numTotThetas)

  ## ---- insert the new $THETA record ----------------------------------
  if (is.null(label)) {
    label <- if (!is.null(parameter)) paste0("TV_", parameter) else
      "added structural THETA"
  }
  thetaTxt <- if (is.character(thetaInit)) {
    thetaInit
  } else if (length(thetaInit) == 3L) {
    sprintf("(%s,%s,%s)", format(thetaInit[1]), format(thetaInit[2]), format(thetaInit[3]))
  } else {
    format(thetaInit[1])
  }
  newThetaLine <- sprintf("$THETA  %s ; %d. %s", thetaTxt, jNew, label)
  lines <- .fremInsertThetaRecord(lines, at = jNew, newLine = newThetaLine)

  numNonFREMThetas <- jNew
  etaIndex <- NA_integer_

  ## Does `parameter` already exist in $PK? That decides whether we are
  ## *creating* a new structural parameter (emit a definition block) or
  ## *modifying* an existing one (edit its assignment in place).
  isNew <- !is.null(parameter) && !.fremPkAssigns(lines, parameter)

  ## ---- optionally add the IIV + wire it in ---------------------------
  if (addEta) {
    iiv <- addFremIIV(lines,
                      parameter        = if (isNew) NULL else parameter,
                      omegaInit        = omegaInit,
                      link             = if (isNew) "none" else "exp",
                      numNonFREMThetas = numNonFREMThetas,
                      numSkipOm        = numSkipOm,
                      label            = paste0("IIV on ", parameter),
                      bWriteMod        = FALSE,
                      quiet            = TRUE)
    lines     <- iiv$model
    etaIndex  <- iiv$etaIndex
    numSkipOm <- iiv$numSkipOm

    if (isNew) {
      ## House style, as run31 writes CL/V/MAT:
      ##   TV<par> = THETA(j) ; MU_k = LOG(TV<par>) ; <par> = EXP(MU_k + ETA(k))
      ## Emitting `MU_k = THETA(j)` directly would match the
      ## `MU_\d+ = THETA` grep that generateFremModel() uses to locate the FREM
      ## block, and - sitting before that block - would make min(mu_indices)
      ## point here, so a later updateFREMmodel() would splice this definition
      ## away. The LOG(TV) form cannot collide.
      pkDef <- if (muReference) {
        c(sprintf("TV%s = THETA(%d)", parameter, jNew),
          sprintf("MU_%d = LOG(TV%s)", etaIndex, parameter),
          sprintf("%s = EXP(MU_%d + ETA(%d))", parameter, etaIndex, etaIndex))
      } else {
        sprintf("%s = THETA(%d) * EXP(ETA(%d))", parameter, jNew, etaIndex)
      }
      lines <- .fremInsertPkBlock(lines, pkDef)
    } else {
      ## existing parameter: addFremIIV() already attached the ETA; add the
      ## new THETA as a factor on the same assignment.
      lines <- .fremAttachEta_thetaFactor(lines, parameter = parameter,
                                          thetaIdx = jNew)
    }
  } else if (isNew) {
    ## A new structural parameter with no IIV.
    lines <- .fremInsertPkBlock(lines, sprintf("%s = THETA(%d)", parameter, jNew))
  } else if (!is.null(parameter)) {
    lines <- .fremAttachEta_thetaFactor(lines, parameter = parameter, thetaIdx = jNew)
  }

  ## ---- write ---------------------------------------------------------
  outFile <- NULL
  if (isPath && bWriteMod) {
    outFile <- if (!is.null(newModel)) newModel else {
      paste0(tools::file_path_sans_ext(strFREMModel), "_theta.",
             tools::file_ext(strFREMModel))
    }
    writeLines(lines, outFile)
  }

  if (!quiet) {
    message("addFremStructuralTheta(): inserted THETA(", jNew, ") (", label, ")",
            if (addEta) paste0(" + ETA(", etaIndex, ")") else "",
            "; numNonFREMThetas -> ", numNonFREMThetas,
            if (addEta) paste0(", numSkipOm -> ", numSkipOm) else "",
            if (!is.null(outFile)) paste0("; written to ", outFile) else "", ".")
  }

  invisible(list(model            = lines,
                 thetaIndex       = jNew,
                 etaIndex         = etaIndex,
                 numNonFREMThetas = numNonFREMThetas,
                 numSkipOm        = numSkipOm,
                 file             = outFile))
}


#' Largest THETA index referenced anywhere in a model's lines
#' @keywords internal
#' @noRd
.fremCountTotTheta <- function(lines) {
  m <- regmatches(lines, gregexpr("THETA\\(\\s*[0-9]+\\s*\\)", lines))
  idx <- as.integer(gsub("[^0-9]", "", unlist(m)))
  if (length(idx) == 0L) 0L else max(idx)
}


#' Shift THETA() indices >= fromIdx up by one, in $PK / $ERROR / $THETA only
#' @keywords internal
#' @noRd
.fremRenumberTheta <- function(lines, fromIdx, maxIdx) {
  if (maxIdx < fromIdx) return(lines)

  recStart <- grep("^\\s*\\$[A-Za-z]", lines)
  region   <- rep(FALSE, length(lines))
  for (s in recStart) {
    if (grepl("^\\s*\\$(PK|ERROR|PRED|THETA)\\b", lines[s], ignore.case = TRUE)) {
      e <- recStart[recStart > s]
      e <- if (length(e)) e[1] - 1L else length(lines)
      region[s:e] <- TRUE
    }
  }

  idx <- which(region)
  seg <- lines[idx]
  for (k in seq(maxIdx, fromIdx)) {
    seg <- gsub(sprintf("THETA\\(\\s*%d\\s*\\)", k), sprintf("THETA(%d)", k + 1L),
                seg, perl = TRUE)
  }
  lines[idx] <- seg
  lines
}


#' Insert a new $THETA record line after the (at-1)-th structural $THETA line
#'
#' Handles both one-`$THETA`-per-line models and continuation-line blocks: it
#' counts theta *values*, not `$THETA` keywords, and inserts after the
#' `(at-1)`-th value.
#' @keywords internal
#' @noRd
.fremInsertThetaRecord <- function(lines, at, newLine) {
  tStart <- grep("^\\s*\\$THETA\\b", lines, ignore.case = TRUE)
  if (length(tStart) == 0L) stop("No $THETA record found.", call. = FALSE)
  first <- tStart[1]

  # the $THETA region: from the first $THETA to the last line before a $ record
  # that is not itself $THETA (continuation lines do not start with $).
  recAfter <- grep("^\\s*\\$[A-Za-z]", lines)
  recAfter <- recAfter[recAfter > first]
  nonTheta <- recAfter[!grepl("^\\s*\\$THETA\\b", lines[recAfter], ignore.case = TRUE)]
  tEnd     <- if (length(nonTheta)) nonTheta[1] - 1L else length(lines)

  # count theta *values* across the whole region; insert after value (at-1)
  count <- 0L
  insertAfter <- tEnd
  for (i in first:tEnd) {
    body <- sub(";.*$", "", lines[i])
    body <- sub("^\\s*\\$THETA\\b", "", body, ignore.case = TRUE)
    n <- length(regmatches(body,
           gregexpr("\\([^)]*\\)|[-+]?[0-9.][-+0-9.eE]*", body))[[1]])
    count <- count + n
    if (count >= at - 1L) { insertAfter <- i; break }
  }
  c(lines[seq_len(insertAfter)], newLine,
    lines[(insertAfter + 1L):length(lines)])
}


#' Is `parameter` assigned anywhere in the model's $PK record?
#' @keywords internal
#' @noRd
.fremPkAssigns <- function(lines, parameter) {
  recStart <- grep("^\\s*\\$[A-Za-z]", lines)
  pkStart  <- recStart[grepl("^\\s*\\$PK\\b", lines[recStart], ignore.case = TRUE)]
  if (length(pkStart) == 0L) return(FALSE)
  pkEnd <- recStart[recStart > pkStart[1]]
  pkEnd <- if (length(pkEnd)) pkEnd[1] - 1L else length(lines)
  pat   <- sprintf("^\\s*%s\\s*=", .fremEscape(parameter))
  any(grepl(pat, lines[pkStart[1]:pkEnd]))
}


#' Insert a delimited block of $PK lines before the FREM MU/COV block
#'
#' Wrapped in `;; Begin added THETA` / `;; End added THETA` so the addition is
#' visually separated from the FREM covariate block, and followed by a blank
#' line. Markers are only ever placed in `$PK` - never in `$THETA` / `$OMEGA`,
#' where `generateFremModel()` reads comments positionally and a standalone
#' comment line would shift every subsequent label (see TODO T16).
#' @keywords internal
#' @noRd
.fremInsertPkBlock <- function(lines, newLines) {
  .fremInsertBeforeFremMuBlock(
    lines,
    c(";; Begin added THETA", newLines, ";; End added THETA", "")
  )
}


#' Insert lines into $PK immediately before the first `MU_k = THETA(...)` FREM line
#' @keywords internal
#' @noRd
.fremInsertBeforeFremMuBlock <- function(lines, newLines) {
  anchor <- grep("^\\s*MU_[0-9]+\\s*=\\s*THETA\\(", lines)
  if (length(anchor) == 0L) {
    # no FREM MU block (unusual) - fall back to just before $ERROR
    anchor <- grep("^\\s*\\$ERROR\\b", lines, ignore.case = TRUE)
    if (length(anchor) == 0L) stop("Could not find an anchor in $PK to insert ",
                                   "the new parameter definition.", call. = FALSE)
  }
  a    <- anchor[1]
  pad  <- sub("\\S.*$", "", lines[a])          # match the anchor's indentation
  at   <- a - 1L
  # do not pad blank separator lines into whitespace-only lines
  padded <- ifelse(nzchar(newLines), paste0(pad, newLines), newLines)
  c(lines[seq_len(at)], padded, lines[(at + 1L):length(lines)])
}


#' Append ` * THETA(j)` to a parameter's $PK assignment (addEta = FALSE path)
#' @keywords internal
#' @noRd
.fremAttachEta_thetaFactor <- function(lines, parameter, thetaIdx) {
  pat <- sprintf("^(\\s*)%s(\\s*)=(\\s*)(.*)$", .fremEscape(parameter))
  recStart <- grep("^\\s*\\$[A-Za-z]", lines)
  pkStart  <- recStart[grepl("^\\s*\\$PK\\b", lines[recStart], ignore.case = TRUE)]
  hit <- grep(pat, lines)
  if (length(pkStart)) {
    pkEnd <- recStart[recStart > pkStart[1]]
    pkEnd <- if (length(pkEnd)) pkEnd[1] - 1L else length(lines)
    hit   <- hit[hit >= pkStart[1] & hit <= pkEnd]
  }
  if (length(hit) != 1L) {
    stop("addFremStructuralTheta(): expected exactly one $PK assignment of '",
         parameter, "' to attach THETA(", thetaIdx, ") to; found ", length(hit),
         ".", call. = FALSE)
  }
  i <- hit[1]
  m <- regmatches(lines[i], regexec(pat, lines[i]))[[1]]
  lead <- m[2]; rhsAll <- m[5]
  cpos <- regexpr(";", rhsAll, fixed = TRUE)
  if (cpos > 0) {
    rhs <- sub("\\s+$", "", substr(rhsAll, 1, cpos - 1))
    comment <- substr(rhsAll, cpos, nchar(rhsAll))
  } else { rhs <- sub("\\s+$", "", rhsAll); comment <- "" }
  lines[i] <- sprintf("%s%s = (%s) * THETA(%d)%s", lead, parameter, rhs, thetaIdx,
                      if (nzchar(comment)) paste0("  ", comment) else "")
  lines
}
