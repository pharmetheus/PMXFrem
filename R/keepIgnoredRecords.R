#' Keep every record in the FREM data set and re-apply the filters in $DATA
#'
#' The last step of [createFREMmodel()] with `keepIgnoredRecords = TRUE`. The
#' FREM data set and model are first made as usual; this rewrites them so that
#' the data set holds every record of the original data set, and the `$DATA`
#' record leaves out what the usual data set leaves out:
#'
#' * the `IGNORE` lists of the base model, as it has them, on the columns they
#'   use (kept in the data set, `=DROP` in `$INPUT`). A covariate record takes
#'   the values of those columns from the first record of its subject that the
#'   lists keep, or from the first record when they keep none, so it is kept
#'   exactly when its subject is.
#' * a proxy for the rest: the column `FREMEXCL`, 1 on an observation with a
#'   missing DV and on every record of a subject with no observations at all,
#'   left out by `IGNORE=(FREMEXCL.EQN.1)`. An `ACCEPT`
#'   list of the base model goes into the proxy too, since NONMEM allows no
#'   `ACCEPT` list next to an `IGNORE` list (NONMEM help, `$DATA`).
#' * `FREMEXCL` 2 on every record of a subject whose observations the
#'   `IGNORE` lists all leave out, and `IGNORE=(FREMEXCL.EQN.2)`: its dose and
#'   covariate records pass the lists. A sensitivity run deletes that line
#'   along with the conditions it edits.
#'
#' Subjects that the usual data set leaves out altogether get covariate
#' records, from a run on the data set without the filters (`rerun`), so that
#' a sensitivity analysis that brings them back has their covariates. The
#' result is checked: the new `$DATA` applied to the new data set must give
#' the usual data set, record for record; otherwise this stops.
#'
#' @param baseModelFile The base model file.
#' @param dataFile The original data set, a path or a data frame.
#' @param finalModelPath,finalDataPath The FREM model and data set, as made
#'   without the option; both are rewritten.
#' @param rerun A function of one argument, a directory holding a copy of the
#'   base model (`base.mod`, `.ext`, ...) without `IGNORE` / `ACCEPT` lists, that
#'   runs [createFREMmodel()] on it and returns its result.
#' @param strID The subject column. @param missVal The missing value.
#' @param keepDoseOnlySubjects As in [createFREMmodel()].
#' @param quiet Suppress messages.
#' @return Nothing; the files are rewritten.
#' @keywords internal
#' @noRd
.fremKeepIgnoredRecords <- function(baseModelFile, dataFile, finalModelPath, finalDataPath, rerun,
                                    strID = "ID", missVal = -99, keepDoseOnlySubjects = FALSE, quiet = FALSE) {
  baseLines <- readLines(baseModelFile, warn = FALSE)
  items <- .fremInputItems(findrecord(baseLines, record = "^\\$INP(U(T)?)?\\s", quiet = TRUE))
  synonyms <- .fremInputSynonyms(items)
  filterLists <- .fremFilterLists(findrecord(baseLines, record = "\\$DATA", quiet = TRUE))

  ## --- The original data set, its columns named by position as $INPUT names them
  if (is.data.frame(dataFile)) {
    num <- as.data.frame(dataFile)
    raw <- as.data.frame(lapply(num, as.character), stringsAsFactors = FALSE, optional = TRUE)
  } else {
    num <- data.table::fread(dataFile, header = TRUE, data.table = FALSE)
    raw <- .readDataAsText(dataFile)
  }
  num <- num[, seq_len(nrow(items)), drop = FALSE]
  raw <- raw[, seq_len(nrow(items)), drop = FALSE]
  names(num) <- names(raw) <- items$name

  ## The columns the conditions use, by the name the conditions use for them.
  condLabel <- vapply(filterLists$conditions, `[[`, "", "label")
  condCol <- ifelse(condLabel %in% names(num), condLabel, unname(synonyms[condLabel]))
  filterCols <- unique(condCol)
  filterLabel <- condLabel[match(filterCols, condCol)]

  ## --- Which original records the usual data set has (FREMTYPE 0)
  passBase <- .fremFilterKeep(baseModelFile, num, quiet = TRUE, rawData = raw, synonyms = synonyms)
  ids <- num[[strID]]
  hasEvid <- "EVID" %in% names(num)
  evid <- if (hasEvid) num$EVID else rep(0, nrow(num))
  subjOK <- if (keepDoseOnlySubjects || !hasEvid) rep(TRUE, nrow(num)) else ids %in% unique(ids[passBase & evid == 0])
  dvOK <- if (hasEvid) num$DV != missVal | evid != 0 else num$DV != missVal
  dvOK[is.na(dvOK)] <- FALSE
  inD0 <- passBase & subjOK & dvOK

  dfD <- utils::read.csv(finalDataPath)
  cols <- names(dfD)
  isCovD <- dfD$FREMTYPE > 0
  if (sum(inD0) != sum(!isCovD)) {
    stop("keepIgnoredRecords: the FREM data set has ", sum(!isCovD), " records of the original data set, ",
      "but the filters keep ", sum(inD0), ". Please report this, with the base model.",
      call. = FALSE
    )
  }

  ## --- Where each covariate record of the usual data set goes: after the
  ## original record it follows there, or before the first kept record of its
  ## subject when it follows none of them.
  origOfD <- integer(nrow(dfD))
  origOfD[!isCovD] <- which(inD0)
  firstKept <- tapply(which(inD0), ids[inD0], min)
  covKey <- numeric(0)
  last <- NA_integer_
  for (i in seq_len(nrow(dfD))) {
    if (!isCovD[i]) {
      last <- origOfD[i]
    } else {
      sameSubject <- !is.na(last) && ids[last] == dfD[[strID]][i]
      covKey <- c(covKey, if (sameSubject) 2 * last + 1 else 2 * firstKept[[as.character(dfD[[strID]][i])]] - 1)
    }
  }
  covRows <- dfD[isCovD, , drop = FALSE]

  ## --- Covariate records for the subjects the usual data set leaves out
  outIds <- setdiff(unique(ids), unique(dfD[[strID]]))
  extra <- .fremCovRecordsFor(outIds, rerun, baseModelFile, finalModelPath, cols, strID, quiet)
  firstRow <- tapply(seq_along(ids), ids, min)
  extraKey <- 2 * firstRow[as.character(extra[[strID]])] + 1

  ## --- The new data set, in the order of the original records
  rows0 <- num[, setdiff(cols, "FREMTYPE"), drop = FALSE]
  rows0$FREMTYPE <- 0
  rows0 <- rows0[, cols, drop = FALSE]
  dfF <- rbind(rows0, covRows, extra)
  key <- c(2 * seq_len(nrow(num)), covKey, extraKey)
  inD <- c(inD0, rep(TRUE, nrow(covRows)), rep(FALSE, nrow(extra)))
  src <- c(seq_len(nrow(num)), .fremFilterSource(c(covRows[[strID]], extra[[strID]]), ids, passBase))
  ord <- order(key, seq_along(key))
  dfF <- dfF[ord, , drop = FALSE]
  inD <- inD[ord]
  src <- src[ord]

  ## The columns the conditions use, as written in the original data.
  extraCols <- setdiff(filterCols, cols)
  for (cc in extraCols) dfF[[cc]] <- raw[[cc]][src]

  ## --- The proxy
  passF <- if (is.null(filterLists$kind) || filterLists$kind == "ACCEPT") {
    rep(TRUE, nrow(dfF))
  } else {
    wide <- dfF
    text <- as.data.frame(lapply(dfF, as.character), stringsAsFactors = FALSE, optional = TRUE)
    for (cc in extraCols) text[[cc]] <- raw[[cc]][src]
    .fremFilterKeep(baseModelFile, wide, quiet = TRUE, rawData = text, synonyms = synonyms)
  }
  ## 1: left out whatever the conditions say - an observation with a missing
  ## DV, a subject with no observations at all, and with an ACCEPT list
  ## everything the list leaves out. A subject is left out whole, so that a
  ## sensitivity run cannot bring back part of it.
  ## 2: a subject with observations, all of which the IGNORE conditions leave
  ## out. Its dose and covariate records pass the conditions, so it needs the
  ## proxy too, on a line of its own that a sensitivity run deletes with them.
  dvOKF <- c(dvOK, rep(TRUE, nrow(covRows) + nrow(extra)))[ord]
  recOK <- c(passBase & dvOK, rep(TRUE, nrow(covRows) + nrow(extra)))[ord]
  subjAny <- if (keepDoseOnlySubjects || !hasEvid) rep(TRUE, nrow(num)) else ids %in% unique(ids[evid == 0])
  subjNone <- unique(ids[!subjAny])
  subjCond <- setdiff(unique(ids[!subjOK]), subjNone)
  editable <- identical(filterLists$kind, "IGNORE")
  excl1 <- !dvOKF | (passF & !recOK) | dfF[[strID]] %in% subjNone | (!editable & dfF[[strID]] %in% subjCond)
  excl2 <- !excl1 & editable & dfF[[strID]] %in% subjCond
  fremExcl <- ifelse(excl1, 1L, ifelse(excl2, 2L, 0L))
  useProxy <- any(fremExcl > 0)
  nProxy <- any(fremExcl == 1) + any(fremExcl == 2)
  if (length(filterLists$conditions) + nProxy > 100) {
    stop("keepIgnoredRecords: NONMEM allows up to 100 IGNORE / ACCEPT conditions, and the base model ",
      "has ", length(filterLists$conditions), " before the ", nProxy, " this option adds.",
      call. = FALSE
    )
  }
  if (useProxy) dfF$FREMEXCL <- fremExcl
  rownames(dfF) <- NULL
  utils::write.csv(dfF, finalDataPath, row.names = FALSE, quote = FALSE)

  ## --- The model: $INPUT gains the dropped columns, $DATA the conditions
  mod <- readLines(finalModelPath, warn = FALSE)
  inputNow <- .fremInputItems(findrecord(mod, record = "^\\$INP(U(T)?)?\\s", quiet = TRUE))$name
  if (!identical(inputNow, cols)) {
    stop("keepIgnoredRecords: the $INPUT record of the FREM model does not name the columns of its data set.",
      call. = FALSE
    )
  }
  ## paste0() of a zero-length vector gives a bare "=DROP", so only when there
  ## are columns to add.
  dropItems <- c(
    if (length(extraCols)) paste0(filterLabel[match(extraCols, filterCols)], "=DROP"),
    if (useProxy) "FREMEXCL=DROP"
  )
  mod <- findrecord(mod,
    record = "^\\$INP(U(T)?)?\\s",
    replace = c(paste("$INPUT", paste(cols, collapse = " ")), if (length(dropItems)) paste("      ", paste(dropItems, collapse = " "))),
    quiet = TRUE
  )
  dataRec <- findrecord(mod, record = "\\$DATA", quiet = TRUE)
  newData <- dataRec[1]
  if (identical(filterLists$kind, "IGNORE")) {
    newData <- c(
      newData,
      "; The IGNORE conditions of the base model. Edit these for a sensitivity analysis.",
      paste("      ", filterLists$lists)
    )
  } else if (identical(filterLists$kind, "ACCEPT")) {
    newData <- c(
      newData,
      "; The base model has the ACCEPT list below, which NONMEM cannot combine with an",
      "; IGNORE list. FREMEXCL carries it, with the records left out for other reasons.",
      paste(";", filterLists$lists)
    )
  }
  if (any(fremExcl == 1)) {
    newData <- c(
      newData,
      "; FREMEXCL = 1: a record the FREM data set leaves out for another reason (a subject",
      "; without observations, an observation with a missing DV).",
      "       IGNORE=(FREMEXCL.EQN.1)"
    )
  }
  if (any(fremExcl == 2)) {
    newData <- c(
      newData,
      "; FREMEXCL = 2: a subject whose observations the IGNORE conditions above all leave out.",
      "; Delete this line too when an edit of those conditions should bring such subjects back.",
      "       IGNORE=(FREMEXCL.EQN.2)"
    )
  }
  mod <- findrecord(mod, record = "\\$DATA", replace = newData, quiet = TRUE)
  writeLines(mod, finalModelPath)

  ## --- The check: NONMEM must see the usual data set
  seen <- .fremSeenByNonmem(finalModelPath, finalDataPath)
  if (!isTRUE(all.equal(seen, dfD, check.attributes = FALSE))) {
    stop("keepIgnoredRecords: the $DATA record of the new FREM model does not leave out exactly ",
      "what the usual FREM data set leaves out. Please report this, with the base model; ",
      "createFREMmodel() without keepIgnoredRecords works as before.",
      call. = FALSE
    )
  }
  if (!quiet) {
    message(
      "keepIgnoredRecords: the FREM data set keeps all ", nrow(num), " records of the original data set; ",
      "$DATA leaves out ", nrow(dfF) - nrow(dfD), " of its ", nrow(dfF), " records."
    )
  }
  invisible(NULL)
}

#' The original record each covariate record takes the filter columns from
#'
#' The first record of the subject that the filters keep, or its first
#' record when they keep none.
#' @param covIds The subject of each covariate record.
#' @param ids The subject of each original record. @param passBase Whether
#'   the filters keep each original record.
#' @return One original row number per covariate record.
#' @keywords internal
#' @noRd
.fremFilterSource <- function(covIds, ids, passBase) {
  firstPass <- tapply(seq_along(ids)[passBase], ids[passBase], min)
  firstAny <- tapply(seq_along(ids), ids, min)
  k <- as.character(covIds)
  out <- firstPass[k]
  out[is.na(out)] <- firstAny[k[is.na(out)]]
  as.integer(out)
}

#' Covariate records for subjects the filters leave out
#'
#' From a run of [createFREMmodel()] on a copy of the base model without its
#' `IGNORE` / `ACCEPT` lists. When that run fails, or names its covariates
#' differently (a level present only among the records left out), there are
#' none, with a warning.
#' @return A data frame with the columns `cols`, possibly with no rows.
#' @keywords internal
#' @noRd
.fremCovRecordsFor <- function(outIds, rerun, baseModelFile, finalModelPath, cols, strID, quiet) {
  none <- utils::read.csv(text = paste(cols, collapse = ","))
  if (length(outIds) == 0) {
    return(none)
  }
  td <- tempfile("frem-all-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  stem <- tools::file_path_sans_ext(baseModelFile)
  for (ext in c(".ext", ".lst", ".phi", ".cov")) {
    if (file.exists(paste0(stem, ext))) file.copy(paste0(stem, ext), file.path(td, paste0("base", ext)))
  }
  baseLines <- readLines(baseModelFile, warn = FALSE)
  dataRec <- findrecord(baseLines, record = "\\$DATA", quiet = TRUE)
  stripped <- gsub("(IGNORE|ACCEPT)\\s*=?\\s*\\([^)]*\\)", "", sub(";.*$", "", dataRec), ignore.case = TRUE)
  writeLines(findrecord(baseLines, record = "\\$DATA", replace = stripped, quiet = TRUE), file.path(td, "base.mod"))

  res <- tryCatch(rerun(td), error = function(e) e)
  if (inherits(res, "error")) {
    warning("keepIgnoredRecords: the ", length(outIds), " subjects the filters leave out get no covariate ",
      "records, because the FREM data set without the filters could not be made: ", conditionMessage(res),
      call. = FALSE
    )
    return(none)
  }
  if (!identical(getCovNames(res$model)$covNames, getCovNames(finalModelPath)$covNames)) {
    warning("keepIgnoredRecords: the ", length(outIds), " subjects the filters leave out get no covariate ",
      "records, because their data give the covariates other levels than the data the filters keep.",
      call. = FALSE
    )
    return(none)
  }
  all <- utils::read.csv(res$data)
  out <- all[all$FREMTYPE > 0 & all[[strID]] %in% outIds, , drop = FALSE]
  out[, cols, drop = FALSE]
}

#' The records NONMEM reads from a model and its data set
#'
#' The `IGNORE` / `ACCEPT` lists of `$DATA` applied, as [filterDataFromModel()]
#' applies them, and the `=DROP` columns removed.
#' @param modelPath The model. @param dataPath Its data set.
#' @return A data frame.
#' @keywords internal
#' @noRd
.fremSeenByNonmem <- function(modelPath, dataPath) {
  items <- .fremInputItems(findrecord(modelPath, record = "^\\$INP(U(T)?)?\\s", quiet = TRUE))
  num <- utils::read.csv(dataPath)
  raw <- .readDataAsText(dataPath)
  names(num) <- names(raw) <- items$name
  keep <- .fremFilterKeep(modelPath, num, quiet = TRUE, rawData = raw, synonyms = .fremInputSynonyms(items))
  out <- num[keep, !items$dropped, drop = FALSE]
  rownames(out) <- NULL
  out
}
