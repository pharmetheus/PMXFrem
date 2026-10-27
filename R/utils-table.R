#' Carry $TABLE records into a FREM model
#'
#' A FREM data set holds fewer columns than the base model's, so a base
#' model's `$TABLE` can list data items NM-TRAN would no longer find. Each
#' `$TABLE` record keeps its model items (`CWRES`, `ETAS(1:LAST)`, `$PK`
#' variables) and options; only data items that were in `oldInput` and are not
#' in `newInput` are removed - from the item list and from `BY` / `EXCLUDE_BY`
#' (NONMEM help, `$TABLE`), which go too when they lose all their items.
#' `FREMTYPE` is added after the last item if missing, and `FILE=` can be
#' renamed. The records are rewritten one item list per record, wrapped.
#'
#' @param lines The model's lines.
#' @param oldInput The data item labels the table could refer to before.
#' @param newInput The data item labels of the FREM data set.
#' @param oldSynonyms Named character vector: for an `$INPUT` `label=synonym`
#'   item, the synonym (name) and the column name the data sets use (value).
#'   A table token for either name is that item: it goes with the item, and
#'   when kept is written as the column name, the only one the new `$INPUT`
#'   has.
#' @param renameFile A function from the old file name to the new one, or
#'   `NULL` to keep it.
#' @param quiet Suppress the message naming the removed items.
#' @return The lines with the `$TABLE` records rewritten.
#' @keywords internal
#' @noRd
.fremCarryTables <- function(lines, oldInput, newInput, renameFile = NULL, quiet = FALSE,
                             oldSynonyms = NULL) {
  starts <- grep("^\\s*\\$TAB", lines, ignore.case = TRUE)
  if (length(starts) == 0) {
    return(lines)
  }
  records <- grep("^\\s*\\$", lines)
  gone <- setdiff(toupper(oldInput), toupper(newInput))
  removed <- character(0)
  out <- character(0)
  pos <- 1
  for (s in starts) {
    e <- c(records[records > s], length(lines) + 1)[1] - 1
    words <- strsplit(trimws(gsub(";[^\n]*", "", paste(lines[s:e], collapse = " "))), "\\s+")[[1]]
    keyword <- words[1]
    words <- words[-1]

    ## A synonym stands for its item.
    isSyn <- toupper(words) %in% toupper(names(oldSynonyms)) & !toupper(words) %in% toupper(oldInput)
    asItem <- words
    asItem[isSyn] <- oldSynonyms[match(toupper(words[isSyn]), toupper(names(oldSynonyms)))]
    drop <- toupper(asItem) %in% gone
    removed <- c(removed, words[drop])
    words <- asItem[!drop]
    ## Both names of an item in one table list it once.
    viaSyn <- isSyn[!drop]
    words <- words[!(duplicated(toupper(words)) & (viaSyn | toupper(words) %in% toupper(words[viaSyn])))]
    ## A BY or EXCLUDE_BY left without items goes too.
    isOption <- function(w) grepl("=", w) | toupper(w) %in% .fremTableOptions
    for (kw in c("BY", "EXCLUDE_BY")) {
      at <- which(toupper(words) == kw)
      for (k in rev(at)) {
        if (k == length(words) || isOption(words[k + 1]) || toupper(words[k + 1]) %in% c("BY", "EXCLUDE_BY")) {
          words <- words[-k]
        }
      }
    }
    if (!"FREMTYPE" %in% toupper(words)) {
      ## After the item list: before the first option or BY / EXCLUDE_BY.
      opt <- which(isOption(words) | toupper(words) %in% c("BY", "EXCLUDE_BY"))
      at <- if (length(opt) > 0) opt[1] - 1 else length(words)
      words <- append(words, "FREMTYPE", after = at)
    }
    if (!is.null(renameFile)) {
      f <- grep("^FILE=", words, ignore.case = TRUE)
      words[f] <- paste0("FILE=", vapply(sub("^FILE=", "", words[f], ignore.case = TRUE), renameFile, ""))
    }
    if (s > pos) out <- c(out, lines[pos:(s - 1)])
    out <- c(out, .fremWrapRecord(keyword, words))
    pos <- e + 1
  }
  if (pos <= length(lines)) out <- c(out, lines[pos:length(lines)])
  if (length(removed) > 0 && !quiet) {
    message(
      "$TABLE: left out ", paste(unique(removed), collapse = ", "),
      ", which the FREM data set does not carry. Add them to cstrKeepCols to keep them."
    )
  }
  out
}

## $TABLE options that are not items (NONMEM 7.5 help, $TABLE). Options
## written KEY=value are recognised by their "=".
.fremTableOptions <- c(
  "PRINT", "NOPRINT", "NOHEADER", "ONEHEADER", "ONEHEADERALL", "NOTITLE", "NOLABEL",
  "FIRSTONLY", "LASTONLY", "FIRSTLASTONLY", "NOFORWARD", "FORWARD", "APPEND", "NOAPPEND",
  "WRESCHOL", "UNCONDITIONAL", "CONDITIONAL", "OMITTED"
)

#' A record keyword and its words, wrapped at about 80 characters
#' @keywords internal
#' @noRd
.fremWrapRecord <- function(keyword, words) {
  out <- character(0)
  cur <- keyword
  for (w in words) {
    if (nchar(cur) + 1 + nchar(w) > 80) {
      out <- c(out, cur)
      cur <- paste0(strrep(" ", nchar(keyword) + 1), w)
    } else {
      cur <- paste(cur, w)
    }
  }
  c(out, cur)
}

#' The new name for a table file
#'
#' `xptab30` becomes `xptab31` when the base run is run30 and the FREM model
#' run31 - the run number swapped, as Xpose expects; any other name gets
#' `_<fremModName>` appended.
#'
#' @param file The table file name. @param baseRunNo The base model's run
#'   number, or `NULL`. @param fremModName The FREM model's name.
#' @keywords internal
#' @noRd
.fremTableFileName <- function(file, baseRunNo, fremModName) {
  fremRunNo <- sub("^run([0-9]+)$", "\\1", fremModName)
  if (!is.null(baseRunNo) && grepl("^run[0-9]+$", fremModName) && grepl(paste0(baseRunNo, "$"), file)) {
    return(sub(paste0(baseRunNo, "$"), fremRunNo, file))
  }
  paste0(file, "_", fremModName)
}
