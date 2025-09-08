#' Parse a Base NONMEM Model for FREM Conversion
#'
#' @return A list containing structural information.
#'
#' @keywords internal
parseBaseModel <- function(baseModelFile, numSkipOm, muInsertLineUser = NULL, fremBlockInsertLineUser = NULL) {
  
  # # (Internal helper .parse_param_block remains the same)
  # .parse_param_block <- function(block) {
  #   if (is.null(block)) return(list(count = 0, lines = character(0)))
  #   
  #   # Clean the block by removing the record name (e.g., $THETA) and comments
  #   clean_lines <- sapply(block, function(line) {
  #     line <- gsub("(?i)^\\$\\w+\\s+", "", line, perl = TRUE)
  #     line <- gsub(";.*", "", line)
  #     return(line)
  #   }, USE.NAMES = FALSE)
  #   
  #   full_string <- paste(clean_lines, collapse = " ")
  #   
  #   # Use a robust regex to find all numbers (including decimals and scientific notation)
  #   # This is not dependent on fragile whitespace or parenthesis parsing.
  #   number_regex <- "[+-]?([0-9]*[.])?[0-9]+([eE][+-]?[0-9]+)?"
  #   matches <- gregexpr(number_regex, full_string)
  #   
  #   # Count the number of matches found
  #   num_estimates <- if (matches[[1]][1] == -1) 0 else length(matches[[1]])
  #   
  #   return(list(count = num_estimates, lines = block))
  # }
  .find_block_end <- function(lines, block_name) {
    block_start <- grep(paste0("^\\", block_name), lines)[1]
    if (is.na(block_start)) {
      stop(paste0("Could not find a ", block_name, " block in the base model."), call. = FALSE)
    }
    if (block_start == length(lines)) {
      return(length(lines))
    }
    next_dollar_index <- grep("^\\$", lines[(block_start + 1):length(lines)])[1]
    if (is.na(next_dollar_index)) {
      return(length(lines))
    } else {
      return(block_start + next_dollar_index - 1)
    }
  }
  
  modelLines <- readLines(baseModelFile)
  
  # thetaInfo <- .parse_param_block(findrecord(modelLines, record = "\\$THETA", quiet = TRUE))
  thetaBlock <- findrecord(modelLines, record = "\\$THETA", quiet = TRUE)
  thetaInfo <- list(count = length(thetaBlock), lines = thetaBlock)
  omegaBlock <- findrecord(modelLines, record = "\\$OMEGA", quiet = TRUE)
  sigmaBlock <- findrecord(modelLines, record = "\\$SIGMA", quiet = TRUE) # Find SIGMA
  
  if (thetaInfo$count == 0) stop("Could not find any initial estimates in the $THETA block.")
  if (is.null(omegaBlock)) stop("Could not find an $OMEGA block in the base model.")
  
  lastThetaLine <- grep("^\\$THETA", modelLines)[1] + length(thetaInfo$lines) - 1
  
  muInsertLine <- if (!is.null(muInsertLineUser)) {
    muInsertLineUser
  } else {
    .find_block_end(modelLines, block_name = "$PK")
  }
  
  fremBlockInsertLine <- if (!is.null(fremBlockInsertLineUser)) {
    fremBlockInsertLineUser
  } else {
    .find_block_end(modelLines, block_name = "$ERROR")
  }
  
  diag_lines <- omegaBlock[!grepl("SAME", omegaBlock, ignore.case = TRUE)]
  if (length(diag_lines) == 0) stop("Could not find any diagonal OMEGA records.")
  
  # (Rest of OMEGA validation logic remains the same)
  block_indices <- grep("BLOCK", diag_lines, ignore.case = TRUE)
  if (length(block_indices) == 0) {
    stop(paste0("The base model's $OMEGA structure is not suitable for FREM.\n",
                "The OMEGAs to be used in FREM must be defined in a final '$OMEGA BLOCK(N)' record."),
         call. = FALSE)
  }
  last_block_declaration_index <- max(block_indices)
  if (last_block_declaration_index < length(diag_lines)) {
    subsequent_lines <- diag_lines[(last_block_declaration_index + 1):length(diag_lines)]
    if (any(grepl("^\\$OMEGA", subsequent_lines, ignore.case = TRUE))) {
      stop(paste0("The base model's $OMEGA structure is not suitable for FREM.\n",
                  "The '$OMEGA BLOCK(N)' record must be the final non-SAME OMEGA definition. ",
                  "Simple $OMEGA records were found after the final BLOCK."),
           call. = FALSE)
    }
  }
  last_block_record <- diag_lines[last_block_declaration_index]
  numParCov <- as.numeric(gsub(".*BLOCK\\((\\d+)\\).*", "\\1", last_block_record))
  num_preceding_omegas <- 0
  if (last_block_declaration_index > 1) {
    preceding_lines <- diag_lines[1:(last_block_declaration_index - 1)]
    for (line in preceding_lines) {
      clean_line <- gsub("(?i)^\\$\\w+\\s+", "", line, perl = TRUE)
      clean_line <- gsub(";.*", "", clean_line)
      items <- strsplit(clean_line, "\\s+")[[1]]
      items <- items[nchar(items) > 0]
      num_numeric_items <- sum(grepl("^[+-]?([0-9]*[.])?[0-9]+([eE][+-]?[0-9]+)?$", items))
      num_preceding_omegas <- num_preceding_omegas + num_numeric_items
    }
  }
  if (num_preceding_omegas != numSkipOm) {
    stop(paste0("The number of simple OMEGAs (", num_preceding_omegas, ") before the final OMEGA BLOCK ",
                "does not match the specified numSkipOm (", numSkipOm, ")."),
         call. = FALSE)
  }
  total_omegas <- numSkipOm + numParCov
  
  # FIX: Add omegaBlock and sigmaBlock to the return list
  return(list(
    modelLines = modelLines,
    numThetas = thetaInfo$count,
    numOmegas = total_omegas,
    numParCov = numParCov,
    omegaBlock = omegaBlock,
    sigmaBlock = sigmaBlock,
    numSkipOm  = numSkipOm,
    lastThetaLine = lastThetaLine,
    muInsertLine = muInsertLine,
    fremBlockInsertLine = fremBlockInsertLine
  ))
}
