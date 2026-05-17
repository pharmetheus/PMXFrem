#' Generate and Format Covariate Labels for Forest Plots
#'
#' @description 
#' Dynamically constructs covariate names from a forest plot covariate data frame.
#' It features an interactive two-step workflow: run with `print_template = TRUE`
#' to generate a boilerplate mapping script in the console, then fill it out and
#' pass the maps back in to append units or replace categorical labels.
#'
#' @param dfCovs A data frame containing covariate scenarios (must have a `COVARIATEGROUPS` column).
#' @param unit_map A named character vector for appending units (e.g., `c(WT = "kg")`).
#' @param label_map A named character vector for exact replacements (e.g., `c("SEX=1" = "Male")`).
#' @param print_template Logical. If TRUE, prints an R script template to the console and exits.
#'
#' @return A character vector of formatted covariate names suitable for the `cdfCovsNames` argument.
#' 
#' @family Diagnostics & Plotting Internal
#' @concept diagnostics
#' @keywords internal
#' @export
#'
#' @examples
#' # Create a synthetic forest plot covariate dataframe using -99 for missing
#' dfCovs <- data.frame(
#'   COVARIATEGROUPS = c("WT", "WT", "SEX", "SEX", "RACEL", "RACEL", "RACEL"),
#'   WT = c(70, 90, -99, -99, -99, -99, -99),
#'   SEX = c(-99, -99, 1, 2, -99, -99, -99),
#'   RACEL_2 = c(-99, -99, -99, -99, 0, 1, 0),
#'   RACEL_3 = c(-99, -99, -99, -99, 0, 0, 1)
#' )
#' 
#' # --- Step 1: Generate the Template ---
#' # Run this in your console to get the boilerplate:
#' generateCovNames(dfCovs, print_template = T)
#' 
#' # --- Step 2: Fill and Apply ---
#' # Paste the output, fill in the blanks, and pass to the function:
#' unit_map <- c(
#'   WT = "kg"
#' )
#' 
#' label_map <- c(
#'   "SEX=1" = "Male",
#'   "SEX=2" = "Female",
#'   "RACEL=1" = "White (Ref)"
#' )
#' 
#' generateCovNames(dfCovs, unit_map = unit_map, label_map = label_map)
generateCovNames <- function(dfCovs, unit_map = NULL, label_map = NULL, print_template = FALSE) {
  if (!"COVARIATEGROUPS" %in% names(dfCovs)) {
    stop("dfCovs must contain a 'COVARIATEGROUPS' column.", call. = FALSE)
  }
  
  # 1. Extract Base Strings and Groups
  groups <- as.character(dfCovs$COVARIATEGROUPS)
  base_strings <- sapply(seq_len(nrow(dfCovs)), function(i) {
    grp <- groups[i]
    
    if (grp %in% names(dfCovs)) {
      return(paste0(grp, "=", dfCovs[[grp]][i]))
    }
    
    pattern <- paste0("^", grp, "_")
    dummy_cols <- grep(pattern, names(dfCovs), value = TRUE)
    
    if (length(dummy_cols) == 0) {
      warning(paste("No matching columns found for covariate group:", grp), call. = FALSE)
      return(paste0(grp, "=UNKNOWN"))
    }
    
    suffixes <- as.numeric(gsub(pattern, "", dummy_cols))
    row_vals <- as.numeric(dfCovs[i, dummy_cols, drop = FALSE])
    
    if (any(row_vals == 1, na.rm = TRUE)) {
      val <- suffixes[which(row_vals == 1)[1]]
    } else {
      val <- min(suffixes, na.rm = TRUE) - 1
    }
    return(paste0(grp, "=", val))
  })
  
  # 2. Template Generation Mode
  if (print_template) {
    u_groups <- unique(groups)
    u_base <- unique(base_strings)
    
    cat("# Copy and paste this mapping template into your script:\n\n")
    cat("unit_map <- c(\n")
    cat(paste0("  ", u_groups, " = \"\""), sep = ",\n")
    cat("\n)\n\n")
    
    cat("label_map <- c(\n")
    cat(paste0("  \"", u_base, "\" = \"\""), sep = ",\n")
    cat("\n)\n")
    
    return(invisible(NULL))
  }
  
  # 3. Application Mode
  res <- sapply(seq_along(base_strings), function(i) {
    b <- base_strings[i]
    g <- groups[i]
    
    # Label Replacement (Categorical) takes precedence
    if (!is.null(label_map) && b %in% names(label_map)) {
      if (label_map[[b]] != "") return(label_map[[b]])
    }
    
    # Unit Appending (Continuous)
    if (!is.null(unit_map) && g %in% names(unit_map)) {
      if (unit_map[[g]] != "") return(paste0(b, " ", unit_map[[g]]))
    }
    
    return(b)
  })
  
  return(as.character(res))
}