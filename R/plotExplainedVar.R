#' plotExplainedVar
#'
#' @param dfres A data.frame with data to plot. Typically obtained by \code{getExplainedVar}.
#' @param parameters A vector with the names of the parameters to include in the plot. The parameter names must be present in the \code{dfres$PARAMETER}.
#' @param parameterLabels A character vector of alternative parameter labels.
#' Should either have the same length as \code{parameters} or the same length as the number of rows in \code{dfres}.
#' @param covariateLabels A character vector of alternative covariate labels. Should have the same length as the number of rows in \code{dfres}.
#' @param labelfun A label function compatible with \code{labeller}. Used to format \code{parameterLabels} used as row facet labels in the plot.
#' @param fill_col The fill color of the bars.
#' @param maxVar maxVar=1 (default): Visualize the explained part of the total variability. maxVar=2: Visualize the explained part of the explainable variability.
#' @param xlb X-axis title.
#'
#' @details
#' With maxVar=1 the explained variability of the covariate (combination) in each row of dfres (COVVAR) is divided by the value in TOTVAR and multiplied by 100.
#' With maxVar=2 the explained variability of the covariate (combination) in each row of dfres (COVVAR) is divided by the value in TOTCOVVAR and multiplied by 100.
#' @return A plot that illustrates the explained variability
#' @export
#'
#' @examples
#' \dontrun{
#' plotExplainedVar(dfres0)
#' }
plotExplainedVar <- function(dfres,
                             parameters=unique(dfres$PARAMETER),
                             parameterLabels=NULL,
                             covariateLabels=NULL,
                             labelfun=label_value,
                             fill_col="#508791",
                             maxVar = 1,
                             xlb = ifelse(maxVar == 1,
                                          "Explained part of total variability (%)",
                                          "Explained part of explainable variability (%)")) {

  ## Input checks
  if(!(maxVar == 1 | maxVar ==2)) {
    stop("maxVar needs to be 1 or 2")
  }

  if(!is.null(parameterLabels)) {
    if(!((length(parameterLabels) == length(parameters)) |
         (length(parameterLabels) ==nrow(dfres)))) {
      stop("The number of parameter labels must either be the same as the number of parameters or have the same length as the number of rows in dfres.")
    }
  }

  if(!is.null(covariateLabels)) {
    if(!((length(covariateLabels) == length(unique(dfres$COVNAME))) |
         (length(covariateLabels)  == nrow(dfres)))) {
      stop("The number of group name labels must either be the same as the number of unique values in COVNAME or have the same length as the number of rows in dfres.")
    }
  }

  if(!all(parameters %in% dfres$PARAMETER)) {
    stop("All parameters must be present in dfres$PARAMETER")
  }

  ## Filter the parameters to use
  dfres <- dfres %>% filter(PARAMETER %in% parameters)

  ## Name the PARAMETER column
  if(!is.null(parameterLabels)) {
    dfres$PARAMETERLABEL <- parameterLabels
  } else {
    dfres$PARAMETERLABEL <- dfres$PARAMETER
  }

  # Retain order
  dfres$PARAMETERLABEL <- factor(dfres$PARAMETERLABEL,levels=unique(dfres$PARAMETERLABEL[order(dfres$PARAMETER)]))

  ## Name the COVNAME column
  if(!is.null(covariateLabels)) {

    if(length(covariateLabels) == length(unique(dfres$COVNAME))) {
      names(covariateLabels) <- unique(dfres$COVNAME)

      dfres$COVNAMELABEL <- dfres$COVNAME

      for (gName in unique(dfres$COVNAME)) {
        dfres$COVNAMELABEL <- ifelse(dfres$COVNAME == gName,covariateLabels[gName],dfres$COVNAMELABEL)
      }

    } else {
      dfres$COVNAMELABEL <- covariateLabels
    }

  } else {
    dfres$COVNAMELABEL <- dfres$COVNAME
  }

  ## Compute the fraction to plot
  dfres <- dfres %>%
    rowwise() %>%
    mutate(Frac = ifelse(maxVar == 1,100*COVVAR/TOTVAR,100*COVVAR/TOTCOVVAR))

  ## Reorder the covariates according to FRAC
  dfres$COVNAMELABEL <- reorder(dfres$COVNAMELABEL,dfres$Frac,FUN=mean)


  ##
  if(compareVersion(as.character(packageVersion("ggplot2")),"3.3-0") <0) { # If ggplot version < 3.3.0
    p1 <- ggplot(dfres, aes(x=COVNAMELABEL, y=Frac)) +
      geom_bar(position="dodge", stat="identity",fill=fill_col) +
      coord_flip() +
      facet_wrap(~PARAMETERLABEL,scales = "free_x",labeller = labeller(PARAMETERLABEL= labelfun)) +
      ylab(xlb) +
      xlab("")
  } else {
    p1 <- ggplot(data=dfres,aes(x=Frac,y=COVNAMELABEL)) +
      geom_bar(position="dodge", stat="identity",fill=fill_col) +
      facet_wrap(~PARAMETERLABEL,scales = "free_x",labeller = labeller(PARAMETERLABEL= labelfun)) +
      xlab(xlb) +
      ylab("")
  }

  return(p1)
}

