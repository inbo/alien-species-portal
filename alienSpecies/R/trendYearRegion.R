#' Create interactive plot for counts per selected communes and years
#' 
#' @param df data.frame with raw data for plotting
#' @param uiText data.frame,  
#' @param combine logical, summarised view of selected regions
#' @param period numeric vector, time range selected for plot
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given species the observed number 
#' per year and per selected commune is plotted in a line plot}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'afschotjaar': }{year at which the animals was shot}
#' \item{'locatie': }{comune name}
#' \item{'aantal' or 'aantal/100ha': }{absolute or relative counts of animals}
#' }
#' }
#' }
#' @import plotly
#' @importFrom stats aggregate
#' @export
trendYearRegion <- function(df, uiText = NULL,
  combine = FALSE, period = NULL) {
  
  
  # To prevent warnings with R CMD check
  locatie <- NULL
  
  unit <- attr(df, "unit")
  unitName <- translate(uiText, unit)$title
  
  if (is.null(period))
    period <- range(df$year)
  
  # Select data
  plotData <- df
  colorList <- replicateColors(nColors = length(unique(df$region)))
  
  # expected to be missing for some region levels
  regionText <- suppressWarnings(translate(uiText, df$region)$title)  
  title <- paste(vectorToTitleString(regionText), yearToTitleString(period))
  
  if (combine) {
    plotData <- plotData[, c("year", "outcome")]
    plotData <- aggregate(outcome ~ year, plotData, sum)
    plotData$region <- translate(uiText, "total")$title
  } else plotData$region <- regionText
  
  # Display year without decimals
  plotData$year <- as.character(plotData$year)
  
  # Filter NA's
  plotData <- plotData[!is.na(plotData$outcome), ]
    
  # Create plot
  pl <- plot_ly(data = plotData, x = ~year, y = ~outcome,
      color = ~region, colors = colorList$colors,
#      line = list(dash = ~group),
      hoverinfo = "x+y+name",
      type = "scatter", mode = "lines+markers") %>%
    layout(title = title,
      xaxis = list(title = translate(uiText, "year")$title),
      yaxis = list(title = unitName,
        range = c(~min(outcome)*1.05, ~max(outcome)*1.05)),
      showlegend = TRUE,
      margin = list(b = 80, t = 100))     
  
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  # change variable names
  names(plotData)[names(plotData) == "outcome"] <- unitName
  
  
  return(list(
      plot = pl, 
      data = plotData[, c("region", "year", unitName)], 
      warning = if (!is.null(colorList$warning))
        translate(uiText, colorList$warning)
    ))
  
}