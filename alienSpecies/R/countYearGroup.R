
#' Create interactive plot for counts per group category and year
#' 
#' @inheritParams countOccupancy
#' @param groupVar character, variable in \code{data} that should server as group variable
#' @param summarizeBy character, how to summarize counts over groups
#' 
#' @return list with plotly object and data.frame#' 
#' 
#' @import plotly
#' @importFrom data.table setnames copy
#' @importFrom INBOtheme inbo_palette
#' @export
countYearGroup <- function(df, groupVar = "", uiText = NULL,
  summarizeBy = c("sum", "cumsum")) {
  
  # For R CMD check
  count <- NULL
  group <- NULL
  
  summarizeBy <- match.arg(summarizeBy)
  
  if (groupVar == "")
    groupVar <- NULL
  
  # Select data
  plotData <- df[, c("year", groupVar, "count"), with = FALSE]
  
  setkey(plotData, year)
  
  # Total count per year
  totalCount <- copy(plotData)
  totalCount <- if (summarizeBy == "sum")
      totalCount[, .(count = sum(count)), by = year] else if (summarizeBy == "cumsum")
      totalCount[, count := cumsum(count)][, .SD[.N], by = year][, .(year, count)]
  
  
  if (is.null(groupVar)) {
    
    summaryData <- totalCount
    
    summaryData$group <- "all"
    colors <- inbo_palette(3)[1]
    names(colors) <- "all"
        
  } else {
    
    data.table::setnames(plotData, groupVar, "group")
    setkey(plotData, year, group)
    
    # Summarize data per year and group category
    summaryData <- if (summarizeBy == "sum")
      plotData[, .(count = sum(count)), by = .(year, group)] else if (summarizeBy == "cumsum")
      plotData[, count := cumsum(count), by = group][, .SD[.N], by = .(year, group)]
    
    groupLevels <- unique(summaryData$group)
    colors <- inbo_palette(max(3, length(groupLevels)))
    names(colors) <- groupLevels
    
  }
  
  
  
  
  # Create plot
  toPlot <- plot_ly(data = summaryData, x = ~year, 
        y = ~count, color = ~group, hoverinfo = "text+name",
        colors = colors, type = "bar") %>%
      layout(
        xaxis = list(title = translate(uiText, "year")$title), 
        yaxis = list(title = translate(uiText, "count")$title),
        legend = list(title = list(translate(uiText, "group")$title)),
        barmode = if (is.null(groupVar) || length(groupLevels) == 1) "group" else "stack",
        annotations = list(x = totalCount$year, 
          y = totalCount$count, 
          text = totalCount$count,
          xanchor = 'center', yanchor = 'bottom',
          showarrow = FALSE)
      )
  
  # To prevent warnings in UI
  toPlot$elementId <- NULL
  
  
  return(list(plot = toPlot, data = summaryData))
  
}



#' Shiny module for creating the plot \code{\link{countYearGroup}} - server side
#' @inheritParams countOccupancyServer
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countYearGroupServer <- function(id, uiText, data) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Plot
      plotModuleServer(id = "countYearGroup",
        plotFunction = "countYearGroup", 
        data = data,
        uiText = uiText
        )
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{countYearGroup}} - UI side
#' @template moduleUI
#' 
#' @author mvarewyck
#' @export
countYearGroupUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
     
      optionsModuleUI(id = ns("countYearGroup"), showGroup = TRUE, showSummary = TRUE,
        showGewest = TRUE),
      plotModuleUI(id = ns("countYearGroup")),
      tags$hr()
    
  )
  
}