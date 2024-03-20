#' Summarize Vespa Velutina data for the plot \code{countYearGroup}
#' 
#' @param df data.frame with nesten data 
#' @param gewest character, which region(s) to filter on
#' @return data.table with summarized nesten data, as input for \code{codeYearGroup}
#' 
#' @author mvarewyck
#' @importFrom dplyr mutate group_by summarise rename filter
#' @importFrom data.table as.data.table
#' @importFrom sf st_drop_geometry
#' @export
summarizeYearGroupData <- function(df, gewest) {
  
  # For R CMD check
  result <- GEWEST <- observation_time <- NULL
  
  toReturn <- df %>%
    st_drop_geometry() %>%
    mutate(year = as.integer(format(observation_time, "%Y")),
      result = ifelse(is.na(result), "onbekend", result)) %>%
    filter(GEWEST %in% gewest) %>%
    group_by(year, result, GEWEST) %>%
    summarise(count = n()) %>% 
    rename(Behandeling = result)  %>%
    as.data.table()  
  
  toReturn
  
}


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
  plotData <- df[!is.na(count), c("year", groupVar, "count"), with = FALSE]
  
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
        yaxis = list(title = translate(uiText, summarizeBy)$title),
        legend = list(title = if (!is.null(groupVar)) list(text = translate(uiText, groupVar)$title)),
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
#' @inheritParams plotModuleServer
#' @inheritParams mapCubeServer
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countYearGroupServer <- function(id, uiText, data, groupChoices, dashReport = NULL) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      tmpTranslation <- reactive(translate(uiText(), ns("countYearGroup")))
      
      output$titleCountYearGroup <- renderUI(h3(HTML(tmpTranslation()$title)))
      
      output$descriptionCountYearGroup <- renderUI(HTML(tmpTranslation()$description))
      
      
      # Plot
      plotResult <- plotModuleServer(id = "countYearGroup",
        plotFunction = "countYearGroup", 
        data = reactive({
            validate(need(nrow(data()) > 0, translate(uiText(), "noData")$title))
            data()
          }),
        groupChoices = groupChoices,
        uiText = uiText
      )
      
      
      ## Report Objects ##
      ## -------------- ##
      
      observe({
          
          # Update when any of these change
          req(plotResult())
          
          # Return the static values
          dashReport[[ns("countYearGroup")]] <- isolate({
              c(plotResult(), 
                list(
                  title = tmpTranslation()$title, 
                  description = tmpTranslation()$description)
              )
            })
          
        })
      
      
      return(dashReport)
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{countYearGroup}} - UI side
#' @inheritParams plotModuleUI
#' @inheritParams plotTriasUI
#' 
#' @author mvarewyck
#' @export
countYearGroupUI <- function(id, showPlotDefault = FALSE) {
  
  ns <- NS(id)
  
  tagList(
    
    actionLink(inputId = ns("linkCountYearGroup"), 
      label = uiOutput(ns("titleCountYearGroup"))),
    conditionalPanel(paste("input.linkCountYearGroup % 2 ==",(as.numeric(showPlotDefault) + 1) %% 2), 
      ns = ns,
      
      uiOutput(ns("descriptionCountYearGroup")),
      
      optionsModuleUI(id = ns("countYearGroup"), showSummary = TRUE),
      plotModuleUI(id = ns("countYearGroup")),
      tags$hr()
    
    )
  )
  
}