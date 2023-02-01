


#' Create occupancy bar chart
#' @param df data.frame as created by \code{\link{createOccupancyData}}
#' @param nSquares integer, total number of squares for calculating percentages
#' @inheritParams countYearNativerange
#' @return list with plotly object and data.frame
#' 
#' @author mvarewyck
#' @import plotly
#' @importFrom INBOtheme inbo_palette
#' @export
countOccupancy <- function(df, nSquares = 370, uiText = NULL) {
  
  plotData <- melt(df[, c("species", "t0", "t1")], id.vars = "species")
  levels(plotData$variable) <- c(
    translate(uiText, "baseline")$title, 
    translate(uiText, "reporting")$title)
  
  colors <- inbo_palette(3)[-1]
  names(colors) <- unique(plotData$variable)
  
  p <- plot_ly(data = plotData, x = ~value/nSquares*100, y = ~species,
      color = ~variable, colors = colors, type = "bar", orientation = "h") %>%
    layout(xaxis = list(title = translate(uiText, 'percentCages')$title),
      yaxis = list(title = ""), barmode = 'group')
  
  
  return(list(plot = p, data = df))
  
}


#' Shiny module for creating the plot \code{\link{countOccupancy}} - server side
#' @inheritParams welcomeSectionServer
#' @param data reactive object, data for \code{\link{countOccupancy}}
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countOccupancyServer <- function(id, uiText, data) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      tmpTranslation <- reactive(translate(uiText(), "countOccupancy"))
      
      output$descriptionOccupancy <- renderUI(HTML(tmpTranslation()$description))
      
      output$titleOccupancy <- renderUI(h3(HTML(tmpTranslation()$title)))
          
      
      plotModuleServer(id = "occupancy",
        plotFunction = "countOccupancy", 
        data = data,
        uiText = uiText
      )
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{countOccupancy}} - UI side
#' @template moduleUI
#' 
#' @author mvarewyck
#' @export
countOccupancyUI <- function(id) {
  
  ns <- NS(id)
  
  
  tagList(
    
    actionLink(inputId = ns("linkOccupancy"), 
      label = uiOutput(ns("titleOccupancy"))),
    conditionalPanel("input.linkOccupancy % 2 == 1", ns = ns,
      
      uiOutput(ns("descriptionOccupancy")),
      
      plotModuleUI(id = ns("occupancy"), height = "800px"),
      optionsModuleUI(id = ns("occupancy"), doWellPanel = FALSE),
      tags$hr()
    
    )
  )
  
}