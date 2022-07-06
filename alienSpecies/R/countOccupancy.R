


#' Create occupancy bar chart
#' @param df data.frame as created by \code{\link{createOccupancyData}}
#' @param nSquares integer, total number of squares for calculating percentages
#' @return list with plotly object and data.frame
#' 
#' @author mvarewyck
#' @import plotly
#' @export
countOccupancy <- function(df, nSquares = 370) {
  
  p <- plot_ly(data = df, x = ~t0/nSquares*100, y = ~species, name = "baseline",
      type = "bar", orientation = "h") %>%
    add_trace(x = ~t1/nSquares*100, name = "rapportage") %>%
    layout(xaxis = list(title = 'Percentage bezette hokken (10km2) in Vlaanderen'),
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
      
      output$titleOccupancy <- renderUI({
          
          h3(HTML(uiText()[uiText()$plotFunction == "countOccupancy", ]$title))
          
        })
      
      plotModuleServer(id = "occupancy",
        plotFunction = "countOccupancy", 
        data = data
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
      
      helpText("NOTE: Plot independent of the selected filters above"),
      
      plotModuleUI(id = ns("occupancy"), height = "800px"),
      optionsModuleUI(id = ns("plotTrias"), doWellPanel = FALSE),
      tags$hr()
    
    )
  )
  
}