


#' Create occupancy bar chart
#' @param df data.frame as created by \code{\link{createOccupancyData}}
#' @param nSquares integer, total number of squares for calculating percentages
#' @inheritParams countYearNativerange
#' @return list with plotly object and data.frame
#' 
#' @author mvarewyck
#' @import plotly
#' @export
countOccupancy <- function(df, nSquares = 370, uiText = NULL) {
  
  p <- plot_ly(data = df, x = ~t0/nSquares*100, y = ~species, 
      name = translate(uiText, "baseline")$title,
      type = "bar", orientation = "h") %>%
    add_trace(x = ~t1/nSquares*100, name = translate(uiText, "reporting")$title) %>%
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