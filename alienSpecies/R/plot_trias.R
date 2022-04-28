
#' Shiny module for creating the plot \code{\link{countIntroductionYear}} - server side
#' @inheritParams welcomeSectionServer
#' @inheritParams countIntroductionYear
#' @param data reactive object, data for \code{\link{countIntroductionYear}}
#' @param region reactive object, selected regions
#' @param time reactive object, selected years 
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import trias
#' @export
plotTriasServer <- function(id, uiText, data, plotFunction) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      subText <- reactive({
          uiText[uiText$plotFunction == plotFunction, ]
        })
      
      output$titlePlotTrias <- renderUI({
          h3(HTML(subText()$title))
          
        })
      
      plotModuleServer(id = "plotTrias",
        plotFunction = plotFunction, 
        data = data
      )
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{countIntroductionYear}} - UI side
#' @template moduleUI
#' 
#' @author mvarewyck
#' @export
plotTriasUI <- function(id) {
  
  ns <- NS(id)
  
  
  tagList(
    
    actionLink(inputId = ns("linkPlotTrias"), 
      label = uiOutput(ns("titlePlotTrias"))),
    conditionalPanel("input.linkPlotTrias % 2 == 1", ns = ns,
      
      plotModuleUI(id = ns("plotTrias")),
      tags$hr()
    
    )
  )
  
}