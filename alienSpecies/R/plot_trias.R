
#' Generic function to call plot function from the trias package
#' 
#' @param triasFunction character, plot function to be called 
#' @param df data.frame see e.g. \code{\link[trias]{visualize_pathways_level1}}
#' @param triasArgs list, extra arguments to be passed to the trias plot function
#' @return list with
#' \itemize{
#' \item{plot}{ggplotly object}
#' \item{data}{data.frame used for the plot}
#' }
#' 
#' @author mvarewyck
#' @importFrom plotly ggplotly
#' @export
plotTrias <- function(triasFunction, df, triasArgs = NULL) {
  
  plotArgs <- list(
    df = df
#    start_year_plot = min(df$first_observed, na.rm = TRUE) - 1,
#    x_lab = "Jaar",
#    y_lab = "Aantal ge\u00EFntroduceerde uitheemse soorten"
  )
  if (!is.null(triasArgs))
    plotArgs <- c(plotArgs, triasArgs)
  
  myPlot <- do.call(triasFunction, plotArgs)
  
  ## convert to plotly object
  p <- ggplotly(myPlot)
  
  
  return(list(plot = p, data = df))
  
}



#' Shiny module for creating the plot \code{\link{plotTrias}} - server side
#' @inheritParams welcomeSectionServer
#' @inheritParams countIntroductionYear
#' @param data reactive object, data for \code{\link{plotTrias}}
#' @param triasFunction character, plot function to be called from trias package
#' @param triasArgs reactive object, extra plot arguments to be passed to the 
#' trias package
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import trias
#' @export
plotTriasServer <- function(id, uiText, data, triasFunction, triasArgs = NULL) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      subText <- reactive({
          uiText[uiText$plotFunction == triasFunction, ]
        })
      
      output$titlePlotTrias <- renderUI({
          h3(HTML(subText()$title))
          
        })
      
      plotModuleServer(id = "plotTrias",
        plotFunction = "plotTrias",
        triasFunction = triasFunction, 
        data = data,
        triasArgs = if (!is.null(triasArgs)) triasArgs else NULL
      )
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{plotTrias}} - UI side
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
      optionsModuleUI(id = ns("plotTrias"), doWellPanel = FALSE),
      tags$hr()
    
    )
  )
  
}