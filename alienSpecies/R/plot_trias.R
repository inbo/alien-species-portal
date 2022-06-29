
#' Generic function to call plot/table function from the trias package
#' 
#' @inheritParams welcomeSectionServer
#' @param triasFunction character, plot function to be called from trias package
#' @param df data.frame see e.g. \code{\link[trias]{visualize_pathways_level1}}
#' @param triasArgs list, extra arguments to be passed to the trias plot function
#' @param outputType character, type of output to be displayed;
#' should be one of \code{"plot", "table"}
#' @return list with
#' \itemize{
#' \item{plot}{ggplotly object, only available if \code{outputType} is "plot"}
#' \item{data}{data.frame used for the plot}
#' }
#' 
#' @author mvarewyck
#' @importFrom plotly ggplotly
#' @export
plotTrias <- function(triasFunction, df, triasArgs = NULL,
  outputType = c("plot", "table"), uiText) {
  
  
  outputType <- match.arg(outputType)
  
  plotArgs <- list(
    df = df
#    start_year_plot = min(df$first_observed, na.rm = TRUE) - 1,
#    x_lab = "Jaar",
#    y_lab = "Aantal ge\u00EFntroduceerde uitheemse soorten"
  )
  if (!is.null(triasArgs))
    plotArgs <- c(plotArgs, triasArgs)
  
  resultFct <- do.call(triasFunction, plotArgs)
  
  ## convert to plotly object
  if (outputType == "plot") {
    
    if (all(c("plot", "data_top_graph") %in% names(resultFct))) {
      
      list(
        plot = ggplotly(resultFct$plot), 
        data = resultFct$data_top_graph
      ) 
      
    } else {

      list(
        plot = ggplotly(resultFct),
        data = df
      )
    
    }
    
  } else {
    
    list(
      data = resultFct, 
      columnNames = displayName(colnames(resultFct), translations = uiText)
    )
    
  }
  
}



#' Shiny module for creating the plot \code{\link{plotTrias}} - server side
#' @inheritParams welcomeSectionServer
#' @inheritParams plotTrias
#' @param data reactive object, data for \code{\link{plotTrias}}
#' @param triasArgs reactive object, extra plot arguments to be passed to the 
#' trias package
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import trias
#' @export
plotTriasServer <- function(id, uiText, data, triasFunction, triasArgs = NULL,
  outputType = c("plot", "table")) {
  
  
  outputType <- match.arg(outputType)
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      subText <- reactive({
          uiText()[uiText()$plotFunction == triasFunction, ]
        })
      
      output$titlePlotTrias <- renderUI({
          
          h3(HTML(subText()$title))
          
        })
      
      plotModuleServer(id = "plotTrias",
        plotFunction = "plotTrias",
        triasFunction = triasFunction, 
        data = data,
        triasArgs = if (!is.null(triasArgs)) triasArgs else NULL,
        outputType = outputType,
        uiText = uiText
      )
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{plotTrias}} - UI side
#' @template moduleUI
#' @inheritParams plotTrias
#' @author mvarewyck
#' @export
plotTriasUI <- function(id, outputType = c("plot", "table")) {
  
  ns <- NS(id)
  outputType <- match.arg(outputType)
  
  tagList(
    
    actionLink(inputId = ns("linkPlotTrias"), 
      label = uiOutput(ns("titlePlotTrias"))),
    conditionalPanel("input.linkPlotTrias % 2 == 1", ns = ns,
      
      if (outputType == "plot")
          plotModuleUI(id = ns("plotTrias")) else
          tableModuleUI(id = ns("plotTrias")),
      optionsModuleUI(id = ns("plotTrias"), doWellPanel = FALSE),
      tags$hr()
    
    )
  )
  
}