
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
#' @importFrom INBOtheme theme_inbo
#' @export
plotTrias <- function(triasFunction, df, triasArgs = NULL,
  outputType = c("plot", "table"), uiText) {
  
  
  outputType <- match.arg(outputType)
  
  plotArgs <- list(df = df)
  
  if (!is.null(triasArgs))
    plotArgs <- c(plotArgs, triasArgs)
  
  resultFct <- suppressWarnings(do.call(triasFunction, plotArgs))
  
  ## convert to plotly object
  if (outputType == "plot") {
    
    if (all(c("plot", "data_top_graph") %in% names(resultFct))) {
      
      list(
        plot = ggplotly(resultFct$plot + INBOtheme::theme_inbo(transparent = TRUE)), 
        data = resultFct$data_top_graph
      ) 
      
    } else if (all(c("plot", "output") %in% names(resultFct))) {
      
      list(
        plot = ggplotly(resultFct$plot + INBOtheme::theme_inbo(transparent = TRUE)), 
        data = resultFct$output
      )
      
    } else if (all(c("static_plot", "data") %in% names(resultFct))) {
      
      list(
        plot = ggplotly(resultFct$static_plot + INBOtheme::theme_inbo(transparent = TRUE)), 
        data = resultFct$data
      ) 
      
    } else {

      list(
        plot = ggplotly(resultFct + INBOtheme::theme_inbo(transparent = TRUE)),
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
#' @inheritParams mapCubeServer
#' @param data reactive object, data for \code{\link{plotTrias}}
#' @param triasArgs reactive object, extra plot arguments to be passed to the 
#' trias package
#' @param filters character vector, additional filters for the TRIAS plot to 
#' be dipslayed
#' @param maxDate reactive date, maximum observation date for printing in description
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import trias
#' @export
plotTriasServer <- function(id, uiText, data, triasFunction, triasArgs = NULL,
  filters = NULL, maxDate = reactive(NULL), outputType = c("plot", "table"),
  dashReport = NULL) {
  
  # For R CMD check
  protected <- NULL
  
  outputType <- match.arg(outputType)
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      tmpTranslation <- reactive(translate(uiText(), triasFunction))
      
      output$titlePlotTrias <- renderUI(h3(HTML(tmpTranslation()$title)))
      
      output$descriptionPlotTrias <- renderUI({
          
          HTML(
            decodeText(text = tmpTranslation()$description,
              params = list(maxDate = format(maxDate(), "%d/%m/%Y")))
          )
          
        })
      
      
      output$filters <- renderUI({
          
          if (!is.null(filters)) 
            wellPanel(
              lapply(filters, function(iFilter) {
                  checkboxInput(inputId = ns(iFilter), 
                    label = translate(uiText(), iFilter)$title)
                })
            )
          
        })
      
      
      plotData <- reactive({
          
          subData <- data()
          
          if (!is.null(input$protectAreas))
            subData <- subData[protected == input$protectAreas, ]
          
          subData
          
        })
      
      plotResult <- plotModuleServer(id = "plotTrias",
        plotFunction = "plotTrias",
        triasFunction = triasFunction, 
        data = plotData,
        triasArgs = reactive({
            if (!is.null(triasArgs)) {
              initArgs <- triasArgs()
              initArgs$eval_years <- min(plotData()$year):max(plotData()$year)
              if (!is.null(input$correctBias) && input$correctBias)
                initArgs$baseline_var <- "cobs"
              initArgs
            } else NULL
          }),
        outputType = outputType,
        uiText = uiText
      )
      
      
      ## Report Objects ##
      ## -------------- ##
      
      observe({
          
          # Update when any of these change
          plotResult()
          input
          
          # Return the static values
          dashReport[[triasFunction]] <- c(
            list(plot = isolate(plotResult())),
            isolate(reactiveValuesToList(input))
          )
          
        })
      
      
      return(dashReport)
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{plotTrias}} - UI side
#' @param showPlotDefault boolean, whether to show the plot by default;
#' default value is FALSE, i.e. plot hidden in conditionalPanel()
#' @inheritParams plotModuleUI
#' @inheritParams plotTrias
#' @author mvarewyck
#' @import shiny
#' @export
plotTriasUI <- function(id, outputType = c("plot", "table"), showPlotDefault = FALSE) {
  
  ns <- NS(id)
  outputType <- match.arg(outputType)
  
  tagList(
    
    actionLink(inputId = ns("linkPlotTrias"), 
      label = uiOutput(ns("titlePlotTrias"))),
    conditionalPanel(paste("input.linkPlotTrias % 2 ==", (as.numeric(showPlotDefault) + 1) %% 2), 
      ns = ns,
      
      uiOutput(ns("descriptionPlotTrias")),
      uiOutput(ns("filters")),
      
      if (outputType == "plot")
          plotModuleUI(id = ns("plotTrias")) else
          tableModuleUI(id = ns("plotTrias")),
      optionsModuleUI(id = ns("plotTrias"), doWellPanel = FALSE),
      tags$hr()
    
    )
  )
  
}