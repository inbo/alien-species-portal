
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
#' @param data reactive object, data for \code{\link{plotTrias}}
#' @param triasArgs reactive object, extra plot arguments to be passed to the 
#' trias package
#' @param filters character vector, additional filters for the TRIAS plot to 
#' be dipslayed
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import trias
#' @export
plotTriasServer <- function(id, uiText, data, triasFunction, triasArgs = NULL,
  filters = NULL, outputType = c("plot", "table")) {
  
  # For R CMD check
  protected <- NULL
  
  outputType <- match.arg(outputType)
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      tmpTranslation <- reactive(translate(uiText(), triasFunction))
      
      output$titlePlotTrias <- renderUI(h3(HTML(tmpTranslation()$title)))
      
      output$descriptionPlotTrias <- renderUI(HTML(tmpTranslation()$description))
      
      
      output$filters <- renderUI({
          
          if (!is.null(filters)) 
            lapply(filters, function(iFilter) {
                checkboxInput(inputId = ns(iFilter), label = switch(iFilter,
                    bias = translate(uiText(), "correctBias")$title,
                    protected = translate(uiText(), "protectAreas")$title)
                )
              })
        })
      
      output$regionFilter <- renderUI({
          
          choices <- c("flanders", "wallonia", "brussels")
          names(choices) <- translate(uiText(), choices)$title
          
          selectInput(inputId = ns("region"), label = translate(uiText(), "regions"),
            choices = choices, multiple = TRUE, selected = choices)
          
        })
      
      plotData <- reactive({
          
          subData <- data()
          
          if (!is.null(input$region))
            # only for GAM
            subData <- summarizeTimeSeries(rawData = subData, region = input$region)
          
          if (!is.null(input$protected))
            subData <- subData[protected == input$protected, ]
          
          subData
          
        })
      
      plotModuleServer(id = "plotTrias",
        plotFunction = "plotTrias",
        triasFunction = triasFunction, 
        data = plotData,
        triasArgs = reactive({
            if (!is.null(triasArgs)) {
              initArgs <- triasArgs()
              if (!is.null(input$bias)) {
                initArgs$eval_years <- min(plotData()$year):max(plotData()$year)
                if (input$bias)
                  initArgs$baseline_var <- "cobs"
              }
              initArgs
            } else NULL
          }),
        outputType = outputType,
        uiText = uiText
      )
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{plotTrias}} - UI side
#' @template moduleUI
#' @inheritParams plotTrias
#' @author mvarewyck
#' @import shiny
#' @export
plotTriasUI <- function(id, outputType = c("plot", "table"), showRegion = FALSE) {
  
  ns <- NS(id)
  outputType <- match.arg(outputType)
  
  tagList(
    
    actionLink(inputId = ns("linkPlotTrias"), 
      label = uiOutput(ns("titlePlotTrias"))),
    conditionalPanel("input.linkPlotTrias % 2 == 1", ns = ns,
      
      uiOutput(ns("descriptionPlotTrias")),
      wellPanel(
        fluidRow(
          column(6, uiOutput(ns("filters"))),
          if (showRegion)
            column(6, uiOutput(ns("regionFilter")))
        )
      ),
      
      if (outputType == "plot")
          plotModuleUI(id = ns("plotTrias")) else
          tableModuleUI(id = ns("plotTrias")),
      optionsModuleUI(id = ns("plotTrias"), doWellPanel = FALSE),
      tags$hr()
    
    )
  )
  
}