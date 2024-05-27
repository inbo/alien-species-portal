
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
#' @importFrom plotly ggplotly layout
#' @importFrom INBOtheme theme_inbo
#' @export
plotTrias <- function(triasFunction, df, triasArgs = NULL,
  outputType = c("plot", "table"), uiText) {
  
  
  outputType <- match.arg(outputType)
  
  plotArgs <- list(df = df)
  
  if (!is.null(triasArgs)) {
    if ("region" %in% names(triasArgs)) {
      selectedRegions <- triasArgs$region
      triasArgs$region <- NULL
    }
    plotArgs <- c(plotArgs, triasArgs)
  }
  
  resultFct <- suppressWarnings(do.call(triasFunction, plotArgs))
  
  ## convert to plotly object
  if (outputType == "plot") {
    
    if (all(c("plot", "data_top_graph") %in% names(resultFct))) {
      
      list(
        plot = ggplotly(resultFct$plot + INBOtheme::theme_inbo(transparent = TRUE)), 
        data = resultFct$data_top_graph
      ) 
      
    } else if (all(c("plot", "output") %in% names(resultFct))) {
      
      myPlot <- ggplotly(resultFct$plot + INBOtheme::theme_inbo(transparent = TRUE))
      
      if (triasFunction == "apply_gam") {
        
        newLabels <- sapply(3:0, function(i)
          uiText$title[uiText$id == paste0("gam_", i)])
        names(newLabels) <- as.character(3:0)
        
        # update title
        myPlot <- myPlot %>% plotly::layout(title = paste0(
            triasArgs$y_label, " GAM - ", triasArgs$name, " (", triasArgs$taxon_key, ") - ",
            paste(c(if (!is.null(triasArgs$baseline_var))
              translate(uiText, "correctBias")$title,
            if (all(resultFct$output$protected))
              translate(uiText, "protectAreas")$title), collapse = " & "),
          " from ", min(df$year, na.rm = TRUE), " to ", max(df$year, na.rm = TRUE),
          " in ",
          if (all(c("flanders", "wallonia", "brussels") %in% selectedRegions))
            translate(uiText, "Belgi\u00EB")$title else
            paste(translate(uiText, selectedRegions)$title, collapse = ", ")
          ))
        # move annotation to the left
        if (any(grepl("The status cannot", myPlot$x$data[[2]]$text))) {
          myPlot$x$data[[2]]$x <- tail(sort(myPlot$x$data[[1]]$x), n = 3)
          myPlot$x$data[[2]]$hovertext <- NULL
        } else {
          for (i in seq_along(plotly_build(myPlot)$x$data))
            if (!is.null(myPlot$x$data[[i]]$name))
              myPlot$x$data[[i]]$name <- newLabels[match(myPlot$x$data[[i]]$name, names(newLabels))]
        }
        
      }
      
      list(
        plot = myPlot, 
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
#' @param translationId character, identifier for the translation file provided 
#' in \code{uiText}; by default this is same as \code{triasFunction}
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
plotTriasServer <- function(id, uiText, data, triasFunction, 
  translationId = triasFunction, triasArgs = NULL,
  filters = NULL, maxDate = reactive(NULL), outputType = c("plot", "table"),
  dashReport = NULL) {
  
  # For R CMD check
  protected <- NULL
  
  outputType <- match.arg(outputType)
  
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      tmpTranslation <- reactive(translate(uiText(), translationId))
      
      output$titlePlotTrias <- renderUI(h3(HTML(tmpTranslation()$title)))
      
      description <- reactive({
          
          decodeText(text = tmpTranslation()$description,
            params = list(maxDate = format(maxDate(), "%d/%m/%Y")))
          
        })
      
      output$descriptionPlotTrias <- renderUI(HTML(description()))
      
      
      output$filters <- renderUI({
          
          if (!is.null(filters)) 
            lapply(names(filters), function(iFilter) {
                  if (filters[[iFilter]]$type == "checkbox") {
                    checkboxInput(inputId = ns(iFilter), 
                      label = translate(uiText(), iFilter)$title) 
                  } else if (filters[[iFilter]]$type == "select") {
                    choices <- filters[[iFilter]]$choices
                    names(choices) <- translate(uiText(), choices)$title
                    fluidRow(column(4, selectInput(inputId = ns(iFilter),
                      label = translate(uiText(), iFilter)$title,
                      choices = choices)))
                  }
                })
          
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
            
            req(plotData())
            
            if (!is.null(triasArgs)) {
              
              initArgs <- triasArgs()
              if (triasFunction == "apply_gam") {
                initArgs$eval_years <- min(plotData()$year, na.rm = TRUE):
                  max(plotData()$year, na.rm = TRUE)
                if (!is.null(input$correctBias) && input$correctBias) {
                  if (initArgs$y_var == "obs")
                    initArgs$baseline_var <- "cobs" else
                    initArgs$baseline_var <- "c_ncells"
                }
              }
              if (!is.null(input$regionLevel))
                initArgs$type <- input$regionLevel
              
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
          req(plotResult())
          input
          
          # Return the static values
          dashReport[[ns(triasFunction)]] <- c(
            list(
              plot = isolate(plotResult()$plot),
              title = isolate(tmpTranslation()$title),
              description = isolate(description())
            ),
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
      wellPanel(
        uiOutput(ns("filters"))
      ),
      
      if (outputType == "plot")
          plotModuleUI(id = ns("plotTrias")) else
          tableModuleUI(id = ns("plotTrias")),
      optionsModuleUI(id = ns("plotTrias"), doWellPanel = FALSE),
      tags$hr()
    
    )
  )
  
}