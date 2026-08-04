# Shiny modules
# 
# Author: mvarewyck
###############################################################################






#' User input for controlling specific plot (ui-side)
#' @param id character, module id, unique name per plot
#' @param showSummary boolean, whether to show a select input field for summary choice
#' @param showPeriod boolean, whether to show a slider input field for period (first_observed)
#' @param exportData boolean, whether a download button for the data is shown
#' @param exportGraph boolean, whether a download button for the graph is shown
#' @param doWellPanel boolean, whether to display the options within a 
#' \code{shiny::wellPanel()}
#' @return ui object (tagList)
#' @import shiny
#' @export
optionsModuleUI <- function(id, showSummary = FALSE, 
  showPeriod = FALSE, exportData = TRUE, exportGraph = TRUE, doWellPanel = TRUE) {
  
  ns <- NS(id)
  
  
  toReturn <- tagList(
    fixedRow(
      column(6, uiOutput(ns("group"))),
      if (showSummary)
        column(6, uiOutput(ns("summarizeBy"))),
      if (showPeriod)
        column(12, uiOutput(ns("period")))
    ),
    if (exportGraph)
      actionButton(ns("graphDownload"), translate("downloadGraph")$title, icon = icon("download"),
        class = "btn-default shiny-download-link downloadButton", type = "button"),
    if (exportData)
      downloadButton(ns("dataDownload"), translate("downloadData")$title)
  )
  
  if (doWellPanel)
    wellPanel(toReturn) else
    toReturn
  
}



#' Interactive plot (ui-side)
#' 
#' @inheritParams optionsModuleUI
#' @param height character, plot height, default is "600px" 
#' 
#' @return ui object
#' @author mvarewyck
#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput
#' @export
plotModuleUI <- function(id, height = "600px") {
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("plotMessage")),
    if (id == "management2_lente-plotTrias")
        # dirty fix: this plot stays hidden when behind spinner
        plotlyOutput(ns("plot"), height = height) else
        withSpinner(plotlyOutput(ns("plot"), height = height))
  )
}


#' Interactive table (ui-side)
#' 
#' @inheritParams optionsModuleUI
#' @param includeTotal boolean, whether include text with total number of records in table
#' 
#' @return ui object
#' @author mvarewyck
#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT DTOutput
#' @export
tableModuleUI <- function(id, includeTotal = FALSE) {
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("plotMessage")),
    withSpinner(DT::DTOutput(ns("table"))),
    if (includeTotal)
      uiOutput(ns("total"))
  )
  
}


#' Interactive plot or table (server-side)
#' 
#' @inheritParams welcomeSectionServer
#' @inheritParams optionsModuleUI
#' @inheritParams plotTriasServer
#' @param plotFunction character, defines the plot function to be called
#' @param data reactive data.frame, data for chosen species
#' @param period reactive numeric vector of length 2, selected period
#' @param regions reactive character vector, selected regions to be passed to the
#' plot function
#' @param combine reactive boolean, see \code{\link{trendYearRegion}}
#' @param groupChoices reactive character, defines the choices for group variable;
#' if NULL no groupChoices available
#' @param addYLabel reactive boolean, see \code{\link{countOccurrence}}
#' @return no return value; plot output object is created
#' @author mvarewyck
#' @import shiny
#' @importFrom utils write.table tail
#' @importFrom DT datatable formatRound renderDT
#' @export
plotModuleServer <- function(id, plotFunction, data,
  outputType = NULL, triasFunction = NULL, triasArgs = NULL, groupChoices = NULL,
  period = NULL, regions = NULL, combine = NULL, addYLabel = NULL, spatialLevel = NULL) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$group <- renderUI({
          
          if (!is.null(groupChoices))
            selectInput(inputId = ns("group"), label = translate("group")$title, 
              choices = groupChoices())
          
        })
      
      output$summarizeBy <- renderUI({
          
          choices <- c("sum", "cumsum")
          names(choices) <- translate(choices)$title
          
          selectInput(inputId = ns("summarizeBy"), 
            label = translate("summarizeBy")$title, choices = choices)
          
        })
      
      output$period <- renderUI({
          
          validate(need("first_observed" %in% colnames(data()), "No year available"))
          
          timeRange <- range(data()$first_observed, na.rm = TRUE) 
          
          sliderInput(inputId = ns("period"), 
              label = translate("period")$title,
              min = timeRange[1], max = timeRange[2], value = timeRange,
              step = 1, sep = "", width = "100%")
        
        })
      
      
      # Filter plot data
      subData <- reactive({
         
          if (is.null(input$period))
            data() else
            data()[data()$first_observed %in% input$period[1]:input$period[2], ]
          
        })
      
      
      argList <- reactive({
          
          if (nrow(subData()) == 0) {
            output$plotMessage <- renderUI(tagList(tags$br(), tags$h4(translate("noData")$title)))

            argList <- NULL
          } else {
            output$plotMessage <- renderUI(NULL)

            argList <- c(
              list(
                # General
                df = subData()),
              if (!is.null(outputType))
                list(outputType = outputType),
              # Trias
              if (!is.null(triasFunction))
                list(triasFunction = triasFunction),
              if (!is.null(triasArgs))
                list(triasArgs = triasArgs()),
              # Reactives
              if (!is.null(period))
                list(period = period()),
              if (!is.null(regions))
                list(regions = regions()),
              if (!is.null(combine))
                list(combine = combine()),
              if (!is.null(spatialLevel))
                list(spatialLevel = spatialLevel()),
              # Input
              if (!is.null(input$group))
                list(groupVar = input$group),
              if (!is.null(input$summarizeBy))
                list(summarizeBy = input$summarizeBy),
              if (!is.null(addYLabel))
                list(addYLabel = addYLabel)
            )
          }
          
          argList
          
        })
      
      resultFct <- reactive({
          
          req(argList())
          
          toReturn <- tryCatch({
              do.call(plotFunction, args = argList())
            },
            error = function(err)
              validate(need(FALSE, err$message))
          )		
          
          validate(need(!is.null(toReturn), "Niet beschikbaar"))
          
          return(toReturn)          
          
        })
      
      finalPlot <- reactive({
          
          req(resultFct())
          
          resultFct()$plot
        
        })
      
      
      output$plot <- renderPlotly({
          tryCatch({
              finalPlot()
            },
            error = function(err)
              NULL
          )	
        })
      
      
#      if (plotFunction != "countOccupancy" & plotFunction != "countOccurrence")
#        outputOptions(output, "plot", suspendWhenHidden = FALSE)
      
      
      output$dataDownload <- downloadHandler(
        filename = function() nameFile(content = paste0(
              if (!is.null(triasFunction)) 
                  triasFunction else 
                  plotFunction, 
              "_data"), fileExt = "csv"),
        content = function(file) {
          
          resFct <- resultFct()
          
          ## checks
          
          # Note: a data.frame is a list!
          isDataPresent <- ifelse(!is.null(resFct),
            ifelse(is.data.frame(resFct), !is.null(resFct), !is.null(resFct$data)),
            FALSE
          )
          
          validate(need(isDataPresent, "Niet beschikbaar"))
          
          ## extract data to export
          dataPlot <- if (is.data.frame(resFct)) resFct	else resFct$data
          
          ## write data to exported file
          write.table(x = dataPlot, file = file, quote = FALSE, row.names = FALSE,
            sep = ";", dec = ",")
          
        }
      )
      
      observeEvent(input$graphDownload, {
          shinyscreenshot::screenshot(id="plot", 
            filename=paste0(if (!is.null(triasFunction)) triasFunction else plotFunction, "_graph")
          )
          
        })
      
      
      output$table <- DT::renderDT({
          
          tryCatch({
              if (identical(plotFunction, "countOccurrence"))
                DT::datatable(resultFct()$data, rownames = FALSE,
                  colnames = resultFct()$columnNames,
                  selection = "single",
                  options = list(dom = 'ftp',
                    pageLength = 10,
                    order = list(list(0, "desc")))
                )
              else
                DT::datatable(resultFct()$data, rownames = FALSE,
                  colnames = resultFct()$columnNames,
                  selection = "single",
                  options = list(dom = 'ftp',
                    pageLength = if (identical(triasFunction, "tableNesten")) -1 else 5))

            },
            error = function(err)
              NULL
          )
          
        })
      
      
      reactive(c(
        list(plot = if (!is.null(outputType) && outputType == "table")
                req(resultFct()) else 
                req(finalPlot())
            ),
        reactiveValuesToList(input)
      ))
      
    })
  
}



#' Translated title - server side
#' @inheritParams plotModuleServer
#' @inheritParams welcomeSectionServer
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
titleModuleServer <- function(id, plotFunction) {
  
  moduleServer(id,
    function(input, output, session) {
      
      output$title <- renderUI({
          
          translate(plotFunction)$title 
          
        })
    })
}


#' Translated title - ui side
#' @inheritParams welcomeSectionUI
#' @return ui object
#' 
#' @author mvarewyck
#' @import shiny
#' @export
titleModuleUI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("title"))
  
}

