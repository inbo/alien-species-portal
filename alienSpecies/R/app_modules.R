# Shiny modules
# 
# Author: mvarewyck
###############################################################################






#' User input for controlling specific plot (ui-side)
#' @param id character, module id, unique name per plot
#' @param showGroup boolean, whether to show a select input field for group variable
#' @param showSummary boolean, whether to show a select input field for summary choice
#' @param showPeriod boolean, whether to show a slider input field for period (first_observed)
#' @param showGewest boolean, whether to show filter for gewest
#' @param exportData boolean, whether a download button for the data is shown
#' @param doWellPanel boolean, whether to display the options within a 
#' \code{shiny::wellPanel()}
#' @return ui object (tagList)
#' @import shiny
#' @export
optionsModuleUI <- function(id, showGroup = FALSE, showSummary = FALSE, 
  showPeriod = FALSE, showGewest = FALSE,
  exportData = TRUE, doWellPanel = TRUE) {
  
  ns <- NS(id)
  
  
  toReturn <- tagList(
    fixedRow(
      if (showGewest)
        column(6, uiOutput(ns("gewest"))),
      if (showGroup)
        column(6, uiOutput(ns("group"))),
      if (showSummary)
        column(6, uiOutput(ns("summarizeBy"))),
      if (showPeriod)
        column(12, uiOutput(ns("period")))
    ),
    if (exportData)
      downloadButton(ns("dataDownload"), "Download data")
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
  
  withSpinner(plotlyOutput(ns("plot"), height = height))
  
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
#' @param combine reactive boolean, see \code{\link{trendYearRegion}}
#' @return no return value; plot output object is created
#' @author mvarewyck
#' @import shiny
#' @importFrom utils write.table tail
#' @importFrom DT datatable formatRound renderDT
#' @importFrom plotly ggplotly layout
#' @export
plotModuleServer <- function(id, plotFunction, data, uiText = NULL,
  outputType = NULL, triasFunction = NULL, triasArgs = NULL, 
  period = NULL, combine = NULL) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$gewest <- renderUI({
          
          choices <- c("flanders", "brussels", "wallonia")
          names(choices) <- translate(uiText(), choices)$title
          
          selectInput(inputId = ns("gewest"), label = translate(uiText(), "gewest")$title,
            choices = choices, selected = choices, multiple = TRUE)
          
        })
      
      output$group <- renderUI({
          
          choices <- c("", "lifeStage")
          names(choices) <- c("", translate(uiText(), choices[-1])$title)
          
          selectInput(inputId = ns("group"), label = translate(uiText(), "group")$title, 
            choices = choices)
          
        })
      
      output$summarizeBy <- renderUI({
          
          choices <- c("sum", "cumsum")
          names(choices) <- translate(uiText(), choices)$title
          
          selectInput(inputId = ns("summarizeBy"), 
            label = translate(uiText(), "summary")$title, choices = choices)
          
        })
      
      output$period <- renderUI({
          
          validate(need("first_observed" %in% colnames(data()), "No year available"))
          
          timeRange <- range(data()$first_observed, na.rm = TRUE) 
          
          div(class = "sliderBlank", 
            sliderInput(inputId = ns("period"), 
              label = translate(uiText(), "period")$title,
              min = timeRange[1], max = timeRange[2], value = timeRange,
              step = 1, sep = "", width = "100%")
          )
        
        })
      
      
      # Filter plot data
      subData <- reactive({
         
          subData <- if (is.null(input$period))
            data() else
            data()[data()$first_observed %in% input$period[1]:input$period[2], ]
        
        if (is.null(input$gewest))
          subData else
          subData[subData$GEWEST %in% input$gewest, ]
          
        })
      
      
      argList <- reactive({
          
          req(nrow(subData()) > 0)
          
          argList <- c(
            list(
            # General
              df = subData()),
            if (!is.null(outputType))
              list(outputType = outputType),
            if (!is.null(uiText))
              list(uiText = uiText()),
            # Trias
            if (!is.null(triasFunction))
              list(triasFunction = triasFunction),
            if (!is.null(triasArgs))
              list(triasArgs = triasArgs()),
            # Reactives
            if (!is.null(period))
              list(period = period()),
            if (!is.null(combine))
              list(combine = combine()),
            # Input
            if (!is.null(input$group))
              list(groupVar = input$group),
            if (!is.null(input$summarizeBy))
              list(summarizeBy = input$summarizeBy)
          )
          
          argList
          
        })
      
      resultFct <- reactive({
          
          toReturn <- tryCatch(
            do.call(plotFunction, args = argList()),
            error = function(err)
              validate(need(FALSE, err$message))
          )		
          
          validate(need(!is.null(toReturn), "Niet beschikbaar"))
          
          return(toReturn)          
          
        })
      
      
      output$plot <- renderPlotly({  
          
          if (!is.null(triasFunction) && triasFunction == "apply_gam") {
            # remove title
            myPlot <- resultFct()$plot %>% layout(title = "")
            # move annotation to the left
            myPlot$x$data[[2]]$x <- tail(sort(myPlot$x$data[[1]]$x), n = 3)
            myPlot$x$data[[2]]$hovertext <- NULL
            myPlot
          } else resultFct()$plot
        
        })

      if (!(plotFunction == "countOccupancy" |
          (!is.null(triasFunction) && triasFunction %in% c("barplotLenteNesten", "countNesten"))))
        outputOptions(output, "plot", suspendWhenHidden = FALSE)
      
      
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
      
      
      output$table <- DT::renderDT({
          
          DT::datatable(resultFct()$data, rownames = FALSE,
            colnames = resultFct()$columnNames,
            selection = "single",
            options = list(dom = 'ftp', 
              pageLength = if (triasFunction == "tableNesten") -1 else 5))
          
        })
      
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
titleModuleServer <- function(id, plotFunction, uiText) {
  
  moduleServer(id,
    function(input, output, session) {
      
      output$title <- renderUI({
          
          translate(uiText(), plotFunction)$title 
          
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

