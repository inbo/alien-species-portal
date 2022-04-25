# Shiny modules
# 
# Author: mvarewyck
###############################################################################






#' User input for controlling specific plot (ui-side)
#' @param id character, module id, unique name per plot
#' @param showType, boolean, whether to select a select input field with type
#' @param exportData boolean, whether a download button for the data is shown
#' @param doWellPanel boolean, whether to display the options within a 
#' \code{shiny::wellPanel()}
#' @return ui object (tagList)
#' @import shiny
#' @export
optionsModuleUI <- function(id, showType = FALSE, exportData = TRUE, doWellPanel = TRUE) {
  
  ns <- NS(id)
  
  
  toReturn <- tagList(
    
    if(showType)
      uiOutput(ns("type")),
    if(exportData)
      downloadButton(ns("dataDownload"), "Download data")
  
  )
  
  if (doWellPanel)
    wellPanel(toReturn) else
    toReturn
  
}




#' User input for controlling specific plot (server-side)
#'
#' @inheritParams optionsModuleUI
#' @param data reactive data.frame, data for chosen species
#' @param types, defines the species types that can be selected
#' @param labelTypes character, the displayed label for selecting options field
#' @param typesDefault, defines the default values for \code{types},
#' same as \code{types} by defualt
#' @param multipleTypes boolean, whether multiple types can be selected or not
#' 
#' @return no return value; some output objects are created
#' @import shiny
#' @export
optionsModuleServer <- function(id, data, 
  types = NULL, labelTypes = "Type", typesDefault = types, multipleTypes = FALSE) {
  
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      
      output$type <- renderUI({
          
          selectInput(inputId = ns("type"), label = labelTypes,
            choices = types(), 
            selected = typesDefault(), multiple = multipleTypes)
          
        })
      
    })
  
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
#' @inheritParams optionsModuleUI
#' @param plotFunction character, defines the plot function to be called
#' @param data reactive data.frame, data for chosen species
#' 
#' @return no return value; plot output object is created
#' @author mvarewyck
#' @import shiny
#' @importFrom utils write.table
#' @importFrom DT datatable formatRound renderDT
#' @export
plotModuleServer <- function(id, plotFunction, data, 
  cumulative = NULL) {
  
  moduleServer(id,
    function(input, output, session) {
      
      
      # Filter plot data
      subData <- reactive({
          
          data()
          
        })
      
      
      argList <- reactive({
          
          req(nrow(subData()) > 0)
          
          argList <- c(
            list(data = subData()),
            if (!is.null(input$time))
              list(jaartallen = input$time[1]:input$time[2]),
            if (!is.null(input$type))
              list(type = input$type),
            if (!is.null(cumulative))
              list(cumulative = cumulative)
          )
          
          
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
          
          resultFct()$plot
          
        })
      
      
      output$dataDownload <- downloadHandler(
        filename = function() nameFile(content = paste0(plotFunction, "_data"), fileExt = "csv"),
        content = function(file) {
          
          resFct <- resultFct()
          
          ## checks
          
          # Note: a data.frame is a list!
          isDataPresent <- ifelse(!is.null(resFct),
            ifelse(is.data.frame(resFct), !is.null(resFct), !is.null(resFct$plot)),
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
          
          DT::datatable(resultFct()$data, rownames = FALSE, container = resultFct()$header,
              selection = "single",
              options = list(dom = 't', pageLength = -1)) %>%
            formatRound(colnames(resultFct()$data), digits = 0, mark = "")
          
        })
      
    })
  
}
