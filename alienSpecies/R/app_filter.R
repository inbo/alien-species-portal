# Shiny functions for filtering on global indicators page
# 
# Author: mvarewyck
###############################################################################



#' Hierarchical selection input
#' 
#' Source: https://stackoverflow.com/a/60315446
#' https://stackoverflow.com/a/53546262
#' @param inputId character, unique identifier for the input object
#' @param choices list, choices to select from 
#' @param multiple boolean, whether user can select multiple options
#' @param cascaded boolean, whether to have cascaded view
#' @param selected character vector, selected values initially
#' @param placeholder character, text to be displayed if none are selected
#' @return div object
#' 
#' @author mvarewyck
#' @import htmltools
#' @importFrom shiny singleton
#' @importFrom jsonlite toJSON
#' @export
comboTreeInput <- function(inputId, choices, multiple = TRUE, cascaded = FALSE,
  selected = NULL, placeholder = ""){
  
  tags$div(style = "width: 100%; height: 50px;",
    shiny::singleton(shiny::tags$link(href = "comboTree.css", rel = "stylesheet")),
    shiny::singleton(shiny::tags$script(src = "comboTreeBinding.js")),
    shiny::singleton(shiny::tags$script(src = "comboTreePlugin.js")),
    tags$input(id = inputId, class = "comboTree", type = "text", 
      placeholder = placeholder,
      `data-choices` = as.character(toJSON(choices, auto_unbox = TRUE)),
      `data-multiple` = ifelse(multiple, "true", "false"), 
      `data-cascaded` = ifelse(cascaded, "true", "false"),
      `data-selected` = if (!is.null(selected)) as.character(toJSON(selected))
    )
  )
}


#' Module for filter selection - server side
#' @inheritParams welcomeSectionUI
#' @param url reactive character, url search query
#' @param placeholder character, placeholder if none is selected
#' @param initChoices character vector, choices for the \code{selectInput}
#' @return reactive, selected filter value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
filterSelectServer <- function(id, url, placeholder, initChoices) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns

      output$filter <- renderUI({
          
          names(initChoices) <- initChoices
          initChoices <- c("", initChoices)
          names(initChoices)[1] <- placeholder
          
          selectInput(inputId = ns("filter"), label = NULL, 
            choices = initChoices, 
            selected = url()[[id]], multiple = TRUE)
        })
      
      outputOptions(output, "filter", suspendWhenHidden = FALSE)
      
      reactive(input$filter)
    
    })
}


#' Module for filter selection - UI side
#' @inheritParams welcomeSectionUI
#' @return html object
#' 
#' @author mvarewyck
#' @import shiny
#' @export
filterSelectUI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("filter"))
  
}