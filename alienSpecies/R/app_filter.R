# Shiny functions for filtering on global indicators page
# 
# Author: mvarewyck
###############################################################################



#' Hierarchical selection input
#' 
#' Sources: 
#' https://stackoverflow.com/a/60315446
#' https://stackoverflow.com/a/53546262
#' https://shiny.rstudio.com/articles/js-custom-input.html
#' @param inputId character, unique identifier for the input object
#' @param choices list, choices to select from 
#' @param multiple boolean, whether user can select multiple options
#' @param cascaded boolean, whether to have cascaded view
#' @param selected character vector, selected values initially
#' @param placeholder character, text to be displayed if none are selected
#' @param isNumeric boolean, whether the selected ids are numeric
#' @return div object
#' 
#' @author mvarewyck
#' @import htmltools
#' @importFrom shiny singleton
#' @importFrom jsonlite toJSON
#' @export
comboTreeInput <- function(inputId, choices, multiple = TRUE, cascaded = FALSE,
  selected = NULL, placeholder = "", isNumeric = FALSE){
  
  tags$div(style = "width: 100%; height: 50px;",
    tags$input(id = inputId, class = "comboTree", type = "text", 
      placeholder = placeholder,
      `data-choices` = as.character(toJSON(choices, auto_unbox = TRUE)),
      `data-multiple` = ifelse(multiple, "true", "false"), 
      `data-cascaded` = ifelse(cascaded, "true", "false"),
      `data-selected` = if (!is.null(selected)) if (isNumeric)
          paste0("[", selected, "]") else
          as.character(toJSON(strsplit(selected, split = ",")[[1]]))
    
    )
  )
}


#' Module for filter selection - server side
#' @inheritParams welcomeSectionUI
#' @param url reactive character, url search query
#' @param placeholder character, placeholder if none is selected
#' @param initChoices character vector, choices for the \code{selectInput}
#' @param translations reactive data.frame with translations
#' @return reactive, selected filter value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
filterSelectServer <- function(id, url, initChoices, translations) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns

      output$filter <- renderUI({
          
          names(initChoices) <- translate(translations(), initChoices)$title
          initChoices[1] <- ""
          
          selectInput(inputId = ns("filter"), label = NULL, 
            choices = initChoices, 
            selected = if (!is.null(url()[[id]])) strsplit(url()[[id]], split = ",")[[1]],
            multiple = TRUE)
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