# Server and UI module for FAQ page
# 
# Author: mvarewyck
###############################################################################


#' Simple HTML page - server module
#' 
#' @inheritParams plotModuleServer 
#' @param language reactive value, language
#' @return no return value
#' 
#' @author mvarewyck
#' @export
simpleHTMLPageServer <- function(id, language) {
  
  moduleServer(id, function(input, output, session) {
      
      ns <- session$ns
      
      output$title <- renderUI({
          
          translate(id = id)$title  
          
        })
      
      htmlSectionServer(id = "content", species = reactive(toupper(id)),
        language = language)
      
    })
  
}

#' Simple HTML page - module UI
#' 
#' @inheritParams plotModuleServer
#' @return UI object, \code{tabPanel} object that can be included in 
#' \code{navbarPage}
#' 
#' @author mvarewyck
#' @export
simpleHTMLPageUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(title = uiOutput(ns("title")), value = id,
    tags$div(class = "noButton", style = "margin-top:20px;", 
      htmlSectionUI(id = ns("content")))
  )  
  
}

