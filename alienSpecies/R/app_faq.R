# Server and UI module for FAQ page
# 
# Author: mvarewyck
###############################################################################


#' FAQ page - server module
#' 
#' @inheritParams plotModuleServer 
#' @param language reactive value, language
#' @return no return value
#' 
#' @author mvarewyck
#' @export
faqServer <- function(id, language) {
  
  moduleServer(id, function(input, output, session) {
      
      ns <- session$ns
      
      output$title <- renderUI({
          
          translate(id = "faq")$title  
          
        })
      
      htmlSectionServer(id = "content", species = reactive("FAQ"),
        language = language)
      
    })
  
}

#' FAQ page - module UI
#' 
#' @inheritParams plotModuleServer
#' @return UI object, \code{tabPanel} object that can be included in 
#' \code{navbarPage}
#' 
#' @author mvarewyck
#' @export
faqUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(title = uiOutput(ns("title")), value = "faq",
    tags$div(class = "noButton", style = "margin-top:20px;", 
      htmlSectionUI(id = ns("content")))
  )  
  
}

