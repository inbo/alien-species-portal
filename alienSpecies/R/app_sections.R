# Text sections in the shiny app
# 
# Author: mvarewyck
###############################################################################




#' Shiny module for creating welcome section - server side
#' @param id character, unique identifier
#' @param uiText reactive value, data.frame contains title and explanation text to display
#' @return no return value
#' 
#' @author mvarewyck
#' @export
welcomeSectionServer <- function(id, uiText) {
  
  moduleServer(id, function(input, output, session) {
      
      subText <- reactive({uiText[uiText$plotFunction == "welcomeSection", ]})
      
      output$welcomeTitle <- renderUI({
          h1(HTML(subText()$title))
          
        })
      
      output$welcomeMain <- renderUI({
          
          HTML(subText()[, id])
          
        })
      
    })
}
#' Section for welcoming (top of the page)
#' @param id character, from which page this function is called
#' e.g. 'wbe'
#' @template moduleUI 
#' 
#' @author mvarewyck
#' @export
welcomeSectionUI <- function(id, uiText) {
  
  ns <- NS(id)
  
  tagList(
    tags$div(align = "center",
      uiOutput(ns("welcomeTitle"))
    ),
    tags$p(class = "lead", uiOutput(ns("welcomeMain")))
  )
  
  
}

