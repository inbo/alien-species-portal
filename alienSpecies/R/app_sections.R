# Text sections in the shiny app
# 
# Author: mvarewyck
###############################################################################




#' Shiny module for creating welcome section - server side
#' @param id character, unique identifier
#' @param uiText reactive object, data.frame contains title and description
#' to display; as obtained by \code{\link{loadMetaData}}
#' @return no return value
#' 
#' @author mvarewyck
#' @export
welcomeSectionServer <- function(id, uiText) {
  
  moduleServer(id, function(input, output, session) {
      
      tmpTranslation <- reactive(translate(uiText(), "welcomeTitle"))
      
      output$welcomeTitle <- renderUI(h1(HTML(tmpTranslation()$title)))
      
      output$welcomeMain <- renderUI(HTML(tmpTranslation()$description))
      
    })
}
#' Section for welcoming (top of the page)
#' @param id character, from which page this function is called
#' e.g. 'wbe'
#' @template moduleUI 
#' 
#' @author mvarewyck
#' @export
welcomeSectionUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    tags$div(align = "center",
      uiOutput(ns("welcomeTitle"))
    ),
    tags$p(class = "lead", uiOutput(ns("welcomeMain")))
  )
  
  
}



#' Replace {{fields}} in title/description translations
#' @param text character, input from translation
#' @param params named list, all parameters that should be replaced with 
#' their value
#' @return character, modified for the \code{params} mentioned in the text  
#' 
#' @author mvarewyck
#' @export
decodeText <- function(text, params) {
  
  newText <- text
  
  for (iParam in names(params)) {
    
    newText <- if (iParam == "period")
      paste(newText, yearToTitleString(params[[iParam]])) else
      gsub(paste0("\\{\\{", iParam, "\\}\\}"), params[[iParam]], newText)
    
  }
  
  newText
  
}

