# Server and UI module for extra DB page
# 
# Author: mvarewyck
###############################################################################


#' Extra DB page - server module
#' 
#' @inheritParams plotModuleServer 
#' @return no return value
#' 
#' @author mvarewyck
#' @export
dbServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
      
      ns <- session$ns
      
      resetNavigation <- reactiveVal(FALSE)
      
      output$title <- renderUI({
          
          translate(id = "other_db")$title  
          
        })
      
      output$content <- renderUI({
          
          tileChoices <- c("early_warning", "mica_db", "radius_db")
          tileNames <- lapply(tileChoices, function(iChoice){
              foto <- list.files(path = system.file("app", "www", package = "alienSpecies"), pattern = iChoice)
              title <- translate(id = iChoice)$title
              hover <- translate(id = iChoice)$description
              HTML(paste0(
                  "<div class='radio-tiles-title' title='", hover, "'>", title, "</div>",
                  "<div class='radio-tiles-image'>", 
                  img(src = foto, width = "100%", `aspect-ratio` = "400/270", title = hover), "</div>"
                ))
            })
          
          if (resetNavigation())
            resetNavigation(FALSE)
          
          tags$div(style = "margin-top: -20px;",
            radioButtons(
              inputId = ns("navigate"), label = "", inline = TRUE,
              choiceValues = tileChoices, choiceNames = tileNames,
              selected = character(0)
            ),
            tags$script("$('.radio-inline').addClass('radio-tiles');")
          )
          
        })
      
      observeEvent(input$navigate, {
          
          switch(input$navigate, 
            "early_warning" = session$sendCustomMessage(type = "openURL", list(message = "
                  window.open('https://alert.riparias.be', '_blank').focus(); 
                  ")), 
            "mica_db" = session$sendCustomMessage(type = "openURL", list(message = "
                  window.open('https://mica.inbo.be/', '_blank').focus(); 
                  ")),
            "radius_db" = session$sendCustomMessage(type = "openURL", list(message = "
                  window.open('https://radius-project.shinyapps.io/dashboard/', '_blank').focus(); 
                  "))
          )
          
          resetNavigation(TRUE)
          
        })
      
      
    })
  
}

#' Extra DB page - module UI
#' 
#' @inheritParams plotModuleServer
#' @return UI object, \code{tabPanel} object that can be included in 
#' \code{navbarPage}
#' 
#' @author mvarewyck
#' @export
dbUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(title = uiOutput(ns("title")), value = "other_db",
    tags$div(class = "noButton", style = "margin-top:20px;", 
      uiOutput(ns("content")))
  )  
  
}

