function(input, output, session) {
  
  # For debugging
  # -------------
  
  observe({
      req(!is.null(input$debug_console))
      if (input$debug_console > 0)
        browser()
    })
  
  
  output$print <- renderPrint({
      
#                        names(session$clientData)
      
    })
  
  
  output$debug <- renderUI({
      
      if (doDebug)
        tags$div(style = "margin-top:50px",
          h5(actionLink(inputId = "debug_console", label = "Connect with console"),
            align = "left"),
          verbatimTextOutput("print")
        )
    })
  
  
  results <- reactiveValues(
    # Default language is dutch
    translations = loadTranslations(language = "nl")
  )
  
  
  # Select language
  observeEvent(input$translate_nl, results$translations <- loadTranslations(language = "nl"))
  observeEvent(input$translate_fr, results$translations <- loadTranslations(language = "fr"))
  observeEvent(input$translate_en, results$translations <- loadTranslations(language = "en"))
  
  
  
  # Tabpages
  # ----------
  
  # Load code for all tabpages
  for (serverFile in list.files("serverFiles", full.names = TRUE))
    source(serverFile, local = TRUE)
  
  
  
  # Tabpanel checklist
  output$checklist_content <- renderUI({
      
      source(file.path("uiFiles", "uiChecklist.R"), local = TRUE)$value
      
    })
  
}