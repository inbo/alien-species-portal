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
    translations = loadMetaData(language = "nl"),
    species_choice = if (doDebug) c(2498252, 2769766)[2] else NULL
  )
  
  
  # Select language
  observeEvent(input$translate_nl, results$translations <- loadMetaData(language = "nl"))
  observeEvent(input$translate_fr, results$translations <- loadMetaData(language = "fr"))
  observeEvent(input$translate_en, results$translations <- loadMetaData(language = "en"))
  
  
  
  # Tabpages
  # ----------
  
  # Load code for all tabpages
  for (serverFile in list.files("serverFiles", full.names = TRUE))
    source(serverFile, local = TRUE)
  
  
  
  # Tabpanels
  output$start_page <- renderUI({
      
      source(file.path("uiFiles", "uiStart.R"), local = TRUE)$value
      
    })
  
  output$indicators_content <- renderUI({
      
      source(file.path("uiFiles", "uiChecklist.R"), local = TRUE)$value
      
    })
  
  output$species_content <- renderUI({
      
      source(file.path("uiFiles", "uiSpecies.R"), local = TRUE)$value
      
    })
  
}