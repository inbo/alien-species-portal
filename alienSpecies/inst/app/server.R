function(input, output, session) {
  
  # For debugging
  # -------------
  
  observe({
      
      if (doDebug)
        shinyjs::showLog()
      
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
    translations = loadMetaData(language = "en"),
    searchId = "",
    exoten_timeNA = defaultTimeNA,
    exoten_time = defaultTime,
    species_choice = if (doDebug) c("Alopochen aegyptiaca", "Muntiacus reevesi")[2] else NULL
  )
  
  
  # Select language
  observeEvent(input$translate_nl, results$translations <- loadMetaData(language = "nl"))
  observeEvent(input$translate_fr, results$translations <- loadMetaData(language = "fr"))
  observeEvent(input$translate_en, results$translations <- loadMetaData(language = "en"))
  
  # URL Query
  # ----------
  
  observeEvent(input$showShare, {
      
      searchId <- if (input$tabs %in% c("global_indicators"))
        results$searchId else 
        ""
      
      showModal(
        modalDialog(title = "Application link",
          tags$textarea(class = "form-control", rows = "1", style = "resize:none;",
            readonly = "readonly",
            paste0("http://alienspecies.inbo.be/?page=", input$tabs, searchId)
          ),
          tagList(
            br(),
            span(class = "text-muted", "This is a direct link to the current page and search criteria."),
            span(id = "shiny-bookmark-copy-text", class = "text-muted")
          ),
          tags$script("$('#shiny-modal').
              one('show.bs.modal', function() {
              setTimeout(function() {
              var $textarea = $('#shiny-modal textarea');
              $textarea.innerHeight($textarea[0].scrollHeight);
              }, 200);
              });
              $('#shiny-modal')
              .one('shown.bs.modal', function() {
              $('#shiny-modal textarea').select().focus();
              });
              $('#shiny-bookmark-copy-text')
              .text(function() {
              if (/Mac/i.test(navigator.userAgent)) {
              return 'Press \u2318+C to copy.';
              } else {
              return 'Press Ctrl+C to copy.';
              }
              });
              "),
          footer = modalButton("Close"),
          easyClose = TRUE
        )
      )
      
    })
    
    observe({
        
        # The url will be sth like: http://awsabiirl1118.jnj.com/?step=qc&id=16608
        url <- parseQueryString(session$clientData$url_search)
        results$urlPage <- url$page
        
      })
    
    observeEvent(results$urlPage, {
        
        updateNavbarPage(session = session, inputId = "tabs", selected = results$urlPage)
        
        url <- parseQueryString(session$clientData$url_search)
        if (!is.null(url$habitat))
          results$urlHabitat <- strsplit(url$habitat, split = ", ")[[1]]
        
      })
    
  
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