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
    translations = loadMetaData(language = "en", local = doDebug),
    searchId = "",
    renderedTabs = c("start", "checklist_taxa"),
    exoten_timeNA = defaultTimeNA,
    exoten_time = defaultTime,
    species_choice = ""
  )
  
  
  # Select language
  observeEvent(input$translate_nl, results$translations <- loadMetaData(language = "nl", local = doDebug))
  observeEvent(input$translate_fr, results$translations <- loadMetaData(language = "fr", local = doDebug))
  observeEvent(input$translate_en, results$translations <- loadMetaData(language = "en", local = doDebug))
  
  results$switchTranslation <- reactive(
    input$translate_nl + input$translate_fr + input$translate_en
  )
  
  
  # URL Query
  # ----------
  
    
  # Create URL for current session
  observeEvent(input$showShare, {
      
      searchId <- if (input$tabs != "start")
          results$searchId else 
          ""
      languageId <- paste0("&language=", attr(results$translations, "language"))
      
      showModal(
        modalDialog(title = "Application link",
          tags$textarea(class = "form-control", rows = "1", style = "resize:none;",
            readonly = "readonly",
            paste0("http://alienspecies.inbo.be/?page=", input$tabs, languageId, searchId)
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
    
    # Resulting URL from previous session
    urlSearch <- reactive(parseQueryString(session$clientData$url_search))
    
    
    # Update page
    observe({
        
        results$urlPage <- urlSearch()$page
        
      })
    
    observeEvent(results$urlPage, {
        
        updateNavbarPage(session = session, inputId = "tabs", selected = results$urlPage)
        
        # TODO necessary here?
        if (!is.null(urlSearch()$habitat))
          results$urlHabitat <- strsplit(urlSearch()$habitat, split = ", ")[[1]]
        
      })

    
    
    # Update language
    observe({
        
        results$urlLanguage <- urlSearch()$language
        
      })
    
    observeEvent(results$urlLanguage, {
        
        results$translations <- loadMetaData(language = results$urlLanguage)
        
      })
    
  
  # Tabpages
  # ----------
  
  output$shareLink <- renderUI(
    actionLink(inputId = "showShare", label = translate(results$translations, "shareLink"))
  )
  
  # Landing page
  source(file.path("serverFiles", "serverStart.R"), local = TRUE)
  output$start_page <- renderUI({
      
      source(file.path("uiFiles", "uiStart.R"), local = TRUE)$value
      
    })
  
  # Render tabpanel upon need
  observeEvent(input$tabs, {
      
      # render only once
      req(!input$tabs %in% results$renderedTabs)
      
      switch(input$tabs,
        global_indicators = {
          
          output$indicators_content <- renderUI({
              source(file.path("uiFiles", "uiChecklist.R"), local = TRUE)$value
            })
          source(file.path("serverFiles", "serverChecklist.R"), local = TRUE)
          results$renderedTabs <- c(results$renderedTabs, "global_indicators")
          
        },
        species_information = {
          
          output$species_content <- renderUI({
              source(file.path("uiFiles", "uiSpecies.R"), local = TRUE)$value
            })
          source(file.path("serverFiles", "serverSpecies.R"), local = TRUE)
          results$renderedTabs <- c(results$renderedTabs, "species_information")
          
        } 
      )
      
    })
  
}