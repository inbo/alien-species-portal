function(input, output, session) {
  
  # For debugging
  # -------------
  
  observe({
      update_lang(attr(results$translations, "language"))
      results$language <- attr(results$translations, "language")
    })
  
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
    language = "en",
    translations = loadMetaData(language = "en", local = doDebug),
    searchId = list(),
    renderedTabs = c("start", "checklist_taxa"),
    exoten_timeNA = defaultTimeNA,
    exoten_time = defaultTime,
    species_choice = "",
    filter_pwLevel2 = NULL,
    filter_nativeRange = NULL
  )
  
  
  # Select language
  observeEvent(input$translate_nl, {
      
      showModal(
        modalDialog(
          title = translate("confirmLanguageChange")$title,
          footer = tagList(
            actionButton(inputId = "confirm_nl", label = NULL, icon = icon("check", style = "color: #fff"), style = "background-color: #356196;"),
            modalButton(label = NULL, icon = icon("xmark"))
          ),
          easyClose = FALSE,
          
          translate("confirmLanguageChange")$description      
        )
      )
    })
  
  observeEvent(input$confirm_nl, {
      update_lang("nl")
      results$language <- "nl"
      
      removeModal()
    })
  
  
  observeEvent(input$translate_fr, {
      
      showModal(
        modalDialog(
          title = translate("confirmLanguageChange")$title,
          footer = tagList(
            actionButton(inputId = "confirm_fr", label = NULL, icon = icon("check", style = "color: #fff"), style = "background-color: #356196;"),
            modalButton(label = NULL, icon = icon("xmark"))
          ),
          easyClose = FALSE,
          
          translate("confirmLanguageChange")$description      
        )
      )
    })
  
  observeEvent(input$confirm_fr, {
      update_lang("fr")
      results$language <- "fr"
      
      removeModal()
    })
  
  observeEvent(input$translate_en, {
      
      showModal(
        modalDialog(
          title = translate("confirmLanguageChange")$title,
          footer = tagList(
            actionButton(inputId = "confirm_en", label = NULL, icon = icon("check", style = "color: #fff"), style = "background-color: #356196;"),
            modalButton(label = NULL, icon = icon("xmark"))
          ),
          easyClose = FALSE,
          
          translate("confirmLanguageChange")$description      
        )
      )
    })
  
  observeEvent(input$confirm_en, {
      update_lang("en")
      results$language <- "en"
      
      removeModal()
    })
  
  results$switchTranslation <- reactive(
    input$confirm_nl + input$confirm_fr + input$confirm_en
  )
  
  
  # Version
  # -------
  
  versionServer(id = "main")
  
  
  # URL Query
  # ----------
  
  shareLink <- reactive({
      
      searchId <- if (input$tabs %in% c("checklist_indicators", "species_information"))
          results$searchId else 
          list()
      searchId$language <- results$language
      searchId$page <- input$tabs
      
      createQueryString(
        baseUrl = config::get("url", file = system.file("config.yml", package = "alienSpecies")),
        query = searchId)
      
    })
    
  # Create URL for current session
  observeEvent(input$showShare, {
      
      showModal(
        modalDialog(title = "Application link",
          tags$textarea(class = "form-control", rows = "1", style = "resize:none;",
            readonly = "readonly",
            shareLink()
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
          footer = modalButton(label = NULL, icon = icon("xmark")),
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
    actionLink(inputId = "showShare", label = translate("shareLink")$title)
  )
  
  # Landing page
  source(file.path("serverFiles", "serverStart.R"), local = TRUE)
  output$start_page <- renderUI({
      
      source(file.path("uiFiles", "uiStart.R"), local = TRUE)$value
      
    })
  
  # Render tabpanel upon need
  observeEvent(input$tabs, {
      
      # Reset filters from other page
      results$searchId <- list()      
      
      # render only once
      req(!input$tabs %in% results$renderedTabs)
      
      switch(input$tabs,
        checklist_indicators = {
          
          output$indicators_content <- renderUI({
              source(file.path("uiFiles", "uiChecklist.R"), local = TRUE)$value
            })
          source(file.path("serverFiles", "serverChecklist.R"), local = TRUE)
          results$renderedTabs <- c(results$renderedTabs, "checklist_indicators")
          
        },
        species_information = {
          
          output$species_content <- renderUI({
              source(file.path("uiFiles", "uiSpecies.R"), local = TRUE)$value
            })
          source(file.path("serverFiles", "serverSpecies.R"), local = TRUE)
          results$renderedTabs <- c(results$renderedTabs, "species_information")
          
        },
        other_db = {
          
          results$renderedTabs <- c(results$renderedTabs, "other_db")
          
        }, faq = {
          
          results$renderedTabs <- c(results$renderedTabs, "faq")
          
        }
        
      )
      
    })
    
    dbServer(id = "dbPage")
    simpleHTMLPageServer(id = "about", language = reactive(results$language))
    simpleHTMLPageServer(id = "faq", language = reactive(results$language))
  
}