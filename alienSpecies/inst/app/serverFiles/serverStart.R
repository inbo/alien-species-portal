

output$start_title <- renderUI({
    
    translate(results$translations, id = tabChoices[1])$title    
    
  })


output$start_tiles <- renderUI({
    
    radioButtons(inputId = "start_navigate", label = "", inline = TRUE,
      choiceValues = tabChoices,
      choiceNames = lapply(tabChoices, function(x)
          if (x == "start")
            "" else 
          HTML(paste0("<div class='fotoTitel'>", 
              translate(data = results$translations, id = x)$title, 
            "</div><div id='", x, "Foto'></div>"))
      )
    )
  
  })


output$start_disclaimer <- renderUI({
    
    if (doDebug)
      tagList(
        tags$b("Disclaimer"),
        tags$ul(
          lapply(c(attr(exotenData, "warning"), attr(occurrenceData, "warning")), function(iText)
              tags$li(iText)
          )
        ),
        helpText("Only shown in debug mode")
      )
  
  })


observeEvent(input$start_navigate, {
    
    if (input$start_navigate == "early_warning")
      browseURL(url = "https://alert.riparias.be") else
      updateNavbarPage(session = session, inputId = "tabs", selected = input$start_navigate)
      
  })

observeEvent(input$tabs, {
    
    updateRadioButtons(session = session, inputId = "start_navigate", selected = input$tabs)
    
  })