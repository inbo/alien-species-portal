

output$start_title <- renderUI({
    
    results$translations$title[results$translations$plotFunction == tabChoices[1]]    
    
  })


output$start_tiles <- renderUI({
    
    choiceNames <- lapply(tabChoices, function(x) 
        results$translations$title[results$translations$plotFunction == x])
    
    radioButtons(inputId = "start_navigate", label = "", inline = TRUE,
      choiceValues = tabChoices,
      choiceNames = lapply(choiceNames, function(x)
          if (x == "Start") 
            "" else
            HTML(paste0("<div class='fotoTitel'>", x, "</div><div id='exampleFoto'></div>"))
      )
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