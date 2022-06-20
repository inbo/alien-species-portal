

observeEvent(input$start_navigate, {
    
      updateNavbarPage(session = session, inputId = "tabs", selected = input$start_navigate)
    
  })
