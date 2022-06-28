

observeEvent(input$start_navigate, {
    
      updateNavbarPage(session = session, inputId = "tabs", selected = input$start_navigate)
      
  })

observeEvent(input$tabs, {
    
    updateRadioButtons(session = session, inputId = "start_navigate", selected = input$tabs)
    
  })