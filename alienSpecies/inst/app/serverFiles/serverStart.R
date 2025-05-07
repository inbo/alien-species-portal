

output$start_title <- renderUI({
    
    translate(results$translations, id = tabChoices[1])$title    
    
  })


output$start_tiles <- renderUI({
    
    tileChoices <- tabChoices[-1]
    tileNames <- lapply(tileChoices, function(iChoice){
        foto <- list.files(path = system.file("app", "www", package = "alienSpecies"), pattern = iChoice)
        title <- translate(data = results$translations, id = iChoice)$title
        HTML(paste0(
            "<div class='radio-tiles-title'>", title, "</div>",
            "<div class='radio-tiles-image'>", 
            img(src = foto, width = "100%", `aspect-ratio` = "400/270"), "</div>"
          ))
      })
    
    tags$div(style = "margin-top: -20px;",
      radioButtons(
        inputId = "start_navigate", label = "", inline = TRUE,
        choiceValues = tileChoices, choiceNames = tileNames,
        selected = character(0)
      ),
      tags$script("$('.radio-inline').addClass('radio-tiles');")
    )
      
  })


observeEvent(input$start_navigate, {
    
    switch(input$start_navigate, 
      "early_warning" = session$sendCustomMessage(type = "openURL", list(message = "
            window.open('https://alert.riparias.be', '_blank').focus(); 
            ")),
      updateNavbarPage(session = session, inputId = "tabs", selected = input$start_navigate)
    )
    
  })

observeEvent(input$tabs, {
    
    updateRadioButtons(session = session, inputId = "start_navigate", selected = input$tabs)
    
  })


# Titles for pages in navbar
output$checklist_title <- renderUI({
    
    translate(results$translations, id = tabChoices[2])$title  
    
  })

output$species_title <- renderUI({
    
    translate(data = results$translations, id = tabChoices[3])$title  
    
  })

output$early_title <- renderUI({
    
    translate(results$translations, id = tabChoices[4])$title  
    
  })

output$db_title <- renderUI({
    
    translate(data = results$translations, id = tabChoices[5])$title  
    
  })