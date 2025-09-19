
# Allows to click the same tile twice: reset value after clicking
resetNavigation <- reactiveVal(FALSE)

output$start_title <- renderUI({
    
    translate(id = tabChoices[1])$title    
    
  })


output$start_tiles <- renderUI({
    
    tileChoices <- tabChoices[-1]
    tileNames <- lapply(tileChoices, function(iChoice){
        foto <- list.files(path = system.file("app", "www", package = "alienSpecies"), pattern = iChoice)
        title <- translate(id = iChoice)$title
        hover <- translate(id = iChoice)$description
        HTML(paste0(
            "<div class='radio-tiles-title' title='", hover, "'>", title, "</div>",
            "<div class='radio-tiles-image'>", 
            img(src = foto, width = "100%", `aspect-ratio` = "400/270", title = hover), "</div>"
          ))
      })
    
    if (resetNavigation())
      resetNavigation(FALSE)
    
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
    
    updateNavbarPage(session = session, inputId = "tabs", selected = input$start_navigate)
    resetNavigation(TRUE)
    
  })

observeEvent(input$tabs, {
    
    updateRadioButtons(session = session, inputId = "start_navigate", selected = input$tabs)
    
  })


# Titles for pages in navbar
output$checklist_title <- renderUI({
    
    translate(id = tabChoices[2])$title  
    
  })

output$species_title <- renderUI({
    
    translate(id = tabChoices[3])$title  
    
  })
