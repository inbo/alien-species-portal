
# Allows to click the same tile twice: reset value after clicking
resetNavigation <- reactiveVal(FALSE)

output$start_title <- renderUI({
    
    translate(id = tabChoices[1])$title    
    
  })


output$start_tiles <- renderUI({
    
    tileChoices <- tabChoices[-1]
    
    # TODO Remove check once we know the about HTML file is available in aspbo (https://github.com/inbo/aspbo/issues/468)
    if (!any(grepl("^ABOUT.*\\.html$", jsonlite::fromJSON(httr::content(httr::GET(paste0("https://api.github.com/repos/inbo/aspbo/contents/HTML_pages/HTML?ref=", if (Sys.getenv("R_CONFIG_ACTIVE") == "production") "main" else "uat")), "text", encoding = "UTF-8"))$name, ignore.case = TRUE))) {
      tileChoices <- tileChoices[tileChoices != "about"]
    }
    
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

output$start_page <- renderUI({
  img <- list.files(
    path = system.file("app", "www", package = "alienSpecies"),
    pattern = "^carousel",
    full.names = TRUE
  )
  
  div(
    class = "front-hero-wrapper",
    
    # 1. Background 4-image slickR slideshow
    slickR::slickR(
      obj = img,
      slideId = "carousel",
      slideType = "img",
      width = "100%",
      padding = 0
    ) +
      slickR::settings(
        dots = TRUE,
        arrows = FALSE,
        autoplay = TRUE,
        autoplaySpeed = 5000,
        fade = TRUE
      ),
    
    # 2. Dark gradient overlay for readable white text
    div(class = "front-hero-overlay"),
    
    # 3. Foreground content: centered welcome text + bottom horizontal tiles
    div(
      class = "front-hero-content",
      
      div(
        class = "front-welcome-box",
        tags$h1(
          class = "front-welcome-title",
          "Welkom op het exotenportaal van het Instituut voor Natuur- en Bosonderzoek"
        )
      ),
      
      div(
        class = "front-bottom-tiles",
        uiOutput("start_tiles")
      )
    )
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
