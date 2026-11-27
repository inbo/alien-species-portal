# Allows to click the same tile twice: reset value after clicking
resetNavigation <- reactiveVal(FALSE)

output$start_title <- renderUI({
  translate(id = tabChoices[1])$title    
})

output$start_welcome_text <- renderUI({
  # Uses translation if available, or falls back to your Dutch title
  welcome <- translate(id = tabChoices[1])$description
  if (is.null(welcome) || welcome == "") {
    "Welkom op het exotenportaal van het Instituut voor Natuur- en Bosonderzoek"
  } else {
    welcome
  }
})

output$start_tiles <- renderUI({
  tileChoices <- tabChoices[-1]
  
  # TODO Remove check once we know the about HTML file is available in aspbo (https://github.com/inbo/aspbo/issues/468)
  about_files <- tryCatch({
    pat <- Sys.getenv("GITHUB_PAT")
    headers <- if (nzchar(pat)) httr::add_headers(Authorization = paste("token", pat)) else NULL
    res <- httr::GET(
      paste0(
        "https://api.github.com/repos/inbo/aspbo/contents/HTML_pages/HTML?ref=",
        if (Sys.getenv("R_CONFIG_ACTIVE") == "production") "main" else "uat"
      ),
      headers
    )
    if (httr::status_code(res) == 200) {
      parsed <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
      if (is.data.frame(parsed) && "name" %in% names(parsed)) parsed$name else character(0)
    } else {
      character(0)
    }
  }, error = function(e) character(0))
  
  if (!any(grepl("^ABOUT.*\\.html$", about_files, ignore.case = TRUE))) {
    tileChoices <- tileChoices[tileChoices != "about"]
  }
  
  tileNames <- lapply(tileChoices, function(iChoice) {
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
  
  tags$div(
    style = "margin-top: -20px;",
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
output$species_title <- renderUI({
  translate(id = "species_information")$title  
})

output$checklist_title <- renderUI({
  translate(id = "checklist_indicators")$title  
})

# Pictures for carousel
output$hero_carousel <- slickR::renderSlickR({
  imgs <- list.files(
    path = system.file("app", "www", package = "alienSpecies"),
    pattern = "^carousel",
    full.names = TRUE
  )
  
  slickR::slickR(
    obj = imgs,
    slideId = "carousel",
    slideType = "img",
    height = "100%",
    width = "100%",
    padding = 0
  ) +
    slickR::settings(
      autoplay = TRUE,
      autoplaySpeed = 5000,
      dots = TRUE,
      arrows = FALSE,
      fade = TRUE,
      infinite = TRUE,
      pauseOnHover = FALSE
    )
})