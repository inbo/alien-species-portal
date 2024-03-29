shinyUI(
  
  bootstrapPage(
    
    ## For debugging
    uiOutput("debug"),
    
    shinyjs::useShinyjs(),
    
    ## Header
    ## ------
    
    tags$head(
      tags$link(rel = "stylesheet",
        href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css",
        integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u",
        crossorigin="anonymous"),
      tags$link(rel = "stylesheet", href = "style.css"),
      tags$link(rel = "stylesheet", href = "navbar.css"),
      tags$link(rel = "stylesheet", href = "buttonPopup.css"),
      includeScript("www/activateTableLink.js"),
      # Combo tree input
      shiny::singleton(shiny::tags$link(href = "comboTree.css", rel = "stylesheet")),
      shiny::singleton(shiny::tags$script(src = "comboTreeBinding.js")),
      shiny::singleton(shiny::tags$script(src = "comboTreePlugin.js"))
    ),
    
    # Load fontawesome library
    tags$span(icon("tag"), style = "display: none;"),
    
    
    ## Body
    ## ------
    
    tags$body(
      
      tags$div(class = "navbar1", 
        navbarPage(
          title = tags$div(
            HTML("&emsp;"),
            img(src = "logoTrias.png", height = "45px", style = "margin-right: 50px"), 
            img(src = "logo.png", float = "top", height = "45px"),
            style = "margin-top: -13px; margin-bottom: -13px; margin-left: -150px; margin-right: 50px;",
            tags$script(HTML(paste("var header = $('.navbar > .container');",
                  "header.append('<div style=\"float:right;\"><span class = \"version\">", 
                  paste0("v", packageVersion("alienSpecies")),"</span></div>')"))
            )),
          windowTitle = "Alien Species Portal",
          fluid = FALSE, 
          id = "tabs",
          position = "fixed-top",
          
          # Main content
          tabPanel(title = uiOutput("start_title"), value = "start",
            uiOutput("start_page")),
          tabPanel(title = uiOutput("checklist_title"), value = "global_indicators",
            uiOutput("indicators_content")),
          tabPanel(title = uiOutput("species_title"), value = "species_information",
            uiOutput("species_content")),
          
          # Shape data source + contact e-mail
          header = tags$header(tags$div(align = "right", style = "margin-top: 60px;",
              tags$p(
                tags$div(uiOutput("shareLink"), style = "display: inline-block;"),
                "-", 
                tags$a(id = "contact", href="mailto:faunabeheer@inbo.be?SUBJECT=Alien species web applicatie", target="_blank", "Contact")
              )
            )
          )
        )
      
      )
    
    )
  
  )

)
