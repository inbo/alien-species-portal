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
                  "header.append('<div style=\"float:right;\">", 
                  versionUI(id = "main"),"</div>')"))
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
                tags$a(id = "contact", href="mailto:mailto:faunabeheer@inbo.be?subject=Alien%20species%20web%20applicatie&body=**Describe%20the%20bug**%0AA%20clear%20and%20concise%20description%20of%20what%20the%20bug%20is.%0A%0A**To%20Reproduce**%0ASteps%20to%20reproduce%20the%20behavior%3A%0A1.%20Go%20to%20%27...%27%0A2.%20Click%20on%20%27....%27%0A3.%20Scroll%20down%20to%20%27....%27%0A4.%20See%20error%0A%0A**Expected%20behavior**%0AA%20clear%20and%20concise%20description%20of%20what%20you%20expected%20to%20happen.%0A%0A**Screenshots**%0AIf%20applicable%2C%20add%20screenshots%20to%20help%20explain%20your%20problem.%0A%0A**Desktop%20%28please%20complete%20the%20following%20information%29%3A**%0A%20-%20OS%3A%20%5Be.g.%20iOS%5D%0A%20-%20Browser%20%5Be.g.%20chrome%2C%20safari%5D%0A%20-%20Version%20%5Be.g.%2022%5D%0A%0A**Smartphone%20%28please%20complete%20the%20following%20information%29%3A**%0A%20-%20Device%3A%20%5Be.g.%20iPhone6%5D%0A%20-%20OS%3A%20%5Be.g.%20iOS8.1%5D%0A%20-%20Browser%20%5Be.g.%20stock%20browser%2C%20safari%5D%0A%20-%20Version%20%5Be.g.%2022%5D%0A%0A**Additional%20context**%0AAdd%20any%20other%20context%20about%20the%20problem%20here.", target="_blank", "Contact")
              )
            )
          )
        )
      
      )
    
    )
  
  )

)
