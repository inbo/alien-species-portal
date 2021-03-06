shinyUI(
  
  bootstrapPage(
    
    ## For debugging
    uiOutput("debug"),
    
    
    ## Header
    ## ------
    
    tags$head(
#                        tags$meta(charset = "utf-8"),
#                        tags$meta(name="viewport", content="width=device-width, initial-scale=1, shrink-to-fit=no"),
      tags$link(rel = "stylesheet",
        href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css",
        integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u",
        crossorigin="anonymous"),
      tags$link(rel = "stylesheet", href = "style.css"),
      tags$link(rel = "stylesheet", href = "navbar.css")
    ),
    
    
    
    ## Body
    ## ------
    
    tags$body(
      
      tags$div(class = "navbar1", 
        navbarPage(title = tags$div(
            HTML("&emsp;"),
            img(src = "logo.png", float = "top", height = "45px"),
            style = "margin-top: -13px; margin-bottom: -13px",
            tags$script(HTML(paste("var header = $('.navbar > .container');",
                  "header.append('<div style=\"float:right;\"><span class = \"version\">", 
                  paste0("v", packageVersion("alienSpecies")),"</span></div>')"))
            )),
          windowTitle = "Alien Species Portal",
          fluid = FALSE, 
          id = "tabs",
          position = "fixed-top",
          
          # Main content
          tabPanel(title = "Checklists", id = "tab-checklist",
            uiOutput("checklist_content")),
          
          # Shape data source + contact e-mail
          header = tags$header(tags$div(align = "right",
              tags$p(
                tags$a(id = "geodataBron", href="http://www.geopunt.be/download?container=referentiebestand-gemeenten&title=Voorlopig%20referentiebestand%20gemeentegrenzen#", target="_blank", "Geodata bron"),
                "-", 
                tags$a(id = "contact", href="mailto:faunabeheer@inbo.be?SUBJECT=Alien species web applicatie", target="_blank", "Contact")
              ),
              tags$p(
                actionLink(inputId = "translate_nl", label = "nl"),
                "-", 
                actionLink(inputId = "translate_fr", label = "fr"),
                "-", 
                actionLink(inputId = "translate_en", label = "en")
              ), style = "margin-top: 55px")
          )
        )
      
      )
    
    )
  
  
  ## Footer
  ## ------                        
  
#                        tags$footer(tags$div(class="container", tags$img(src="logo.png")))
  
  )

)
