shinyUI(
  
  bootstrapPage(
    # Use i18n in UI for translations
    usei18n(i18n),
    
    ## For debugging
    uiOutput("debug"),
    
    shinyjs::useShinyjs(),
    tags$head(
      tags$script(HTML("
            $(document).on('shiny:connected', function() {
            // Function to update all leaflet attribution links
            function updateLeafletLinks() {
            $('.leaflet-control-attribution a').attr('target', '_blank');
            }
            
            setTimeout(updateLeafletLinks, 100);
            
            // Create observer for all future leaflet maps
            var observer = new MutationObserver(function(mutations) {
            updateLeafletLinks();
            });
            
            // Observe the entire body for new leaflet maps
            observer.observe(document.body, { 
            childList: true, 
            subtree: true 
            });
            });
            "))
    ),
    
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
            tags$a(href = "https://www.inbo.be", target = "_blank", 
              tags$img(src = "www/logo.png", height = "45px;")),
            tags$script(HTML(paste("var header = $('.navbar > .container');",
                  "header.append('<div style=\"float:right;\">", 
                  versionUI(id = "main"),"</div>')"))
            ),
            tags$script(
              "Shiny.addCustomMessageHandler('openURL', function(data) {
                eval(data.message)
                });"
            )
          ),
          
          # Shape data source + contact e-mail
          header = tags$header(
            tags$div(align = "right", style = "margin-top: 60px; padding-right: 15px;",
              tags$p(
                tags$div(uiOutput("shareLink"), style = "display: inline-block;"))
            ),
            tags$div(align = "right", style = "padding-right: 15px;",
              tags$p(
                actionLink(inputId = "translate_en", label = "EN"),
                "-",
                actionLink(inputId = "translate_fr", label = "FR"),
                "-", 
                actionLink(inputId = "translate_nl", label = "NL")
              )
          )),
          
          windowTitle = "Alien Species Portal",
          fluid = FALSE, 
          id = "tabs",
          position = "fixed-top",
          
          # Main content
          tabPanel(title = uiOutput("start_title"), value = "start",
            uiOutput("start_page")),
          tabPanel(title = uiOutput("checklist_title"), value = "checklist_indicators",
            uiOutput("indicators_content")),
          tabPanel(title = uiOutput("species_title"), value = "species_information",
            uiOutput("species_content")),
          dbUI(id = "dbPage"),
          faqUI(id = "faqPage")
        )
      
      )
    
    )
  
  )

)
