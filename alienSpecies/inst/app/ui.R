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
                   tags$script(HTML(paste0(
                     "$(document).on('shiny:connected', function() {",
                     "  if ($('#top-right-nav').length === 0) {",
                     "    var header = $('.navbar > .container');",
                     "    header.append('<div id=\"top-right-nav\" class=\"top-right-controls\">",
                     "      <span id=\"shareLink\" class=\"shiny-html-output\"></span>",
                     "      <span class=\"lang-links\">",
                     "        <a id=\"translate_en\" href=\"#\" class=\"action-button\">EN</a> - ",
                     "        <a id=\"translate_fr\" href=\"#\" class=\"action-button\">FR</a> - ",
                     "        <a id=\"translate_nl\" href=\"#\" class=\"action-button\">NL</a>",
                     "      </span>",
                     "      <span class=\"version-wrapper\">", versionUI(id = "main"), "</span>",
                     "    </div>');",
                     "    Shiny.bindAll(document.getElementById('top-right-nav'));",
                     "  }",
                     "});"
                   ))),
                   tags$script(
                     "Shiny.addCustomMessageHandler('openURL', function(data) {
                eval(data.message)
                });"
                   )
                 ),
                 
                 windowTitle = "Alien Species Portal",
                 fluid = FALSE, 
                 id = "tabs",
                 position = "fixed-top",
                 
                 # Main content
                 tabPanel(title = uiOutput("start_title"), value = "start",
                          uiOutput("start_page")),
                 tabPanel(title = uiOutput("species_title"), value = "species_information",
                          uiOutput("species_content")),
                 tabPanel(title = uiOutput("checklist_title"), value = "checklist_indicators",
                          uiOutput("indicators_content")),
                 dbUI(id = "dbPage"),
                 # TODO Remove check once we know the about HTML file is available in aspbo (https://github.com/inbo/aspbo/issues/468)
                 {
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
                   
                   if (any(grepl("^ABOUT.*\\.html$", about_files, ignore.case = TRUE))) {
                     simpleHTMLPageUI(id = "about")
                   }
                 },
                 simpleHTMLPageUI(id = "faq")
               )
               
      )
      
    )
    
  )
  
)