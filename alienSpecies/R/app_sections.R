# Text sections in the shiny app
# 
# Author: mvarewyck
###############################################################################




#' Shiny module for creating welcome section - server side
#' @param id character, unique identifier
#' @return no return value
#' 
#' @author mvarewyck
#' @export
welcomeSectionServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
      
      tmpTranslation <- reactive(translate(paste0(id, "-welcome")))
      
      output$welcomeTitle <- renderUI(h1(HTML(tmpTranslation()$title)))
      
      output$welcomeMain <- renderUI(HTML(tmpTranslation()$description))
      
    })
}
#' Section for welcoming (top of the page)
#' @param id character, from which page this function is called
#' e.g. 'wbe'
#' @template moduleUI 
#' 
#' @author mvarewyck
#' @export
welcomeSectionUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    tags$div(align = "center",
      uiOutput(ns("welcomeTitle"))
    ),
    uiOutput(ns("welcomeMain"))
  )
  
  
}


#' Shiny module for creating footer section - server side
#' @inheritParams welcomeSectionServer
#' @return reactive for creating the report
#' 
#' @author mvarewyck
#' @import shiny
#' @export
footerSectionServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
      
      ns <- session$ns    
      
      ## SUBMIT & DOWNLOAD report ##
      
      observe({
          
          updateActionButton(inputId = "createReport", 
            label = translate(id = "createReport")$title)
          
        })
      
      
      ## REPORT missing and CONTACT ##
      observeEvent(input$contact, {
          
          showModal(
            modalDialog(
              title = "Contact",
              footer = modalButton(label = NULL, icon = icon("xmark")),
              easyClose = TRUE,
              
              tags$h5(translate("contactMissing")$title, ":"),
              tags$a(href = "https://waarnemingen.be/fieldwork/observations/create/", target="_blank", 
                "https://waarnemingen.be/fieldwork/observations/create/"),
              tags$br(),
              tags$a(href = "https://www.inaturalist.org/observations/upload", target = "_blank",
                "https://www.inaturalist.org/observations/upload"),
              tags$h5(translate("contactApp")$title, ":"),
              tags$a(href="mailto:faunabeheer@inbo.be?subject=Alien%20species%20web%20applicatie&body=**Describe%20the%20bug**%0AA%20clear%20and%20concise%20description%20of%20what%20the%20bug%20is.%0A%0A**To%20Reproduce**%0ASteps%20to%20reproduce%20the%20behavior%3A%0A1.%20Go%20to%20%27...%27%0A2.%20Click%20on%20%27....%27%0A3.%20Scroll%20down%20to%20%27....%27%0A4.%20See%20error%0A%0A**Expected%20behavior**%0AA%20clear%20and%20concise%20description%20of%20what%20you%20expected%20to%20happen.%0A%0A**Screenshots**%0AIf%20applicable%2C%20add%20screenshots%20to%20help%20explain%20your%20problem.%0A%0A**Desktop%20%28please%20complete%20the%20following%20information%29%3A**%0A%20-%20OS%3A%20%5Be.g.%20iOS%5D%0A%20-%20Browser%20%5Be.g.%20chrome%2C%20safari%5D%0A%20-%20Version%20%5Be.g.%2022%5D%0A%0A**Smartphone%20%28please%20complete%20the%20following%20information%29%3A**%0A%20-%20Device%3A%20%5Be.g.%20iPhone6%5D%0A%20-%20OS%3A%20%5Be.g.%20iOS8.1%5D%0A%20-%20Browser%20%5Be.g.%20stock%20browser%2C%20safari%5D%0A%20-%20Version%20%5Be.g.%2022%5D%0A%0A**Additional%20context**%0AAdd%20any%20other%20context%20about%20the%20problem%20here.", target="_blank", 
                "faunabeheer@inbo.be")              
            )
          )
        
          
        })
      
      return(reactive(input$createReport))
      
      
    })
  
}

#' #' Shiny module for creating footer section - UI side
#' @param id character, unique identifier
#' @param showReport boolean, whether to show a button to download report
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
footerSectionUI <- function(id, showReport = FALSE) {
  
  ns <- NS(id)
  
  tags$div(style = "margin-bottom: 70px;",
    
    tags$div(class = "footer",
      tags$div(class = "footer-content",
        
        actionButton(inputId = ns("contact"), label = "Contact", 
          icon = icon("envelope")),
        
        if (showReport)
          tagList(
            singleton(
              tags$head(tags$script(src = "triggerDownload.js"))
            ),
            actionButton(inputId = ns("createReport"), label = "Create report", 
              icon = icon("file-pdf")),
            downloadLink(ns("downloadReport"), " ", class = "invisible")
          )
      )
    )
  )

}

#' Replace \code{"{{fields}}"} in title/description translations
#' @param text character, input from translation
#' @param params named list, all parameters that should be replaced with 
#' their value
#' @return character, modified for the \code{params} mentioned in the text  
#' 
#' @author mvarewyck
#' @export
decodeText <- function(text, params) {
  
  newText <- text
  
  for (iParam in names(params)) {
    
    newText <- if (iParam == "period")
        paste(newText, yearToTitleString(params[[iParam]])) else
        gsub(paste0("\\{\\{", iParam, "\\}\\}"), params[[iParam]], newText)
    
  }
  
  HTML(newText)
  
}

#' Link with version info - UI side
#' 
#' @inherit welcomeSectionUI
#' @importFrom utils packageVersion
#' @export
versionUI <- function(id) {
  
  actionLink(inputId = NS(id, "version"), 
    label = paste0("v", packageVersion("alienSpecies")),
    class = "version")
  
}


#' Link with version info - server side
#' @inherit welcomeSectionServer
#' @importFrom utils packageVersion sessionInfo
#' @export
versionServer <- function(id) {
  
  moduleServer(id,
    function(input, output, session) {
      
      observeEvent(input$version, {
          
#          # For internal testing
          ## PRD
#          Sys.setenv("GIT_SHA" = system("git describe --tags `git rev-list --tags --max-count=1`", intern = TRUE))
          ## UAT
#          Sys.setenv("GIT_SHA" = system("git rev-parse HEAD", intern = TRUE))
          hashCode <- Sys.getenv("GIT_SHA")
          
          
          showModal(
            modalDialog(
              fluidPage(
                paste("R package:", packageVersion("alienSpecies")),
                tags$br(),
                "GIT:", if (hashCode == "") 
                    translate("unknown")$title else 
                    tags$a(id = "gitVersion", 
                      href = if (Sys.getenv("R_CONFIG_ACTIVE") == "production") {
                          paste0("https://github.com/inbo/alien-species-portal/releases/tag/", hashCode)
                        } else {
                          paste0("https://github.com/inbo/alien-species-portal/commit/", hashCode)
                        }, 
                      target = "_blank", hashCode),
                tags$br(),
                actionLink(inputId = session$ns("showInfo"), label = "R Session Info"),
                conditionalPanel("input.showInfo % 2 == 1", ns = session$ns,
                  verbatimTextOutput(session$ns("sessionInfo"))
                )
              ), 
              title = translate("version")$title,
              size = "l", easyClose = TRUE
            ))
          
        })
      
      output$sessionInfo <- renderPrint(sessionInfo())
      
    })
}


#' Shiny module for including html file - server side
#' @param id character, unique identifier
#' @param species reactive object, taxonkey for the selected species
#' @param language reactive object, language for UI content
#' @param url character, url to be included in the link (back-end)
#' @param linkText character, text to be displayed for the url (front-end)
#' @return no return value
#' 
#' @author mvarewyck
#' @importFrom htmltools includeHTML
#' @importFrom stats na.omit
#' @export
htmlSectionServer <- function(id, species, language, url = NA, linkText) {
  
  moduleServer(id, function(input, output, session) {
      
      output$addLinks <- renderUI({
          
          dataPath <- file.path("https://raw.githubusercontent.com/inbo/aspbo",
            if (Sys.getenv("R_CONFIG_ACTIVE") == "production") "main" else "uat",
            "HTML_pages/HTML")
          dataFile <- file.path(dataPath, paste0(species(), "_", language(), ".html"))
          
          url <- na.omit(url)
          if (length(url) > 0) {
            lapply(seq_along(url), function(i)
                  tags$p(tags$a(href = url[i], target = "_blank", linkText[i]))) 
          } else if (httr::http_status(httr::GET(dataFile))$category != "Client error") {
            includeHTML(dataFile)
          }
        
        })
      
    })
}


#' Shiny module for including html file - UI side
#' @inherit welcomeSectionUI
#' 
#' @author mvarewyck
#' @export
htmlSectionUI <- function(id) {
  
  ns <- NS(id)
  
  tags$div(style = "margin-top: 20px;",
    uiOutput(ns("addLinks"))
  )
  
}

