# Server and UI module for extra DB page
# 
# Author: mvarewyck
###############################################################################


#' Extra DB page - server module
#' 
#' @inheritParams plotModuleServer 
#' @return no return value
#' 
#' @author mvarewyck
#' @export
dbServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {

      ns <- session$ns

      output$title <- renderUI({

          translate(id = "other_db")$title

        })

      output$content <- renderUI({

          df <- read.csv(system.file("data", "other_dashboards.csv", package = "alienSpecies"),
            stringsAsFactors = FALSE)

          cards <- apply(df, 1, function(db) {

              foto <- list.files(
                path = system.file("app", "www", package = "alienSpecies"),
                pattern = db["id"]
              )

              translation <- translate(id = db["translationId"])
              title <- translation$title
              description <- translation$description

              tags$div(
                class = "db-link-card",

                tags$h1(
                  class = "db-link-title",
                  tags$a(title, href = db["url"], target = "_blank")
                ),

                tags$div(
                  class = "db-link-body",

                  tags$div(
                    class = "db-link-image",
                    tags$img(src = foto)
                  ),

                  tags$div(
                    class = "db-link-content",
                    tags$p(HTML(description)),
                    tags$em(
                      "URL link: ",
                      tags$a(title, href = db["url"], target = "_blank")
                    )
                  )
                )
              )
            })

          tags$div(
            class = "db-link-wrapper",
            cards
          )

        })

    })
  
}

#' Extra DB page - module UI
#' 
#' @inheritParams plotModuleServer
#' @return UI object, \code{tabPanel} object that can be included in 
#' \code{navbarPage}
#' 
#' @author mvarewyck
#' @export
dbUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(title = uiOutput(ns("title")), value = "other_db",
    tags$div(class = "noButton", style = "margin-top:20px;", 
      uiOutput(ns("content")))
  )  
  
}

