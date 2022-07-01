# TODO: Add comment
# 
# Author: mvarewyck
###############################################################################



#' Hierarchical selection input
#' 
#' Source: https://stackoverflow.com/a/60315446
#' @param inputId character, unique identifier for the input object
#' @param choices list, choices to select from 
#' @param multiple boolean, whether user can select multiple options
#' @param cascaded boolean, whether to have cascaded view
#' @return div object
#' 
#' @author mvarewyck
#' @import htmltools
#' @importFrom shiny singleton
#' @importFrom jsonlite toJSON
#' @export
comboTreeInput <- function(inputId, choices, multiple = TRUE, cascaded = FALSE,
  placeholder = "All taxa"){
  
  tags$div(style = "width: 100%; height: 50px;",
    shiny::singleton(shiny::tags$link(href = "comboTree.css", rel = "stylesheet")),
    shiny::singleton(shiny::tags$script(src = "comboTreeBinding.js")),
    shiny::singleton(shiny::tags$script(src = "comboTreePlugin.js")),
    tags$input(id = inputId, class = "comboTree", type = "text", 
      placeholder = placeholder,
      `data-choices` = as.character(toJSON(choices, auto_unbox = TRUE)),
      `data-multiple` = ifelse(multiple, "true", "false"), 
      `data-cascaded` = ifelse(cascaded, "true", "false")
    )
  )
}
