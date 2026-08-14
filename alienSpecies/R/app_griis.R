# Server and UI module for the GRIIS tab (Species Information)
#
# Author: mvarewyck
###############################################################################


#' GRIIS species detail - server module
#'
#' Shows a two-column (field, value) table with GRIIS checklist detail
#' for the currently selected species
#' @inheritParams plotModuleServer
#' @param exotenData data.frame, as read from \code{\link{loadTabularData}}
#' @param species reactive character, scientific name of the currently selected species
#' @param language reactive character, currently selected UI language
#' @return no return value
#'
#' @author mvarewyck
#' @export
griisServer <- function(id, exotenData, species, language) {

  moduleServer(id, function(input, output, session) {

      output$table <- DT::renderDT({

          req(species())

          # `input$species_choice` is a taxonKey from the occurrence cube (be_alientaxa_info.csv),
          # a different id space than exotenData$key - species name is the only reliable join field
          row <- exotenData[exotenData$species == species() & exotenData$locality == "België", ]

          validate(need(nrow(row) > 0, translate("noData")$title))

          vernacularName <- row[[paste0("vernacular_name_", language())]][1]

          # Same columns (and labels) as the Checklist Indicators > Taxa table
          labelKeys <- c("species", "vernacular_name_col", "gbifLink", "last_observed",
            "habitat", "sourceLink")
          values <- c(row$species[1], vernacularName, row$gbifLink[1],
            row$last_observed[1], row$habitat[1], row$sourceLink[1])

          DT::datatable(
            data.frame(field = tools::toTitleCase(names(displayName(labelKeys))), value = values),
            rownames = FALSE, colnames = c("", ""), escape = FALSE,
            options = list(dom = "t", paging = FALSE)
          )

        })

    })

}

#' GRIIS species detail - module UI
#'
#' @inheritParams plotModuleServer
#' @return UI object
#'
#' @author mvarewyck
#' @export
griisUI <- function(id) {

  ns <- NS(id)

  DT::dataTableOutput(ns("table"))

}
