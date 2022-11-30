

#' Create table for display in the app
#' 
#' @param exotenData data.frame, as read from \code{\link{loadTabularData}}
#' @param unionlistData data.frame, as read from \code{\link{loadTabularData}}
#' @param occurrenceData data.frame, as read from \code{\link{loadTabularData}}
#' @return data.frame
#' 
#' @author mvarewyck
#' @importFrom stats aggregate
#' @export
tableIndicators <- function(exotenData, unionlistData, occurrenceData) {
  
  if (nrow(exotenData) == 0)
    return(NULL)
  
  tableData <- exotenData[, c("key", "species", "gbifLink", "first_observed", "last_observed", "habitat",
      "pathway", "degree_of_establishment", "sourceLink", "locality")]
  
  # combine locality & first_observed
  tableData$first_observed <- paste0(tableData$locality, ": ", tableData$first_observed)
  tableData$locality <- NULL
  
  # combine multiple rows
  combinedData <- sapply(c("first_observed", "degree_of_establishment", "pathway"), function(iName) {
      if (all(is.na(tableData[[iName]]))) {
        newData <- data.frame(key = unique(tableData$key), iName = NA)
      } else {
        newData <- aggregate(get(iName) ~ key, data = tableData, FUN = function(x)
            paste(unique(x), collapse = "</br>"))
      }
      colnames(newData)[2] <- iName
      newData[!duplicated(newData$key), ]
    }, simplify = FALSE)
  
  # Keep unique keys
  tableData <- tableData[!duplicated(tableData$key), ]
  for (iName in names(combinedData)) {
    tableData[, iName] <- NULL
    tableData <- merge(tableData, combinedData[[iName]], by = "key", all.x = TRUE)
  }
  
  # Add unionlist info
  tableData$unionColor <- c(NA, "orange")[tableData$species %in% unionlistData$scientificName + 1]
  # Add occurrence info
  tableData$occurColor <- c(NA, "green")[tableData$species %in% occurrenceData$scientificName + 1]
  
  tableData
  
}




#' Shiny module for creating the table \code{\link{tableIndicators}} - server side
#' @inheritParams welcomeSectionServer
#' @inheritParams tableIndicators
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import leaflet
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot webshot
#' @importFrom DT renderDT datatable
#' @export
tableIndicatorsServer <- function(id, exotenData, unionlistData, occurrenceData,
  uiText) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      selectedKey <- reactiveVal()
      
      
      output$disclaimerTableIndicators <- renderUI({
          
          tags$em(HTML(translate(uiText(), "tableIndicatorsDisclaimer")))
          
        })
      
      output$table <- renderDT({
          
          validate(need(nrow(exotenData()) > 0, "No data available"))
          
          tableData <- tableIndicators(exotenData = exotenData(),
            unionlistData = unionlistData,
            occurrenceData = occurrenceData)
          
          # More column
          tableData$more <- paste0('
              <div class="btn-group" role="group">
              ',
            # Button to union
            ifelse(!is.na(tableData$unionColor), 
              paste0('<button title="Union list" type="button" class="btn btn-default action-button" ',
                'data-inputid=', ns("union"), ' data-id=', tableData$key, ' onclick="activateTableLink(this);"', 
                '><i class="fa fa-star table-icon-', tableData$unionColor ,'"></i></button>'), ""),
            # Button to occurrence
            ifelse(!is.na(tableData$occurColor), 
              paste0('<button title="Occurrence" type="button" class="btn btn-default action-button" ',
                'data-inputid=', ns("occur"), ' data-id=', tableData$key, ' onclick="activateTableLink(this);"', 
                '><i class="fa fa-play table-icon-', tableData$occurColor ,'"></i></button>'), ""),
            '</div>')
          
          # data-inputid='%s' data-id='%s' onclick='activateTableLink(this);
          
          columnNames <- displayName(colnames(tableData), translations = uiText())
          
          
          DT::datatable(tableData, rownames = FALSE,
            selection = "single",
            colnames = columnNames,
            escape = FALSE, # display HTML code
            options = list(pageLength = 5,
              columnDefs = list(list(visible = FALSE, targets = c(0, 9, 10))))
          )
          
        })
      
      
      observeEvent(input$union, {
          
          selectedKey(input$union)
          
        })
      
      observeEvent(input$occur, {
          
          selectedKey(input$occur)
          
        })
      
      return(reactive(selectedKey()))
      
    })
  
} 



#' Shiny module for creating the table \code{\link{tableIndicators}} - UI side
#' @inheritParams welcomeSectionServer
#' @return UI object
#' 
#' @author mvarewyck
#' @import shiny
#' @importFrom DT DTOutput
#' @importFrom htmltools div
#' @export
tableIndicatorsUI <- function(id) {
  
  ns <- NS(id)
  
  div(style = "margin-top: 20px;",
    uiOutput(ns("disclaimerTableIndicators")),
    div(style = "margin-top: 20px;", DTOutput(ns("table")))
  )

}
