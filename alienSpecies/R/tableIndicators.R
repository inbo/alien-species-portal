

#' Create table for display in the app
#' 
#' @inheritParams displayName
#' @param exotenData data.frame
#' @return datatable object
#' 
#' @author mvarewyck
#' @importFrom DT datatable
#' @importFrom stats aggregate
#' @export
tableIndicators <- function(exotenData, unionlistData, occurrenceData, translations = NULL) {
  
  tableData <- exotenData[, c("key", "species", "gbifLink", "first_observed", "last_observed", "habitat",
      "pathway", "degree_of_establishment", "sourceLink", "locality")]
  
  # combine locality & first_observed
  tableData$first_observed <- paste0(tableData$locality, ": ", tableData$first_observed)
  tableData$locality <- NULL
  # combine multiple rows
  combinedData <- sapply(c("first_observed", "degree_of_establishment", "pathway"), function(iName) {
      newData <- aggregate(get(iName) ~ key, data = tableData, FUN = function(x)
          paste(unique(x), collapse = "</br>"))
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
  tableData$unionColor <- c(NA, "black")[tableData$species %in% unionlistData$scientificName + 1]
  # Add occurrence info
  tableData$occurColor <- c(NA, "black")[tableData$species %in% occurrenceData$scientificName + 1]
  # More column
  tableData$more <- paste0('
        <div class="btn-group" role="group">
        ',
      # Button to remove record
      ifelse(!is.na(tableData$unionColor), 
          paste0('<button title="Union list" type="button" class="fa fa-star" id=union_', 
            rownames(tableData), '></button>'), ""),
      # Button to edit record
    ifelse(!is.na(tableData$occurColor), 
      paste0('<button title="Occurrence" type="button" class="fa fa-play" id=occur_', 
        rownames(tableData), '></button>'), ""),
    '</div>')
    
  columnNames <- displayName(colnames(tableData), translations = translations)
  
  
  DT::datatable(tableData, rownames = FALSE,
    selection = "single",
    colnames = columnNames,
    escape = FALSE, # display HTML code
    options = list(pageLength = 5,
      columnDefs = list(list(visible = FALSE, targets = c(0, 9, 10))))
  )
  
}
