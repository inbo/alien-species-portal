

#' Create table for display in the app
#' 
#' @inheritParams displayName
#' @param exotenData data.frame
#' @return datatable object
#' 
#' @author mvarewyck
#' @importFrom DT datatable
#' @export
tableIndicators <- function(exotenData, translations = NULL) {
  
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
  
  columnNames <- displayName(colnames(tableData), translations = translations)
  
  
  DT::datatable(tableData, rownames = FALSE,
    selection = "single",
    colnames = columnNames,
    escape = FALSE, # display HTML code
    options = list(pageLength = 5,
      columnDefs = list(list(visible = FALSE, targets = 0))))  
  
}
