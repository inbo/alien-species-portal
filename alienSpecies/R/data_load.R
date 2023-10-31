
#' Read .RData shape data from s3 bucket
#' return a data list
#' @inheritParams readS3
#' @export
#' @author yzhang

loadShapeData <- function(file, 
                          bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies"))
                          ,...){
  tempEnv <- new.env()
  readS3(file = file, bucket = bucket, envir = tempEnv,...)
  return(tempEnv[[names(tempEnv)]])
}





#' Load tabular data
#' 
#' Data is preprocessed by createTabularData()
#' @inheritParams createTabularData
#' @return data.frame, loaded data
#' @author mvarewyck
#' @export

loadTabularData <- function(
    bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")),
    type = c("indicators", "unionlist", "occurrence")) {
  
  type <- match.arg(type)
  
  # For R CMD check
  rawData <- NULL  
  
  dataFile <-  switch(type,
         "indicators" = "data_input_checklist_indicators_processed.RData",
         "unionlist" = "eu_concern_species_processed.RData",
         "occurrence" = "be_alientaxa_cube_processed.RData"
  )
  
  readS3(file = dataFile, bucket = bucket, envir = environment())
  
  
  return(rawData)
  
}




#' Load meta data for the UI
#' @inheritParams loadTabularData 
#' @param type character, which type of translations should be loaded;
#' should be one of \code{c("ui","keys")}
#' @param language character, which language data sheet should be loaded;
#' should be one of \code{c("nl", "fr", "en")}
#' @return data.frame
#' 
#' @author mvarewyck
#' @importFrom utils read.csv
#' @export

loadMetaData <- function(type = c("ui", "keys"),
  #dataDir = system.file("extdata", package = "alienSpecies"), 
  bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")),
  language = c("nl", "fr", "en")) {
  
  type <- match.arg(type)
  language <- match.arg(language)
  # 
  # allData <- read.csv(file.path(dataDir, switch(type, 
  #       ui = "translations.csv",
  #       keys = "keys.csv"
  #     )), sep = if (type == "ui") ";" else ",", 
  #   encoding = "UTF-8") 
  # 
 fileName <- switch(type, 
         ui = "translations.csv",
         keys = "keys.csv"
  )
  
 allData <- readS3(FUN = read.csv,  sep = if (type == "ui") ";" else ",",encoding = "UTF-8", 
 file = fileName)
  
  filterData <- switch(type, 
    ui = {
      
      uiText <- allData[, c("title_id", paste0(c("title_", "description_"), language))]
      colnames(uiText) <- c("id", "title", "description")
      uiText <- uiText[uiText$id != "", ]
      
      if (any(duplicated(uiText$id)))
        stop("Following translations occur multiple times, please clean the file: ",
          paste(uiText$id[duplicated(uiText$id)], collapse = ", "))
      
      uiText
      
    },
    keys = allData
  )
  
  if (type == "ui")
    attr(filterData, "language") <- language
  
  
  return(filterData)
  
}





#' Create data with occupancy for t0 and t1 data
#' 
#' @author mvarewyck
#' @importFrom data.table dcast setDT as.data.table
#' @export

loadOccupancyData <- function() {
  
  readS3(file = "dfCube.RData")
  
  dfCube$cell_code10 <- NULL
  dfCube$year <- NULL
  dfTable <- dcast(data = setDT(as.data.frame(table(dfCube))), 
    species ~ source, value.var = "Freq")
  dfTable$total <- dfTable$t0 + dfTable$t1
  
  dfTable <- dfTable[order(dfTable$total), ]
  dfTable$species <- factor(dfTable$species, levels = unique(dfTable$species)) # sort by freq in barchart
  
  as.data.table(dfTable)
  
}

#' List Dutch names to replace English names for exoten
#' 
#' Will not return NA, but rather the original English name in case
#' no match could be found.
#' 
#' @param x character, what to transform
#' @param type character, defines how to transform short to full names
#' @return named character vector, names are the original values
#' 
#' @author eadriaensen
#' @export
#' 
getDutchNames <- function(x, type = c("regio")) {
  
  type <- match.arg(type)
  
  new <- switch(type,
    regio = c(
      "Belgi\u00EB" 	    = "Belgium",
      "Brussels Hoofdstedelijk Gewest" 	= "Brussels-Capital Region",
      "Vlaanderen" 	= "Flemish Region",
      "Walloni\u00EB"	= "Walloon Region"
    )
  )
  
  toReturn <- names(new)
  names(toReturn) <- new
  
  result <- toReturn[match(x, names(toReturn))]
  
  
  
  ## in case there was no match (i.e. because new schade code(s))
  ## keep the raw name anyway instead of NA
  if(any(is.na(result))){
    
    naPosition <- which(is.na(result))
    result[naPosition] <- x[naPosition]
    names(result)[naPosition] <- x[naPosition]
    
  }
  
  result
  
}


#' Translate text given id
#' @param data data.frame with columns title and id
#' @param id character, row identifier for the \code{data}
#' 
#' @return character 
#' 
#' @author mvarewyck
#' @export
translate <- function(data = loadMetaData(type = "ui"), id) {
  
  # id NA
  if (all(is.na(id)))
    return(data)
  
  # Helpfull during development to see which are missing
  # can be turned of in production
  if (!is.null(data) & !all(id %in% data$id)) {
    if (!all(is.na(id[!id %in% data$id])))
      warning("Not in translation file: ", vectorToTitleString(id[!id %in% data$id]))
  }
  
  data <- rbind(
    data,
    # empty if no match
    data.frame(id = id, title = id, description = "")
  )
  
  data[match(id, data$id), c("title", "description")]
  
}



# #' Extract the vernacular names using API requests
# #' 
# #' WIP: Currently all missing for this dataset https://www.gbif.org/dataset/6d9e952f-948c-4483-9807-575348147c7e
# #' WARNING: Takes long time to process
# #' @inheritParams loadTabularData
# #' @param taxonKeys numeric vector, taxon keys for GBIF
# #' @return no return value, data file 'vernacular_names.csv' is written to \code{dataDir}
# #' data.frame with
# #' \itemize{
# #' \item{key}{\code{taxonKeys} entered as input}
# #' \item{name}{character, vernacular name (language); multiple names are pasted togeter}
# #' }
# #' @author mvarewyck
# #' @importFrom utils write.csv
# #' @importFrom httr GET content
# #' @export
# #' @examples
##' fullData <- rgbif::name_lookup(
##'  query = "Tricellaria",
##'  datasetKey = "0a2eaf0c-5504-4f48-a47f-c94229029dc8",
##'  limit = 10000)
##'fullData$names
##'fullData$data$key
##'
##'myRequest <- httr::GET("https://api.gbif.org/v1/species/157131005/vernacularNames")
##'httr::content(myRequest)$results
#getVernacularNames <- function(dataDir = system.file("extdata", package = "alienSpecies"),
#  taxonKeys) {
#  
#  extractedNames <- sapply(taxonKeys, function(iKey) {
#      
#      # test: iKey <- 157131005
#      # https://api.gbif.org/v1/species/152543101/vernacularNames
#      myRequest <- httr::GET(paste0("https://api.gbif.org/v1/species/", iKey, "/vernacularNames"))
#      allNames <- httr::content(myRequest)$results
#      if (length(allNames) == 0)
#        return("") else
#        paste(sapply(allNames, function(x) paste0(x$vernacularName, " (", x$language, ")")), 
#          collapse = "</br>")
#      
#    })
#  
#  newData <- data.frame(
#    key = taxonKeys,
#    name = extractedNames)
#  
#  
#  write.csv(newData, file.path(dataDir, "vernacular_names.csv"))
#  
#  
#}
