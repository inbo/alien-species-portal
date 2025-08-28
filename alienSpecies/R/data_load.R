
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
#' @return data.frame or data.table, loaded data; except for \code{code == 'timeseries'}
#' it loads pointer to the data of which a subset can be loaded using 
#' \code{dplyr::collect()}
#' @author mvarewyck
#' @importFrom arrow read_parquet open_dataset
#' @importFrom data.table as.data.table
#' @export

loadTabularData <- function(
    bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")),
    type = c("indicators", "unionlist", "occurrence", "timeseries", "taxachoices")) {
  
  type <- match.arg(type)
  
  dataFile <-  switch(type,
         "indicators" = "data_input_checklist_indicators_processed.parquet",
         "unionlist" = "eu_concern_species_processed.parquet",
         "occurrence" = "be_alientaxa_cube_processed.parquet",
         "timeseries" = "full_timeseries.parquet",
         "taxachoices" = "taxachoices_processed.parquet"
  )
  
  rawData <- if (type == "timeseries")
    open_dataset(file.path("s3:/", bucket, dataFile)) else
    read_parquet(file = file.path("s3:/", bucket, dataFile))
  
  message(attr(rawData, "warning"))

  if (type == "indicators")
    attr(rawData, "habitats") <- c("marine", "freshwater", "terrestrial")
  
  return(rawData)
  
}




#' Load meta data for the UI
#' @inheritParams loadTabularData 
#' @param type character, which type of translations should be loaded;
#' should be one of \code{c("ui", "keys", "harmonia")}
#' @param language character, which language data sheet should be loaded;
#' should be one of \code{c("nl", "fr", "en")}
#' @param local boolean, whether to use local translation file in
#' \code{system.file("extdata", "translations.csv", package = "alienSpecies")}
#' @return data.frame
#' 
#' @author mvarewyck
#' @importFrom utils read.csv
#' @export

loadMetaData <- function(type = c("ui", "keys", "harmonia"),
  bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")),
  language = c("nl", "fr", "en"),
  local = FALSE) {
  
  type <- match.arg(type)
  language <- match.arg(language)
   
  fileNames <- switch(type, 
    ui = paste0("translations", c("", "_simple", "_regions")),
    keys = "keys",
    harmonia = "PRA_links"
  )
  
  allData <- sapply(fileNames, function(iFile) { 
      iFile <- paste0(iFile, ".csv")
      tryCatch({
          if (local)
            read.csv(system.file("extdata", iFile, package = "alienSpecies"),
              sep = if (type == "ui") ";" else ",", encoding = "UTF-8") else
            readS3(FUN = read.csv, sep = if (type == "ui") ";" else ",", encoding = "UTF-8", 
              file = iFile)
        }, error = function(err) NULL)
    }, simplify = FALSE)
  

  
  filterData <- switch(type, 
    ui = {
      
      allData <- allData[!sapply(allData, is.null)]
      
      # Fill out missing regions - nl always filled out
      if ("translations_regions" %in% names(allData)) {
        missingFr <- is.na(allData$translations_regions$title_fr)
        missingEn <- is.na(allData$translations_regions$title_en)
        allData$translations_regions$title_fr[missingFr] <- allData$translations_regions$title_nl[missingFr]
        allData$translations_regions$title_en[missingEn] <- allData$translations_regions$title_nl[missingEn]
      }
      
      # Merge all sources
      allData <- Reduce(function(x, y) merge(x, y, all = TRUE), allData)
      # Filter language
      uiText <- allData[, c("title_id", paste0(c("title_", "description_"), language))]
      colnames(uiText) <- c("id", "title", "description")
      uiText <- uiText[!uiText$id %in% c(NA, ""), ]
      uiText[is.na(uiText)] <- ""
      
      if (any(duplicated(uiText$id)))
        stop("Following translations occur multiple times, please clean the file: ",
          paste(uiText$id[duplicated(uiText$id)], collapse = ", "))
      
      uiText
      
    },
    keys = allData$keys,
    harmonia = {
      
      selectedColumns <- c("gbif_taxonkey", "harmonia_url", "url_type")
      
      # TODO remove when fixing https://github.com/inbo/aspbo/issues/423
      ## begin temp fix
      if (!"url_type" %in% colnames(allData$harmonia))
        allData$harmonia$url_type <- "harmonia"
      ## end temp fix
      
      allData$harmonia[, selectedColumns]
      
    }
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

#' List region names as can be recognized by the translation file
#' 
#' Will not return NA, but rather the original name in case
#' no match could be found.
#' 
#' @param x character, what to transform
#' @return named character vector, names are the original values
#' 
#' @author eadriaensen
#' @export
#' 
getRegionNames <- function(x) {
  
  new <- c(
      "Belgi\u00EB" 	    = "Belgium",
      "brussels" 	= "Brussels-Capital Region",
      "flanders" 	= "Flemish Region",
      "wallonia"	= "Walloon Region"
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
  
  
  # Composite translations e.g. habitats
  compositeIds <- grepl("|", id, fixed = TRUE)
  if (any(compositeIds)) {
    
    newIds <- unique(id[compositeIds])
    data <- rbind(data,
      data.frame(id = newIds, t(as.data.frame(sapply(newIds, function(x)
                apply(data[match(strsplit(x, split = "\\|")[[1]], data$id), c("title", "description")], 2, paste, collapse = "|")))))
    )
    
  } 
  
  # Helpfull during development to see which are missing
  # can be turned of in production
  if (!is.null(data) & !all(id %in% data$id)) {
    if (!all(is.na(id[!id %in% data$id])))
      message("Not in translation file: ", vectorToTitleString(id[!id %in% data$id]))
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
