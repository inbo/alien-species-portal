#' Read shape data
#' @param dataDir path to the shape files. Folder should contain .shp file but also
#' helper files such as .dbf and .prj
#' @return list with sf objects
#' 
#' @author mvarewyck
#' @export
readShapeData <- function(
  dataDir = system.file("extdata", "grid", package = "alienSpecies")
) {
  
  
  shapeFiles <- list.files(dataDir, pattern = ".shp")
  
  toReturn <- lapply(shapeFiles, function(iFile) {
      
      sf::st_read(file.path(dataDir, iFile), layer = gsub(".shp", "", iFile))
      
    })
  
  names(toReturn) <- gsub(".shp", "", shapeFiles)
  
  toReturn
  
}



#' Create dictionary for combining data keys from multiple data sources
#' @inheritParams loadTabularData
#' @return no return value, data is written to \code{file.path(dataDir, "keys.csv")}
#' 
#' @author mvarewyck
#' @importFrom utils write.csv
#' @importFrom data.table fread setnames as.data.table
#' @export
createKeyData <- function(dataDir = system.file("extdata", package = "alienSpecies")) {
  
  # For R CMD check
  classKey <- NULL
  i.classKey <- NULL
    
  # Occurrence cube
  taxaData <- fread(file.path(dataDir, "be_alientaxa_info.csv"))
  classKeys <- as.data.table(do.call(rbind, lapply(unique(taxaData$taxonKey), function(k) {
      tmpData <- rgbif::name_usage(key = k)$data
      if (!"classKey" %in% colnames(tmpData))
        tmpData$classKey <- NA
      tmpData[, c("key", "classKey")]
    })))
  setnames(classKeys, "key", "taxonKey")
  taxaData <- taxaData[classKeys, classKey := i.classKey, on = "taxonKey"]
  
  ## clean scientific name
  taxaData$scientificName <- sapply(strsplit(taxaData$scientificName, split = " "), function(x) paste(x[1:2], collapse = " "))
  
  # Checklist
  exotenData <- loadTabularData(dataDir = dataDir, type = "indicators")[, c('key', 'species')]
  exotenData <- exotenData[!duplicated(exotenData), ]
  dictionary <- merge(taxaData, exotenData,
    by.x = "scientificName", by.y = "species", all = TRUE)
  setnames(dictionary, "key", "gbifKey")
    
  
  write.csv(dictionary, file = file.path(dataDir, "keys.csv"),
    row.names = FALSE)  
  
}



#' Load tabular data
#' 
#' For indicator data:
#' by default data for which \code{first_observed < 1950} is excluded.
#' For unionlist data:
#' only 'scientific name', 'english name' and 'kingdom' are retained
#' @param dataDir character vector, defines the path to the data file(s)
#' @param type data type, one of:
#' \itemize{
#' \item{\code{"indicators"}:}{for indicator data, i.e. main data set}
#' \item{\code{"unionlist"}:}{for union list data, i.e. }
#' }
#' @return data.table, loaded indicator/unionlist data; 
#' and attribute 'Date', the date that this data file was created
#' @importFrom data.table fread :=
#' @export
loadTabularData <- function(
  dataDir = system.file("extdata", package = "alienSpecies"),
  type = c("indicators", "unionlist", "occurrence", "protectedAreas")) {
  
  # For R CMD check
  scientificName <- NULL
  i.scientificName <- NULL
  
  type <- match.arg(type)
  
  dataFiles <- file.path(dataDir, switch(type,
      indicators = "data_input_checklist_indicators.tsv",
#      "indicators" = c("description.txt", "distribution.txt", "speciesprofile.txt", "taxon.txt"),
      unionlist = "eu_concern_species.tsv",
      occurrence = "be_alientaxa_cube.csv",
      protectedAreas = "intersect_EEA_ref_grid_protected_areas.tsv"))
  
  if (type == "indicators") {
    
    # recode missing values to NA
    rawData <- fread(dataFiles, stringsAsFactors = FALSE, na.strings = "")
    
    # Warning if new habitat columns
    currentHabitats <- c("marine", "freshwater", "terrestrial")
    if (!all(unlist(strsplit(rawData$habitat, split = "|", fixed = TRUE)) %in% c(NA, currentHabitats)))    
      stop("New habitats detected. Add relevant columns in loadTabularData()")
    
    ## extract necessary columns
    rawData <- rawData[, c(
        # GBIF key - necessary to use trias function
        "key", 
        # Taxon key
        "nubKey",
        # full scientific name
        "scientificName",
        # Period - slider should use first_observed
        "first_observed", "last_observed", 
        # Taxonomy
        "kingdom", "phylum", "class", "order", "family",
        # Locality
        "locality", "locationId", "native_range",
        # Degree establishment
        "degree_of_establishment",
        # Pathway
        "pathway_level1", "pathway_level2",
        # Habitat
        "habitat", ## easier to use the 3 booleans below instead
        ..currentHabitats,
        # Source
        "source",
        # union list filtering
        "species", "canonicalName"
      )]
    
    ## exclude data before 1950 - keeps values with NA for first_observed
    toExclude <- (rawData$first_observed < 1950 & !is.na(rawData$first_observed))
    warning(type, " data: ", sum(toExclude), " observaties dateren van voor 1950 en zijn dus uitgesloten")
    rawData <- rawData[!toExclude, ]
    
    ## convert english to dutch names for region
    rawData$locality <- getDutchNames(rawData$locality, type = "regio")
    
    ## combine pathways
    rawData$pathway <- paste0(rawData$pathway_level1, ": ", rawData$pathway_level2)
    
    ## Extract hyperlinks 
    
    ### Extract gbif link
    rawData$gbifLink <- sapply(strsplit(rawData$source, split = ": "), function(x) x[1])
    rawData$gbifLink <- paste0("<a href='", rawData$gbifLink, "' target = '_blank'>", 
      sapply(strsplit(rawData$gbifLink, split = "/"), function(x) tail(x, n = 1)), "</a>")
    # common name and source: https://www.gbif.org/species/157131005
    
    ## recode `source` variable
    
    ## Remove everything up until (jjjj)<space(s)>.<space(s)>
    rawData$source <- sub("(.*?)\\(\\d{4}\\)\\s*\\.\\s*", "", rawData$source)
    ## Remove everything after the first dot
    rawData$source <- sub("\\..*$", "", rawData$source)
    
    ## re-define source into shorter names
    rawData[, source := as.factor(source)]
    levels(rawData$source) <- list(
      "Ad hoc alien species checklist" = "Ad hoc checklist of alien species in Belgium",
      "Alien Bird Checklist" = "Checklist of alien birds of Belgium",
      "Alien Fish Flanders" = "Checklist of non-native freshwater fishes in Flanders, Belgium",
      "Alien Macroinverts" = "Inventory of alien macroinvertebrates in Flanders, Belgium",
      "Alien Mollusc checklist" = "Registry of introduced terrestrial molluscs in Belgium", 
      "Belgian rust fungi" = "Catalogue of the Rust Fungi of Belgium",
      "Manual of Alien Plants" = "Manual of the Alien Plants of Belgium",
      "RINSE1" = "RINSE - Registry of non-native species in the Two Seas region countries (Great Britain, France, Belgium and the Netherlands)",
      "RINSE2" = "RINSE - Pathways and vectors of biological invasions in Northwest Europe",
      "WRiMS" = "World Register of Introduced Marine Species (WRiMS)")
    
    ### source link
    # TODO add more links
    rawData$sourceLink <- ifelse(is.na(rawData$source), "", 
      paste0("<a href='", sapply(as.character(rawData$source), function(iSource) switch(iSource,
              WRiMS = "https://www.marinespecies.org/")), "' target = '_blank'>", rawData$source, "</a>"))
    
    ## regroup native_range variable into new native_continent variable
    ## from https://en.wikipedia.org/wiki/United_Nations_geoscheme
    
    africa <- c("Africa", "Northern Africa", "Sub-Saharan Africa", "Subsaharan Africa", "Eastern Africa", "Middle Africa", "Southern Africa", "Western Africa")
    americas <- c("Americas", "Latin America and the Caribbean", "Caribbean", "Central America", "South America", "Northern America")
    asia <- c("Asia", "Central Asia", "Eastern Asia", "South-eastern Asia", "Southeastern Asia", "Southern Asia", "Western Asia")
    europe <- c("Europe", "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe")
    oceania <- c("Oceania", "Australia and New Zealand", "Melanesia", "Micronesia", "Polynesia")
    
    rawData$native_continent[rawData$native_range %in% c(africa, tolower(africa))] <- "Africa"
    rawData$native_continent[rawData$native_range %in% c(americas, tolower(americas))] <- "Americas"
    rawData$native_continent[rawData$native_range %in% c(asia, tolower(asia))] <- "Asia"
    rawData$native_continent[rawData$native_range %in% c(europe, tolower(europe))] <- "Europe"
    rawData$native_continent[rawData$native_range %in% c(oceania, tolower(oceania))] <- "Oceania"
    
    # group any undefined regions in "undefined"
    rawData$native_continent[!(rawData$native_range %in% c(africa, tolower(africa),
            americas, tolower(americas),
            asia, tolower(asia),
            europe, tolower(europe),
            oceania, tolower(oceania))) & 
        !is.na(rawData$native_range)] <- "undefined"
    
    ## replace missing "species" with "canonicalName" if available
    # then drop "canonicalName"
    ind <- which(is.na(rawData$species) & !is.na(rawData$canonicalName))
    rawData$species[ind] <- rawData$canonicalName[ind]
    rawData$canonicalName <- NULL
    warning(type, " data: Voor ", length(ind), " observaties is de 'species' onbekend. 'canonicalName' wordt gebruikt in de plaats.")
    
    
    attr(rawData, "habitats") <- currentHabitats
    
    
  } else if (type == "unionlist") {
    
    rawData <- fread(dataFiles, stringsAsFactors = FALSE, na.strings = "")
    
    ## extract necessary columns
    rawData <- rawData[, c("checklist_scientificName", "english_name", "checklist_kingdom")]
    names(rawData) <- c("scientificName", "englishName", "kingdom")
    
  } else if (type == "occurrence") {
    
    rawData <- fread(dataFiles, stringsAsFactors = FALSE, na.strings = "",
      drop = "min_coord_uncertainty")
    
    ## exclude data before 1950 - keeps values with NA for first_observed
    toExclude <- (rawData$year < 1950 & !is.na(rawData$year))
    warning(type, " data: ", sum(toExclude), " observaties dateren van voor 1950 en zijn dus uitgesloten")
    rawData <- rawData[!toExclude, ]
    
    # for linking scientific name & classKey
    dictionary <- loadMetaData(type = "keys")
    rawData <- rawData[dictionary, c("scientificName", "classKey") := list(
        i.scientificName,
        i.classKey), on = "taxonKey"]
        
    # Class cube
    classData <- fread(file.path(dataDir, "be_classes_cube.csv"), 
      drop = c("min_coord_uncertainty", "eea_cell_code"))
    setnames(classData, "n", "class_n")
    classData <- unique(classData, by = c("year", "classKey"))
    rawData <- rawData[classData, class_n := i.class_n, on = c("classKey", "year")]
    
    # attach 10km cellcode
    rawData$cell_code10 <- gsub(pattern = "1km", replacement = "10km", 
      paste0(substr(rawData$eea_cell_code, start = 1, stop = 7),
        substr(rawData$eea_cell_code, start = 9, stop = 12)))
    colnames(rawData)[colnames(rawData) == "eea_cell_code"] <- "cell_code1"
    
  } else if (type == "protectedAreas") {
    
    rawData <- fread(dataFiles, stringsAsFactors = FALSE, na.strings = "")
    
    ## extract necessary columns
    rawData <- rawData[, c("CELLCODE", "EOFORIGIN", "NOFORIGIN", "natura2000")]
    # rows of interest: https://trias-project.github.io/indicators/05_occurrence_indicators_preprocessing.html#1_setup
    rawData <- rawData[rawData$natura2000, ]
    
  }
  
  attr(rawData, "Date") <- file.mtime(dataFiles)
  
  return(rawData)
  
}



#' Load meta data for the UI
#' @inheritParams loadTabularData 
#' @param type character, which type of translations should be loaded;
#' should be one of \code{c("species", "ui")}
#' @param language character, which language data sheet should be loaded;
#' should be one of \code{c("nl", "fr", "en")}
#' @return data.frame
#' 
#' @author mvarewyck
#' @importFrom utils read.csv
#' @export
loadMetaData <- function(type = c("ui", "keys"),
  dataDir = system.file("extdata", package = "alienSpecies"), 
  language = c("nl", "fr", "en")) {
  
  type <- match.arg(type)
  language <- match.arg(language)
  
  allData <- read.csv(file.path(dataDir, switch(type, 
        ui = "translations.csv",
        keys = "keys.csv"
      ))) 
  
  filterData <- switch(type, 
    ui = {
      
      uiText <- allData[, c("plotFunction", paste0(c("title_", "description_"), language))]
      colnames(uiText) <- c("plotFunction", "title", "description")
      uiText
      
    },
    keys = allData
  )
  
  return(filterData)
  
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



##' Extract the vernacular names using API requests
##' 
##' WIP: Currently all missing for this dataset https://www.gbif.org/dataset/6d9e952f-948c-4483-9807-575348147c7e
##' WARNING: Takes long time to process
##' @inheritParams loadTabularData
##' @param taxonKeys numeric vector, taxon keys for GBIF
##' @return no return value, data file 'vernacular_names.csv' is written to \code{dataDir}
##' data.frame with
##' \itemize{
##' \item{key}{\code{taxonKeys} entered as input}
##' \item{name}{character, vernacular name (language); multiple names are pasted togeter}
##' }
##' @author mvarewyck
##' @importFrom utils write.csv
##' @importFrom httr GET content
##' @export
##' @examples
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
