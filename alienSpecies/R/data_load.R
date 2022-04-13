#' Read exoten data
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
loadExotenData <- function(
  dataDir = system.file("extdata", package = "alienSpecies"),
  type = c("indicators", "unionlist")) {
  
  type <- match.arg(type)
  
  dataFile <- file.path(dataDir, switch(type,
      "indicators" = "data_input_checklist_indicators.tsv",
      "unionlist" = "eu_concern_species.tsv"))
  
  if (type == "indicators") {
    
    # recode missing values to NA
    rawData <- fread(dataFile, stringsAsFactors = FALSE, na.strings = "")
    
    # Warning if new habitat columns
    currentHabitats <- c("marine", "freshwater", "terrestrial")
    if (!all(unlist(strsplit(rawData$habitat, split = "|", fixed = TRUE)) %in% c(NA, currentHabitats)))    
      stop("New habitats detected. Add relevant columns in loadExotenData()")
    
    ## extract necessary columns
    rawData <- rawData[, c(
        # necessary to use trias function
        "key",
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
        # "habitat", ## easier to use the 3 booleans below instead
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
    warning(type, " data: Voor ", length(ind), " observaties is de 'specie' onbekend. 'canonicalName' wordt gebruikt in de plaats.")
    
    
    
    attr(rawData, "Date") <- file.mtime(dataFile)
    attr(rawData, "habitats") <- currentHabitats
    
    
  } else if (type == "unionlist") {
    
    rawData <- fread(dataFile, stringsAsFactors = FALSE, na.strings = "")
    
    ## extract necessary columns
    rawData <- rawData[, c("checklist_scientificName", "english_name", "checklist_kingdom")]
    names(rawData) <- c("scientificName", "englishName", "kingdom")
    
    attr(rawData, "Date") <- file.mtime(dataFile)
    
    
  }
  
  return(rawData)
  
}



#' Load labels for the UI
#' @inheritParams loadExotenData 
#' @param language character, which language data sheet should be loaded;
#' should be one of \code{c("nl", "fr", "en")}
#' @return data.frame
#' 
#' @author mvarewyck
#' @importFrom openxlsx read.xlsx
#' @export
loadTranslations <- function(
  dataDir = system.file("extdata", package = "alienSpecies"), 
  language = c("nl", "fr", "en")) {
  
  read.xlsx(file.path(dataDir, "translations.xlsx"), sheet = language)
  
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
