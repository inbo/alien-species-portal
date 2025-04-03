library(alienSpecies)
library(shiny)



### General
### ------------

`%<>%` <- magrittr::`%<>%`

# overwrite config::get as default
get <- base::get

if (!exists("doDebug"))
  doDebug <- FALSE

chromote::set_chrome_args(c('--headless','--no-sandbox'))


### Data
### -----------

tabChoices <- c("start", "checklist_indicators", "species_information", 
  "early_warning", "mica_db", "radius_db", "management")[1:6]

if (!doDebug | !exists("exotenData"))
  exotenData <- loadTabularData(type = "indicators")
if (!doDebug | !exists("unionlistData"))
  unionlistData <- loadTabularData(type = "unionlist")
if (!doDebug | !exists("occurrenceData"))
  occurrenceData <- loadTabularData(type = "occurrence")
if (!doDebug | !exists("taxaChoices"))
  taxaChoices <- loadTabularData(type = "taxachoices")

# Load occupancy data from createOccupancyCube() - also loads `dfCube`
if (!doDebug | !exists("occupancy"))
  occupancy <- loadOccupancyData()


# Specify default year to show (and default max to show in time ranges)
defaultYear <- max(exotenData$first_observed, na.rm = TRUE)
defaultTimeNA <- TRUE
defaultTime <- c(min(exotenData$first_observed, na.rm = TRUE), defaultYear)


# Load cube data
if (!doDebug | !exists("allShapes"))
  allShapes <- c(
    # Grid data
    #readShapeData(),
    loadShapeData("grid.RData"),
    ## be_1km and be_10km data have neither is nor GEWEST attribute to indicate region.
    #loadShapeData("occurrenceCube.RData"),
    # gemeentes & provinces
    "provinces" = list(loadShapeData("provinces.RData")),
    "communes" = list(loadShapeData("communes.RData"))
    #readShapeData(extension = ".geojson")
  )

dictionary <- loadMetaData(type = "keys")

# Warning for missing info in keys.csv
if (doDebug) {
  # occurrence
  missingSpecies <- occurrenceData[!duplicated(taxonKey) & is.na(scientificName), "taxonKey"]
  if (nrow(missingSpecies))
    warning(paste("Scientific name is missing for", nrow(missingSpecies), "species in occurrence data."))
  # occupancy
  missingSpecies <- unique(dfCube$species[!dfCube$species %in% dictionary$scientificName])
  missingSpecies <- missingSpecies[!is.na(missingSpecies)]
  if (length(missingSpecies))
    warning(paste("Taxonkey is not available (in keys.csv) for", length(missingSpecies), "species in occupancy data."))
}



# Initial exoten filter choices
# e.g. search for Stylommatophora
taxaLevels <- c("kingdom", "phylum", "class", "order", "family", "species")
habitatChoices <- attr(exotenData, "habitats")
doeChoices <- sort(unique(exotenData$degree_of_establishment))
regionChoices <- sort(unique(exotenData$locality))
bronChoices <- sort(levels(exotenData$source))


# Available species for risk maps (Species > More > Risk maps)
request <- httr::GET("https://api.github.com/repos/trias-project/risk-maps/contents/public/geotiffs")
keysRiskMap <- unique(sapply(httr::content(request), function(x) 
      strsplit(gsub("public/geotiffs/be_", "", x$path), split = "_")[[1]][1]))

# Available species for links (Species > More > Links)
request <- httr::GET("https://api.github.com/repos/inbo/aspbo/contents/HTML_pages/HTML")
keysLinks <- unique(sapply(httr::content(request), function(x) 
      strsplit(gsub("HTML_pages/HTML/", "", x$path), split = "_")[[1]][1]))

# Available species for Harmonia+ DB (Species > More > Risk assessment)
harmoniaData <- loadMetaData(type = "harmonia")
