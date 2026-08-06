library(alienSpecies)
library(shiny)
library(shiny.i18n)
library(trias)

library(data.table)



### General
### ------------

`%<>%` <- magrittr::`%<>%`

# overwrite config::get as default
get <- base::get

if (!exists("doDebug"))
  doDebug <- FALSE

chromote::set_chrome_args(c('--headless','--no-sandbox'))
#options(
#  chromote.chrome_args = c(
#    "--headless=new",
#    "--no-sandbox",
#    "--disable-dev-shm-usage"
#  ),
#  chromote.timeout = 60
#)

addResourcePath("www", system.file("app", "www", package = "alienSpecies"))

### Translations
### -----------
translation_dir <- download_translations()
i18n <- Translator$new(translation_csvs_path = translation_dir)
i18n$set_translation_language("id")

### Data
### -----------

tabChoices <- c("start", "checklist_indicators", "species_information", 
  "other_db", "about", "faq", "management")[1:6]

if (!doDebug | !exists("exotenData"))
  exotenData <- loadTabularData(type = "indicators")
if (!doDebug | !exists("unionlistData"))
  unionlistData <- loadTabularData(type = "unionlist")
if (!doDebug | !exists("occurrenceData"))
  occurrenceData <- loadTabularData(type = "occurrence")
if (!doDebug | !exists("taxaChoices"))
  taxaChoices <- loadTabularData(type = "taxachoices")

# Cap taxa search at family level, exclude species-level choices (issue #205)
taxaChoices <- taxaChoices[lengths(strsplit(taxaChoices$long, " > ", fixed = TRUE)) < 6, ]

# Load occupancy data from createOccupancyCube()
if (!doDebug | !exists("occupancy"))
  occupancy <- loadOccupancyData()

# TODO fetch correct file from bucket
dfCube <- read.csv(
    system.file("extdata", "trendOccupancy_belgium.csv", package = "alienSpecies"), 
    sep = ",", encoding = "UTF-8"
  )


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

# Attach Natura 2000 status (per 1x1 km cell) to occurrence data, for the
# Occupancy tab's Region filter - status is based on the 1km cell an
# observation falls in, not on whether the containing 10km cell overlaps
# a Natura 2000 area
natura2000Lookup <- as.data.table(sf::st_drop_geometry(allShapes$utm1_bel_with_regions)[, c("CELLCODE", "isNatura2000")])
setnames(natura2000Lookup, "CELLCODE", "cell_code1")
occurrenceData[natura2000Lookup, on = "cell_code1", isNatura2000 := i.isNatura2000]

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
pwLevel1Choices <- sort(unique(exotenData$pathway_level1))
habitatChoices <- attr(exotenData, "habitats")
doeChoices <- sort(unique(exotenData$degree_of_establishment))
nativeChoices <- sort(unique(exotenData$native_continent))
regionChoices <- sort(unique(exotenData$locality))
bronChoices <- sort(levels(exotenData$source))


# Available species for risk maps (Species information > Risk maps)
# New source (issue #207): inbo/wisdm-maps-iasportal, one subfolder named after
# the taxonKey per species under data/ - TODO: branch "uat" is hardcoded here,
# same as the "main" branch was hardcoded for the old source; revisit once this
# is promoted to production (may need a config.yml-style uat/production split)
request <- httr::GET("https://api.github.com/repos/inbo/wisdm-maps-iasportal/contents/data?ref=uat")
keysRiskMap <- unique(sapply(httr::content(request), function(x) x$name))

# Available species for links (Species > More > Links)
request <- httr::GET("https://api.github.com/repos/inbo/aspbo/contents/HTML_pages/HTML")
keysLinks <- unique(sapply(httr::content(request), function(x) 
      strsplit(gsub("HTML_pages/HTML/", "", x$path), split = "_")[[1]][1]))

# Available species for Harmonia+ DB (Species > More > Risk assessment)
harmoniaData <- loadMetaData(type = "harmonia")
