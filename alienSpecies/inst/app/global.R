library(alienSpecies)
library(shiny)



### General
### ------------

`%<>%` <- magrittr::`%<>%`


if (!exists("doDebug"))
  doDebug <- FALSE


### Data
### -----------

tabChoices <- c("start", "global_indicators", "species_information", 
  "early_warning", "management")[1:4]
dataDir <- system.file("extdata", package = "alienSpecies")
managementDir <- system.file("extdata", "management", package = "alienSpecies")


# Create summary data
if (!file.exists(file.path(dataDir, "sum_timeseries.csv")))
  createTimeseries()
if (!file.exists(file.path(dataDir, "dfCube.RData")))
  createOccupancyCube()

if (!doDebug | !exists("exotenData"))
  exotenData <- loadTabularData(type = "indicators")
if (!doDebug | !exists("unionlistData"))
  unionlistData <- loadTabularData(type = "unionlist")
if (!doDebug | !exists("occurrenceData"))
  occurrenceData <- loadTabularData(type = "occurrence")
if (!doDebug | !exists("timeseries"))
  timeseries <- loadTabularData(type = "timeseries")

# Specify default year to show (and default max to show in time ranges)
defaultYear <- max(exotenData$first_observed, na.rm = TRUE)
defaultTimeNA <- TRUE
defaultTime <- c(min(exotenData$first_observed, na.rm = TRUE), defaultYear)


# Load occupancy data from createOccupancyCube()
load(file = file.path(dataDir, "dfCube.RData"))
occupancy <- createOccupancyData(dfCube = dfCube)

# Load cube data
if (!doDebug | !exists("allShapes"))
  allShapes <- c(
    # Grid data
    readShapeData(),
    # gemeentes & provinces
    readShapeData(dataDir = system.file("extdata", package = "alienSpecies"),
      extension = ".geojson")
  )
dictionary <- loadMetaData(type = "keys")


# Initial exoten filter choices
# e.g. search for Stylommatophora
taxaLevels <- c("kingdom", "phylum", "class", "order", "family", "species")
#taxaChoices <- createTaxaChoices(exotenData = exotenData)
#longTaxaChoices <- unlist(taxaChoices)  # for matching id and title in search query
taxaChoices <- createTaxaChoices2(exotenData = exotenData)

habitatChoices <- attr(exotenData, "habitats")
doeChoices <- sort(unique(exotenData$degree_of_establishment))
regionChoices <- sort(unique(exotenData$locality))
bronChoices <- sort(levels(exotenData$source))

