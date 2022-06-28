library(alienSpecies)
library(shiny)


### General
### ------------

`%then%` <- function(x, y) {
  
  if (is.null(x) || isTRUE(is.na(x)))
    y
  else
    x
  
}
`%<>%` <- magrittr::`%<>%`


if (!exists("doDebug"))
  doDebug <- FALSE


### Data
### -----------

dataDir <- system.file("extdata", package = "alienSpecies")

if (!doDebug | !exists("exotenData"))
  exotenData <- loadTabularData(type = "indicators")
if (!doDebug | !exists("unionlistData"))
  unionlistData <- loadTabularData(type = "unionlist")
if (!doDebug | !exists("occurrenceData"))
  occurrenceData <- loadTabularData(type = "occurrence")

# Specify default year to show (and default max to show in time ranges)
defaultYear <- max(exotenData$first_observed, na.rm = TRUE)

habitats <- attr(exotenData, "habitats")

# Load occupancy data from createOccupancyData()
load(file = file.path(dataDir, "dfCube.RData"))
occupancy <- createOccupancyData(dfCube = dfCube)

# Load occurrence data
if (!doDebug | !exists("allShapes"))
  allShapes <- readShapeData()
dictionary <- loadMetaData(type = "keys")


