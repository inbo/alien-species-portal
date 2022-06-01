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


exotenData <- loadExotenData(type = "indicators")
unionlistData <- loadExotenData(type = "unionlist")

# Specify default year to show (and default max to show in time ranges)
defaultYear <- max(exotenData$first_observed, na.rm = TRUE)

habitats <- attr(exotenData, "habitats")

# Load occupancy data from createOccupancyData()
load(file = file.path(dataDir, "occupancy.RData"))

# Load occurrence data
if (!doDebug | !exists("allShapes"))
  allShapes <- readShapeData()
taxData <- loadExotenData(type = "occurrence")
dictionary <- loadTranslations(type = "species")


