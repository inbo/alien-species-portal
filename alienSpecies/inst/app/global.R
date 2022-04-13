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


### Data
### -----------

dataDir <- system.file("extdata", package = "alienSpecies")

exotenData <- loadExotenData(type = "indicators")
unionlistData <- loadExotenData(type = "unionlist")

# Specify default year to show (and default max to show in time ranges)
defaultYear <- max(exotenData$first_observed, na.rm = TRUE)

habitats <- attr(exotenData, "habitats")


### Debugging
### -----------

if (!exists("doDebug"))
  doDebug <- FALSE
