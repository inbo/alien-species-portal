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

# Translations
translations <- read.csv(file.path(dataDir, "translations.csv"))


### Debugging
### -----------

if (!exists("doDebug"))
  doDebug <- FALSE
