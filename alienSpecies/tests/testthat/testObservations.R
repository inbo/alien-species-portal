# Test plots for occurrence cubes
# 
# Author: mvarewyck
###############################################################################


## Load data
allShapes <- readShapeData()
taxData <- loadTabularData(type = "occurrence")
## Settings
# many versus few occurrences
mySpecies <- c("Alopochen aegyptiaca", "Ruscus aculeatus")[2]
period <- c(2000, 2018)

test_that("Occurrence grid shape", {
    
    expect_equal(length(allShapes), 2)
    
    expect_is(allShapes, "list")
    expect_is(allShapes$be_10km, "data.frame")
    
  })

test_that("Occurrence plots", {
    
    myKey <- unique(taxData$taxonKey[taxData$scientificName == mySpecies])
    expect_is(myKey, "integer")
    
    expect_is(taxData, "data.frame")
    
    # Filter on year and taxonKey
    occurrenceData <- taxData[taxonKey %in% myKey & year >= period[1] & year <= period[2], ]
    
    # Leaflet plot
    occurrenceShape <- createOccurrenceData(occurrenceData = occurrenceData, shapeData = allShapes)
    expect_equal(length(occurrenceShape), 2)
    expect_is(occurrenceShape[[1]], "sf")
    expect_lte(nrow(occurrenceShape[[1]]), length(unique(taxData$cell_code1[taxData$taxonKey == myKey])))
    expect_lte(nrow(occurrenceShape[[2]]), length(unique(taxData$cell_code10[taxData$taxonKey == myKey])))
    
    myPlot <- mapOccurrence(occurrenceShape = occurrenceShape, legend = "topright", addGlobe = TRUE)
    expect_is(myPlot, "leaflet")
    
    # Barplot
    occurrenceData <- taxData[taxData$taxonKey %in% myKey, ]
    myResult <- countOccurrence(df = occurrenceData, period = c(2012, 2018))
    
    expect_is(myResult$plot, "plotly")
    expect_is(myResult$data, "data.frame")
    
    # Color bars when full range selected
    myResult <- countOccurrence(df = occurrenceData, period = c(1950, 2021))$plot
    
  })
