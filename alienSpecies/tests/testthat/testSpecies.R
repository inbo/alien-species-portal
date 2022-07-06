# Test plots for occurrence cubes
# 
# Author: mvarewyck
###############################################################################


## Load data
allShapes <- readShapeData()
taxData <- loadTabularData(type = "occurrence")
baseMap <- createBaseMap()
## Settings
# many versus few occurrences
allSpecies <- c("Alopochen aegyptiaca", "Muntiacus reevesi")
period <- c(2000, 2018)



test_that("Create summary data", {
    
    if (!file.exists(file.path(dataDir, "sum_timeseries.csv")))
      createTimeseries()
    if (!file.exists(file.path(dataDir, "dfCube.RData")))
      createOccupancyCube()
    
  })

test_that("Occurrence grid shape", {
    
    expect_equal(length(allShapes), 2)
    
    expect_is(allShapes, "list")
    expect_is(allShapes$be_10km, "data.frame")
    
  })

test_that("Occurrence plots", {
    
    myKey <- unique(taxData$taxonKey[taxData$scientificName == allSpecies[2]])
    expect_is(myKey, "integer")
    
    expect_is(taxData, "data.frame")
    
    # Filter on year and taxonKey
    occurrenceData <- taxData[taxonKey %in% myKey & year >= period[1] & year <= period[2], ]
    
    # Leaflet data
    occurrenceShape <- createCubeData(df = occurrenceData, shapeData = allShapes,
      groupVariable = "cell_code")
    expect_equal(length(occurrenceShape), 2)
    expect_is(occurrenceShape[[1]], "sf")
    expect_lte(nrow(occurrenceShape[[1]]), length(unique(taxData$cell_code1[taxData$taxonKey == myKey])))
    expect_lte(nrow(occurrenceShape[[2]]), length(unique(taxData$cell_code10[taxData$taxonKey == myKey])))
    # Data download
    myData <- do.call(rbind, reportingShape)
    myData$source <- attr(reportingShape, "splitFactor")
    myData$geometry <- NULL
    expect_is(myData, "data.frame")
    
    # Leaflet plot
    myPlot <- mapCube(cubeShape = occurrenceShape, baseMap = baseMap, 
      legend = "topright", addGlobe = TRUE, groupVariable = "cell_code")
    expect_is(myPlot, "leaflet")
    
    # Barplot
    occurrenceData <- taxData[taxData$taxonKey %in% myKey, ]
    myResult <- countOccurrence(df = occurrenceData, period = c(2012, 2018))
    
    expect_is(myResult$plot, "plotly")
    expect_is(myResult$data, "data.frame")
    
    # Color bars when full range selected
    myResult <- countOccurrence(df = occurrenceData, period = c(1950, 2021))$plot
        
  })



test_that("Emergence status GAM - Observations", {

    timeseries <- loadTabularData(type = "timeseries")
    
    myKey <- unique(taxData$taxonKey[taxData$scientificName == allSpecies[2]])
    
    correctBias <- c(TRUE, FALSE)[1]
    isProtected <- c(TRUE, FALSE)[2]
    
    subData <- timeseries[taxonKey %in% myKey, ]
    subData <- subData[protected == isProtected, ]
    
    tmpResult <- plotTrias(triasFunction = "apply_gam", 
      df = subData,
      triasArgs = list(
        y_var = "obs",
        eval_years = min(subData$year):max(subData$year),
        taxon_key = myKey, name = allSpecies[2],
        baseline_var = if (correctBias) "cobs",
        verbose = TRUE)
    )
 
    expect_is(tmpResult, "list")
    expect_is(tmpResult$plot, "plotly")
    expect_is(tmpResult$data, "data.frame")
    
  })


test_that("Reporting t1", {
    
    load(file = file.path(dataDir, "dfCube.RData"))
    
    # Filter on taxonKey and source
    reportingData <- dfCube[species %in% allSpecies[1] & source == "t1", ]
    
    reportingShape <- createCubeData(df = reportingData, shapeData = allShapes,
      groupVariable = "source")
    expect_equal(length(reportingShape), 2)
    expect_is(reportingShape[[1]], "sf")
    expect_lte(nrow(reportingShape[[1]]), length(unique(reportingData$cell_code10)))
    
    myPlot <- mapCube(cubeShape = reportingShape, baseMap = baseMap, 
      legend = "topright", addGlobe = TRUE, groupVariable = "source")
    expect_is(myPlot, "leaflet")
    
    # For data download in the app
    myData <- do.call(rbind, reportingShape)
    myData$source <- attr(reportingShape, "splitFactor")
    expect_is(myData, "data.frame")
    
  })  

  
test_that("Reporting t0 and t1", {
    
    load(file = file.path(dataDir, "dfCube.RData"))
    
    # Filter on taxonKey and source
    reportingData <- dfCube[species %in% allSpecies[1], ]
    
    reportingShape <- createCubeData(df = reportingData, shapeData = allShapes,
      groupVariable = "source")
    expect_equal(length(reportingShape), 4)
    expect_is(reportingShape[[1]], "sf")
    expect_lte(nrow(reportingShape[[length(reportingShape)]]), length(unique(reportingData$cell_code10)))
    
    myPlot <- mapCube(cubeShape = reportingShape, baseMap = baseMap, 
      legend = "topright", addGlobe = TRUE, groupVariable = "source")
    expect_is(myPlot, "leaflet")
    
    # For data download in the app
    myData <- unsplit(reportingShape, attr(reportingShape, "splitFactor"))
    myData$source <- attr(reportingShape, "splitFactor")
    expect_is(myData, "data.frame")
    
  })  