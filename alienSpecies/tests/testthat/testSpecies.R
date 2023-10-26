# Test plots for occurrence cubes
# 
# Author: mvarewyck
###############################################################################


## Load data
allShapes <- loadShapeData("grid.RData")
taxData <- loadTabularData(type = "occurrence")
## Settings
# many versus few occurrences
allSpecies <- c("Alopochen aegyptiaca", "Muntiacus reevesi")
period <- c(2000, 2018)



test_that("Check summary data", {
    
    dataFiles <- aws.s3::get_bucket_df(
      bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")))$Key
    
    expect_true("full_timeseries.RData" %in% dataFiles)
    expect_true("dfCube.RData" %in% dataFiles)
    
  })

test_that("Occurrence grid shape", {
    
    expect_equal(length(allShapes), 7)
    
    expect_type(allShapes, "list")
    
    expect_setequal(
      c("gewestbel", "utm1_bel_with_regions", "utm10_bel_with_regions","be_10km", "be_1km","provinces","communes"  ) , names(allShapes)
      
    ) 
  })

test_that("Occurrence plots", {
    
    myKey <- unique(taxData$taxonKey[taxData$scientificName == allSpecies[2]])
    expect_type(myKey, "integer")
    
    expect_equal(dim(taxData), c(1085453, 7))
    
    # Filter on year and taxonKey
    occurrenceData <- taxData[taxonKey %in% myKey & year >= period[1] & year <= period[2], ]
    
    # Leaflet data
    occurrenceShape <- createCubeData(df = occurrenceData, shapeData = allShapes,
      region = "flanders", groupVariable = "cell_code")
    expect_equal(length(occurrenceShape), 2)
    expect_s3_class(occurrenceShape[[1]], "sf")
    expect_lte(nrow(occurrenceShape[[1]]), length(unique(taxData$cell_code1[taxData$taxonKey == myKey])))
    expect_lte(nrow(occurrenceShape[[2]]), length(unique(taxData$cell_code10[taxData$taxonKey == myKey])))
    # Data download
    myData <- do.call(rbind, occurrenceShape)
    myData$source <- attr(occurrenceShape, "splitFactor")
    myData$geometry <- NULL
    expect_s3_class(myData, "data.frame")
    
    # Leaflet plot
    myPlot <- mapCube(cubeShape = occurrenceShape, addGlobe = TRUE, 
      groupVariable = "cell_code")
    expect_s3_class(myPlot, "leaflet")
    
    # Change borders
    map2 <- addBaseMap(map = myPlot, regions = c("flanders", "wallonia"), combine = TRUE)
    map3 <- addBaseMap(map = map2, regions = c("flanders", "wallonia"), combine = FALSE)
    
    # Barplot
    # Add region as group variable
    df <- merge(taxData[taxData$taxonKey %in% myKey, ], 
      sf::st_drop_geometry(allShapes$utm1_bel_with_regions)[, c("CELLCODE", "isFlanders", "isBrussels")], 
      by.x = "cell_code1", by.y = "CELLCODE")
    myResult <- countOccurrence(df = df, period = c(2012, 2021), combine = FALSE, 
      uiText = loadMetaData(type = "ui"))
    
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    # Color bars when full range selected
    myResult <- countOccurrence(df = df, period = c(1950, 2021),
      uiText = loadMetaData(type = "ui"))$plot
        
  })



test_that("Emergence status GAM - Observations", {

    readS3(file = "full_timeseries.RData")
    
    timeseries <- summarizeTimeSeries(
      rawData = timeseries, 
      region = c("flanders", "brussels")
    )
    
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
 
    expect_type(tmpResult, "list")
    expect_s3_class(tmpResult$plot, "plotly")
    expect_s3_class(tmpResult$data, "data.frame")
    
  })


  
test_that("Reporting t0 and t1", {
    
    readS3(file = "dfCube.RData")
    
    # Filter on taxonKey and source
    reportingData <- dfCube[species %in% allSpecies[2], ]
    
#    # Filter on source
#    reportingData <- reportingData[reportingData$source == "t1", ]
    
    occurrenceShape <- createCubeData(df = reportingData, shapeData = allShapes,
      groupVariable = "source")
    expect_equal(length(occurrenceShape), 4)
    expect_s3_class(occurrenceShape[[1]], "sf")
    expect_equal(sum(sapply(occurrenceShape[1:3], nrow)), length(unique(reportingData$cell_code10)))
    
    myPlot <- mapCube(cubeShape = occurrenceShape,
      legend = "topright", addGlobe = TRUE, groupVariable = "source")
    expect_s3_class(myPlot, "leaflet")
    
    # For data download in the app
    myData <- do.call(rbind, occurrenceShape)
    myData$source <- attr(occurrenceShape, "splitFactor")
    expect_s3_class(myData, "data.frame")
    
  })  