# Test plots for occurrence cubes
# 
# Author: mvarewyck
###############################################################################


## Load data
allShapes <- readShapeData()
taxData <- loadTabularData(type = "occurrence")
## Settings
# many versus few occurrences
allSpecies <- c("Alopochen aegyptiaca", "Ruscus aculeatus")
period <- c(2000, 2018)

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
    
    # Leaflet plot
    occurrenceShape <- createCubeData(df = occurrenceData, shapeData = allShapes,
      groupVariable = "cell_code")
    expect_equal(length(occurrenceShape), 2)
    expect_is(occurrenceShape[[1]], "sf")
    expect_lte(nrow(occurrenceShape[[1]]), length(unique(taxData$cell_code1[taxData$taxonKey == myKey])))
    expect_lte(nrow(occurrenceShape[[2]]), length(unique(taxData$cell_code10[taxData$taxonKey == myKey])))
    
    myPlot <- mapCube(cubeShape = occurrenceShape, legend = "topright", 
      addGlobe = TRUE, groupVariable = "cell_code")
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
    
    # Taxa without occurrences
    indicatorData <- loadTabularData(type = "indicators")
    data_without_occs <- indicatorData[!indicatorData$nubKey %in% taxData$taxonKey, ]
    head(data_without_occs[order(data_without_occs$nubKey), ])
    
    # Protected areas
    areasData <- loadTabularData(type = "protectedAreas")
    
    myKey <- unique(taxData$taxonKey[taxData$scientificName == allSpecies[2]])
        
    # Filter on year and taxonKey
    occurrenceData <- taxData[taxonKey %in% myKey, ]
    # Sum over cell_code
    data.table::setkey(occurrenceData, year)
    occurrenceData <- unique(occurrenceData[, total := .(sum(n)), by = year], by = 'year')
    
    trias::apply_gam(df = occurrenceData, y_var = "total", 
      eval_years = 2018,
      taxon_key = myKey, name = allSpecies[2], verbose = TRUE)
    
    # observer bias: attach n per class when loading data
    trias::apply_gam(df = occurrenceData, y_var = "total", 
      baseline_var = "class_n",
      eval_years = 2018,
      taxon_key = myKey, name = allSpecies[2], verbose = TRUE)
    
    # protected areas
    # https://trias-project.github.io/indicators/05_occurrence_indicators_preprocessing.html#34_add_information_about_presence_in_protected_areas
#    df_prot_areas <- read_tsv(
#      here::here(
#        "data",
#        "interim",
#        "intersect_EEA_ref_grid_protected_areas.tsv"
#      ),
#      na = ""
#    )
    
    
  })


test_that("Reporting t1", {
    
    createOccupancyCube()
    load(file = file.path(dataDir, "dfCube.RData"))
    
    # Filter on taxonKey and source
    reportingData <- dfCube[species %in% allSpecies[1] & source == "t1", ]
    
    reportingShape <- createCubeData(df = reportingData, shapeData = allShapes,
      groupVariable = "source")
    expect_equal(length(reportingShape), 1)
    expect_is(reportingShape[[1]], "sf")
    expect_lte(nrow(reportingShape[[1]]), length(unique(reportingData$cell_code10)))
    
    myPlot <- mapCube(cubeShape = reportingShape, legend = "topright", addGlobe = TRUE,
      groupVariable = "source")
    expect_is(myPlot, "leaflet")
    
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
    
    myPlot <- mapCube(cubeShape = reportingShape, legend = "topright", addGlobe = TRUE,
      groupVariable = "source")
    expect_is(myPlot, "leaflet")
    
  })  

