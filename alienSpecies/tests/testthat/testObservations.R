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



test_that("Emergence status GAM - Observations", {
    
    # Taxa without occurrences
    indicatorData <- loadTabularData(type = "indicators")
    data_without_occs <- indicatorData[!indicatorData$nubKey %in% taxData$taxonKey, ]
    head(data_without_occs[order(data_without_occs$nubKey), ])
    
    # Protected areas
    areasData <- loadTabularData(type = "protectedAreas")
    
    myKey <- unique(taxData$taxonKey[taxData$scientificName == mySpecies])
        
    # Filter on year and taxonKey
    occurrenceData <- taxData[taxonKey %in% myKey, ]
    # Sum over cell_code
    data.table::setkey(occurrenceData, year)
    occurrenceData <- unique(occurrenceData[, total := .(sum(n)), by = year], by = 'year')
    
    trias::apply_gam(df = occurrenceData, y_var = "total", 
      eval_years = 2018,
      taxon_key = myKey, name = mySpecies, verbose = TRUE)
    
    # observer bias: attach n per class when loading data
    trias::apply_gam(df = occurrenceData, y_var = "total", 
      baseline_var = "class_n",
      eval_years = 2018,
      taxon_key = myKey, name = mySpecies, verbose = TRUE)
    
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
