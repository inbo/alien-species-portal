# Test plots for occurrence cubes
# 
# Author: mvarewyck
###############################################################################


## Load data
allShapes <- allShapes <- c(
  # Grid data
  #readShapeData(),
  loadShapeData("grid.RData"),
  ## be_1km and be_10km data have neither is nor GEWEST attribute to indicate region.
  #loadShapeData("occurrenceCube.RData"),
  # gemeentes & provinces
  "provinces" = list(loadShapeData("provinces.RData")),
  "communes" = list(loadShapeData("communes.RData"))
#readShapeData(extension = ".geojson")
)

taxData <- loadTabularData(type = "occurrence")
## Settings
# many versus few occurrences
allSpecies <- c("Alopochen aegyptiaca", "Muntiacus reevesi")
period <- c(2000, 2018)

# Translations
translation_dir <- download_translations()
i18n <<- Translator$new(translation_csvs_path = translation_dir)


test_that("Check summary data", {
    
    dataFiles <- aws.s3::get_bucket_df(
      bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")))$Key
    
    expect_true("full_timeseries.RData" %in% dataFiles)
    expect_true("dfCube.RData" %in% dataFiles)
    
  })

test_that("All shape files", {
    
    expect_equal(length(allShapes), 5)
    
    expect_type(allShapes, "list")
    
    expect_setequal(
      c("gewestbel", "utm1_bel_with_regions", "utm10_bel_with_regions", "provinces", "communes"), 
      names(allShapes)
    ) 
    
  })

test_that("Occurrence plots", {
    
    myKey <- unique(taxData$taxonKey[taxData$scientificName %in% allSpecies[2]])
    expect_type(myKey, "integer")
    
    expect_gte(nrow(taxData), 1)
    
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
    myResult <- countOccurrence(df = df, period = c(2012, 2021), combine = FALSE)
    
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    # Color bars when full range selected
    countOccurrence(df = df, period = c(1950, 2021))$plot
        
  })
    
  
test_that("Map invasion", {
    
    # TODO temporary fix until data is created with alienSpecies >= v1.0.0
    if (TRUE) {
      
      readS3(
        file = "be_alientaxa_cube_processed.RData", 
        bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies"))
      )
      taxData <- rawData[, c("year", "cell_code1", "taxonKey", "n",
          "isFlanders", "isWallonia", "isBrussels", "gemeente", "provincie", "gewest",           
          "scientificName", "classKey", "cell_code10")]
      
      setnames(taxData, "gemeente", "NAAM")
      setnames(taxData, "gewest", "GEWEST")
      
    }
    
    myKey <- unique(taxData$taxonKey[taxData$scientificName %in% allSpecies[2]])
    currentYear <- 2023
    
    for (regionLevel in c("communes", "provinces", "cell_code1", "cell_code10")) {
      
      summaryData <- createSummaryRegions(
        data = taxData[taxonKey %in% myKey, ],
        shapeData = allShapes,
        regionLevel = regionLevel,
        year = list(
          c(currentYear-8, currentYear-5), 
          c(currentYear-4, currentYear-1),
          currentYear)
      )
      
      myPlot <- mapRegionsFacet(managementData = summaryData,
        shapeData = allShapes, regionLevel = regionLevel, addGlobe = TRUE)
      
      expect_s3_class(myPlot, "ggplot")
      # ggsave(filename = file.path(tempdir(), paste0("example_", regionLevel, ".png")), plot = myPlot)
      
    }
    
  })
  

## Note: fitting GAM model only works when loading the R-package using library(alienSpecies)
## When loading via devtools::load_all() there is a conflict with config::get()
## which can be resolved by
get <- base::get

test_that("Emergence status GAM - Observations", {
    
    ## Note: fitting GAM model only works when loading the R-package using library(alienSpecies)
    ## When loading via devtools::load_all() there is a conflict with config::get()
    ## which can be resolved by
    ## get <- base::get

    myKey <- unique(taxData$taxonKey[taxData$scientificName %in% allSpecies[2]])
    
    timeseries <- loadTabularData(type = "timeseries")
    
    correctBias <- c(TRUE, FALSE)[1]
    isProtected <- c(TRUE, FALSE)[2]
    
    subData <- summarizeTimeSeries(
      species = myKey,
      region = c("flanders", "brussels")
    )[protected == isProtected, ]
    
    # Gam model can be fitted
    tmpResult <- plotTrias(triasFunction = "apply_gam", 
      df = subData,
      triasArgs = list(
        y_var = "obs",
        taxon_key = myKey, 
        name = allSpecies[1],
        x_label = "Year",
        y_label = "Observations",
        eval_years = 2010 - c(3,1),
        type_indicator = "observations",
        baseline_var = if (correctBias) "cobs",
        region = "flanders"
        )
    )
 
    expect_type(tmpResult, "list")
    expect_s3_class(tmpResult$plot, "plotly")
    expect_s3_class(tmpResult$data, "data.frame")
    expect_true(!any(is.na(tmpResult$data$ucl)), label = "GAM can be assessed")
    
    # Gam model cannot be fitted
    tmpResult <- plotTrias(triasFunction = "apply_gam", 
      df = subData[subData$year %in% 2020:2022, ],
      triasArgs = list(
        y_var = "obs",
        taxon_key = myKey, 
        name = allSpecies[1],
        x_label = "Year",
        y_label = "Observations",
        eval_years = 2020,
        type_indicator = "observations",
        baseline_var = if (correctBias) "cobs",
        region = "flanders"
      )
    )
    
    expect_true(all(is.na(tmpResult$data$ucl)), label = "GAM cannot be assessed")
    
  })


test_that("Emergence status GAM - Occupancy", {
    
    myKey <- unique(taxData$taxonKey[taxData$scientificName %in% allSpecies[2]])
    
    subData <- summarizeTimeSeries(
      species = myKey,
      region = c("flanders", "brussels")
    )
    
    correctBias <- c(TRUE, FALSE)[2]
    isProtected <- c(TRUE, FALSE)[2]
    
    subData <- subData[protected == isProtected, ]
    
    tmpResult <- plotTrias(triasFunction = "apply_gam", 
      df = subData,
      triasArgs = list(
        y_var = "ncells",
        eval_years = min(subData$year):max(subData$year),
        taxon_key = myKey, name = allSpecies[2],
        baseline_var = if (correctBias) "c_ncells",
        region = "flanders",
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