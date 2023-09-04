# Test plots for the management page
# 
# Author: mvarewyck
###############################################################################



allShapes <- c(
  # Grid data
  readShapeData(),
  # gemeentes & provinces
  suppressWarnings(readShapeData(dataDir = system.file("extdata", package = "alienSpecies"),
      extension = ".geojson"))
)
uiText <- loadMetaData()



## Rosse stekelstaart ##
## Oxyura jamaicensis

outFile <- "Oxyura_jamaicensis.csv"


test_that("Create management data", {
    
    skip("Takes long time to run & will overwrite data")
    
    getGbifOccurrence(datasetKey = "7522721f-4d97-4984-8231-c9e061ef46df",
      outFile = outFile, user = "mvarewyck", pwd = "6P.G6DrErq.mmUy")
    
  })


managementData <- loadGbif(dataFile = outFile)

test_that("Map for Ruddy Duck", {
    
    # Filters on sampling
    filterValue <- unique(managementData$samplingProtocol)[1]
    managementData <- managementData[managementData$samplingProtocol == filterValue, ]
    # Filter on gender
    filterValue <- unique(managementData$gender)[1]
    managementData <- managementData[managementData$gender == filterValue, ]
    sum(managementData$count)
    
    myPlot <- mapOccurrence(occurrenceData = managementData, addGlobe = TRUE,
      baseMap = addBaseMap(regions = "flanders"))
    expect_s3_class(myPlot, "leaflet")
    
  })


test_that("Barplot for Ruddy Duck", {
    
    myPlot <- countOccurrence(df = managementData, uiText = uiText)
    expect_s3_class(myPlot$plot, "plotly")
    
    # Filter on sampling
    filterValue <- unique(managementData$samplingProtocol)[2]
    countOccurrence(df = managementData[managementData$samplingProtocol == filterValue, ],
      uiText = uiText)
    
  })



## Amerikaanse stierkikker ##
## Lithobates catesbeianus

outFile <- "Lithobates_catesbeianus.csv"

managementData <- loadGbif(dataFile = outFile)
# Add GEWEST
managementData$GEWEST <- allShapes$communes$GEWEST[
  match(managementData$NISCODE, allShapes$communes$NISCODE)]


test_that("Barplot for Bullfrogs", {
   
    countYearGroup(df = managementData)$plot
    countYearGroup(df = managementData, groupVar = "lifeStage")$plot
    
    myResult <- countYearGroup(df = managementData, summarizeBy = "cumsum")
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    countYearGroup(df = managementData, summarizeBy = "cumsum", groupVar = "lifeStage")
        
  })

test_that("Map & trend for Bullfrogs", {
    
    occurrenceData <- loadTabularData(type = "occurrence")
    # Filter on taxonKey and year
    occurrenceData <- occurrenceData[occurrenceData$scientificName == gsub("_", " ", gsub(".csv", "", outFile)) & year == 2018, ]
    
    # Map - gemeente
    summaryData <- createSummaryRegions(data = managementData, 
      shapeData = allShapes, regionLevel = "communes", 
      year = 2018, unit = "cpue")
    myPlot <- mapRegions(managementData = summaryData, occurrenceData = occurrenceData, 
      shapeData = allShapes, regionLevel = "communes")
    expect_s3_class(myPlot, "leaflet")
    
    # Map - provinces
    summaryData <- createSummaryRegions(data = managementData, 
      shapeData = allShapes, regionLevel = "provinces",
      year = 2018)
    myPlot <- mapRegions(managementData = summaryData, occurrenceData = occurrenceData, 
      shapeData = allShapes, regionLevel = "provinces")
    
    # Filter on gewest
    gewest <- "flanders"
    subShape <- lapply(allShapes, function(iData) {
        if ("GEWEST" %in% colnames(iData))
          iData[iData$GEWEST %in% gewest, ] else
          iData[apply(sf::st_drop_geometry(iData[, paste0("is", simpleCap(gewest)), drop = FALSE]), 1, sum) > 0, ]
      })
    mapRegions(managementData = summaryData, occurrenceData = occurrenceData, 
      shapeData = subShape, regionLevel = "communes")
    
  })


test_that("Trend for Bullfrogs", {
    
    # Municipalities
    summaryData <- createSummaryRegions(data = managementData, 
      shapeData = allShapes, regionLevel = "communes", 
      year = unique(managementData$year))
    
    myResult <- trendYearRegion(df = summaryData[summaryData$region %in% c("Arendonk", "Kasterlee"), ])
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    # Provinces
    summaryData <- createSummaryRegions(data = managementData, 
      shapeData = allShapes, regionLevel = "provinces",
      year = unique(managementData$year))
    
    trendYearRegion(df = summaryData[summaryData$region %in% c("Antwerpen", "Limburg"), ])
    
    trendYearRegion(df = summaryData[summaryData$region %in% c("Antwerpen", "Limburg"), ],
      combine = TRUE)
    
    # Flanders - per gewest
    summaryData <- createSummaryRegions(data = managementData, 
      shapeData = allShapes, regionLevel = "gewest")
    
    trendYearRegion(df = summaryData[summaryData$region %in% c("flanders", "wallonia"), ])
    
  })





## Aziatische hoornaar ##
## Vespa velutina

vespaData <- readShapeData(
  extension = ".geojson", 
  dataDir = system.file("extdata", "management", "Vespa_velutina", package = "alienSpecies")
)

test_that("Actieve haarden", {
    
    combinedData <- combineActiveData(
      activeData = vespaData$actieve_haarden,
      managedData = vespaData$beheerde_nesten,
      untreatedData = vespaData$onbehandelde_nesten
    )
    myPlot <- mapHeat(
      combinedData = combinedData,
      colors = {
        myColors <- c("blue", "black", "red")
        names(myColors) <- c("individual", "managed nest", "untreated nest")
        myColors
      },
      selected = unique(combinedData$filter),
      blur = "individual",
      addGlobe = TRUE
    )
    
    expect_s3_class(myPlot, "leaflet")
    
  })



test_that("Alle observaties", {
    
    combinedData <- combineNestenData(pointsData = vespaData$points, nestenData = vespaData$nesten)
    myPlot <- mapHeat(
      combinedData = combinedData,
      colors = {
        myColors <- c("blue", "red")
        names(myColors) <- c("individual", "nest")
        myColors
      },
      selected = unique(combinedData$filter),
      addGlobe = TRUE
    )
    
    expect_s3_class(myPlot, "leaflet")
    
  })

test_that("Voorjaarsnesten", {
    
    myPlot <- barplotLenteNesten(df = read.csv(system.file("extdata", "management", "Vespa_velutina", "aantal_lente_nesten.csv", package = "alienSpecies")))
    expect_s3_class(myPlot, "ggplot")
    
  })

test_that("Provincie nesten", {
    
    myPlot <- countNesten(df = vespaData$nesten)
    expect_s3_class(myPlot, "ggplot")
    
    myTable <- tableNesten(df = vespaData$nesten)
    expect_s3_class(myTable, "data.frame")
    
  })

test_that("Map Trend", {
    
    ## Points data
    vespaPoints <- vespaData$points
    vespaPoints$type <- "individual"
    
    ## Refactor data
    # Columns
    regionVariables <- list(level3Name = "NAAM", level2Name = "provincie", level1Name = "GEWEST")
    for (iName in names(regionVariables))
      names(vespaPoints)[match(iName, names(vespaPoints))] <- regionVariables[[iName]]
    # Gewest
    vespaPoints$GEWEST <- ifelse(vespaPoints$GEWEST == "Vlaanderen", "flanders", 
      ifelse(vespaPoints$GEWEST == "Bruxelles", "brussels", 
        ifelse(vespaPoints$GEWEST == "Wallonie", "wallonia", "")))
    # Provincie
    vespaPoints$provincie <- ifelse(vespaPoints$provincie == "Vlaams Brabant", "Vlaams-Brabant",
      ifelse(vespaPoints$provincie == "Bruxelles", "HoofdstedelijkGewest", 
        ifelse(vespaPoints$provincie == "Liège", "Luik", 
          ifelse(vespaPoints$provincie == "Brabant Wallon", "Waals-Brabant",
            ifelse(vespaPoints$provincie == "Hainaut", "Henegouwen", vespaPoints$provincie)))))
    
    summaryData <- createSummaryRegions(data = vespaPoints, shapeData = allShapes,
      regionLevel = "communes",
      year = 2022,
      unit = "absolute")
    mapRegions(managementData = summaryData, shapeData = allShapes,
      regionLevel = "communes")
    
    
    summaryData <- createSummaryRegions(data = vespaPoints, shapeData = allShapes,
      regionLevel = "provinces",
      year = 2022,
      unit = "absolute")
    mapRegions(managementData = summaryData, shapeData = allShapes,
      regionLevel = "provinces")
    
    ## Nesten data
    vespaNesten <- vespaData$nesten
    vespaNesten$type <- "nest"
    
    # TODO combined on 1 map - test from here
    
    keepColumns <- c("year", "type", "NAAM", "provincie", "GEWEST", "geometry")
    vespaBoth <- rbind(vespaPoints[, keepColumns], vespaNesten[, keepColumns])
    summaryData <- createSummaryRegions(
      data = vespaBoth, shapeData = allShapes,
      regionLevel = "communes",
      year = 2022,
      unit = "absolute",
      groupingVariable = "type")
    mapRegions(managementData = summaryData, shapeData = allShapes,
      regionLevel = "communes")
    
    

  })



