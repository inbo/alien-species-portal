# Test plots for the management page
# 
# Author: mvarewyck
###############################################################################


outFile <- "Oxyura_jamaicensis.csv"

if (!file.exists(file.path(dataDir, "management", outFile)))
  getGbifOccurrence(datasetKey = "7522721f-4d97-4984-8231-c9e061ef46df",
    outFile = outFile, user = "mvarewyck", pwd = "6P.G6DrErq.mmUy")

managementData <- loadGbif(dataFile = outFile)
baseMap <- createBaseMap()


test_that("Create management data in tempdir", {
    
    skip("Takes long time to run")
    
    getGbifOccurrence(datasetKey = "7522721f-4d97-4984-8231-c9e061ef46df",
      outFile = outFile, user = "mvarewyck", pwd = "6P.G6DrErq.mmUy",
      dataDir = tempdir())
    
  })



test_that("Map for Ruddy Duck", {
    
    # Filters on sampling
    filterValue <- unique(managementData$samplingProtocol)[1]
    managementData <- managementData[managementData$samplingProtocol == filterValue, ]
    # Filter on gender
    filterValue <- unique(managementData$gender)[1]
    managementData <- managementData[managementData$gender == filterValue, ]
    sum(managementData$count)
    
    myPlot <- mapOccurrence(occurrenceData = managementData, baseMap = baseMap, addGlobe = TRUE)
    expect_s3_class(myPlot, "leaflet")
    
  })


test_that("Barplot for Ruddy Duck", {
    
    myPlot <- countOccurrence(df = managementData)
    expect_s3_class(myPlot$plot, "plotly")
    
    # Filter on sampling
    filterValue <- unique(managementData$samplingProtocol)[2]
    countOccurrence(df = managementData[managementData$samplingProtocol == filterValue, ])
    
  })


outFile <- "Lithobates_catesbeianus.csv"

managementData <- loadGbif(dataFile = outFile)

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
    
    allShapes <- c(
      # Grid data
      readShapeData(),
      # gemeentes & provinces
      readShapeData(dataDir = system.file("extdata", package = "alienSpecies"),
        extension = ".geojson")
    )
    
    occurrenceData <- loadTabularData(type = "occurrence")
    # Filter on taxonKey and year
    occurrenceData <- occurrenceData[occurrenceData$scientificName == gsub("_", " ", gsub(".csv", "", outFile)) & year == 2018, ]
    
    summaryData <- createSummaryRegions(data = managementData, 
      shapeData = allShapes, regionLevel = "communes", year = 2018, unit = "cpue")
    
    # Map
    myPlot <- mapRegions(managementData = summaryData, occurrenceData = occurrenceData, 
      shapeData = allShapes, regionLevel = "communes")
    expect_s3_class(myPlot, "leaflet")
    
  })


test_that("Trend for Bullfrogs", {
    
    # Municipalities
    summaryData <- createSummaryRegions(data = managementData, 
      shapeData = allShapes, regionLevel = "communes", year = unique(managementData$year))
    
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
    
    # Flanders
    summaryData <- createSummaryRegions(data = managementData, regionLevel = "flanders")
    
    trendYearRegion(df = summaryData)
    
  })