# Test plots for the management page
# 
# Author: mvarewyck
###############################################################################


outFile <- "Oxyura jamaicensis.csv"

if (!file.exists(file.path(dataDir, "management", outFile)))
  getGbifOccurrence(datasetKey = "7522721f-4d97-4984-8231-c9e061ef46df",
    outFile = outFile, user = "mvarewyck", pwd = "6P.G6DrErq.mmUy")

managementData <- loadGbif(dataFile = outFile)
baseMap <- createBaseMap()



test_that("Map for Ruddy Duck", {
    
    # Filters on sampling
    filterValue <- unique(managementData$samplingProtocol)[1]
    managementData <- managementData[managementData$samplingProtocol == filterValue, ]
    # Filter on gender
    filterValue <- unique(managementData$gender)[1]
    managementData <- managementData[managementData$gender == filterValue, ]
    sum(managementData$count)

    mapOccurrence(occurrenceData = managementData, baseMap = baseMap, addGlobe = TRUE)
    
  })


test_that("Barplot for Ruddy Duck", {
        
    countOccurrence(df = managementData)
    
    # Filter on sampling
    filterValue <- unique(managementData$samplingProtocol)[2]
    countOccurrence(df = managementData[managementData$samplingProtocol == filterValue, ])
    
  })