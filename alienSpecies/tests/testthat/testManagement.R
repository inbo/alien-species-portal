# Test plots for the management page
# 
# Author: mvarewyck
###############################################################################


outFile <- "Oxyura.csv"

if (!file.exists(file.path(dataDir, "management", outFile)))
  getGbifOccurrence(datasetKey = "7522721f-4d97-4984-8231-c9e061ef46df",
    outFile = outFile, user = "mvarewyck", pwd = "6P.G6DrErq.mmUy")

managementData <- loadGbif(dataFile = outFile)
baseMap <- createBaseMap()



test_that("Map for Ruddy Duck", {
    
    mapOccurrence(occurrenceData = managementData, baseMap = baseMap)
    
  })


test_that("Barplot for Ruddy Duck", {
    
    
    countOccurrence(df = managementData)
    
  })