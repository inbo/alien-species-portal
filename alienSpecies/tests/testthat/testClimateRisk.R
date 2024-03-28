# Climate risk maps
# 
# Code: https://github.com/trias-project/risk-maps/tree/main
# Live: https://trias-project.github.io/risk-maps/
#
# Author: mvarewyck
###############################################################################


taxData <- loadTabularData(type = "occurrence")
allSpecies <- c("Psittacula krameri")

test_that("Create climate risk map", {
    
    # Specify trias risk map file
    
    keyChoice <- unique(taxData$taxonKey[taxData$scientificName %in% allSpecies])
    keyChoice <- 2479226
    
    climateChoice <- c("hist", "rcp26", "rcp45", "rcp85")[1]
    mapChoice <- c("", "conf", "diff")[1]
    
    tiffPath <- "https://raw.githubusercontent.com/trias-project/risk-maps/main/public/geotiffs"
    tiffFile <- paste0(paste("be", keyChoice, climateChoice, sep = "_"),
      if (mapChoice != "") paste0("_", mapChoice), ".4326.tif")
    
    # List files from GIT -> if any files for the species, show tab 'More' 
    library(httr)
    request <- GET("https://api.github.com/repos/trias-project/risk-maps/contents/public/geotiffs")
    stop_for_status(request)
    filelist <- sapply(content(request), function(x) x$path)
    keyFiles <- grep(keyChoice, filelist, value = TRUE, fixed = TRUE)
    
    keyChoices <- unique(sapply(content(request), function(x) strsplit(gsub("public/geotiffs/be_", "", x$path), split = "_")[[1]][1]))
    
    # Given user specification
    download.file(file.path(tiffPath, tiffFile), 
      destfile = file.path(tempdir(), tiffFile), method = "curl")
    
    library(terra)
    rasterInput <- terra::rast(x = file.path(tempdir(), tiffFile))
    #plot(r)
    # Error: external pointer is not valid
    # Ignore, doesn't limit plotting and only in Eclipse
    # See also: https://www.eclipse.org/lists/statet-users/msg00142.html
    
    # Need leaflet >= 2.2.0 (https://github.com/rstudio/leaflet/issues/865)
    myPlot <- mapRaster(rasterInput = rasterInput, addGlobe = FALSE)
    
    myPlot %>% addTiles(options = tileOptions(zIndex = -10))
    
  })

## CODE from trias project - which tif files are used

#if (this.mapTypeId === "") {
#  return `${this.publicPath}geotiffs/be_${this.speciesId}_${this.climateScenarioId}.4326.tif`;
#} else {
#  return `${this.publicPath}geotiffs/be_${this.speciesId}_${this.climateScenarioId}_${this.mapTypeId}.4326.tif`;
#}



