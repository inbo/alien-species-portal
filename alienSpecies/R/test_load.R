# TODO: Add comment
# 
# Author: mvarewyck
###############################################################################


library(rgbif)

fullData <- name_lookup(
  query = "Tricellaria",
  datasetKey = "0a2eaf0c-5504-4f48-a47f-c94229029dc8",
  limit = 10000)
fullData$names
fullData$data$key


myRequest <- httr::GET("https://api.gbif.org/v1/species/157131005/vernacularNames")
httr::content(myRequest)$results


sessionInfo()