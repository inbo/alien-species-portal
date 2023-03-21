# Summarize time series data
# 
# Author: mvarewyck
###############################################################################


#' Summarize time series data over the 1kmx1km grids for selected regions
#' 
#' @param rawData data.table, as created by \code{\link{createTimeSeries()}}
#' @inheritParams createCubeData 
#' @return data.table
#' 
#' @author mvarewyck
#' @export
summarizeTimeSeries <- function(rawData, region = c("flanders", "wallonia", "brussels")) {
  
  # Filter on region
  regionCols <- paste0("is", simpleCap(region))
  rawData <- rawData[rowSums(rawData[, regionCols, with = FALSE]) > 0, ]
  
  ## Summarize over the 1km x 1km grids
  # obs: number of observations for species per year
  # cobs: number of observations for class per year
  # ncells: number of 1x1 grids with species present in protected areas (occupancy)
  # c_ncells: number of 1x1 grids with class present in protected areas (occupancy)
  nonProtectedData <- rawData[, .(obs = sum(obs), cobs = sum(cobs), 
      ncells = sum(pa_obs), c_ncells = sum(pa_cobs), classKey = unique(classKey)), 
    by = .(taxonKey, year)][, protected := FALSE]
  
  protectedData <- rawData[(natura2000), .(obs = sum(obs), cobs = sum(cobs), 
      ncells = sum(pa_obs), c_ncells = sum(pa_cobs), classsKey = unique(classKey)), 
    by = .(taxonKey, year)][, protected := TRUE]
  
  combinedData <- do.call(rbindlist, list(list(nonProtectedData, protectedData), fill = TRUE))
  
  return(combinedData)
  
  
}
