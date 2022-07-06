


#' Create data.frame with occupancy for t0 and t1 data per cellcode
#'  
#' @param dataDir path to folder where to find the raw t0 and t1 files
#' @param packageDir path to folder where to save the created data.frame
#' @return TRUE if creation succeeded
#' 
#' @author mvarewyck
#' @importFrom rgdal readOGR
#' @importFrom sp spTransform CRS
#' @importFrom sf st_read
#' @importFrom data.table as.data.table
#' @export
createOccupancyCube <- function(dataDir = "~/git/alien-species-portal/data/trendOccupancy",
  packageDir = system.file("extdata", package = "alienSpecies")) {
  
  ## DATA T0 ##
  ## ------- ##
  
  # geojson: https://zenodo.org/record/3835756#.Yoc5oLxBw5m
  data_t0 <- lapply(seq(2016, 2020, by = 2), function(iYear) {
      dataYear <- rgdal::readOGR(dsn = file.path(dataDir, paste0("ias_belgium_t0_", iYear, ".geojson")), verbose = TRUE)
      dataYear <- sp::spTransform(dataYear, sp::CRS("+proj=longlat +datum=WGS84"))
      # inconsistent colnames over the years
      colnames(dataYear@data) <- c("cellcode", "reference", "data_partn", "accepted", "species", "notes")
      dataYear$year <- iYear
      dataYear
    })
  # sp::plot(data_t0[[1]])
  
  data_t0_all <- do.call(rbind, data_t0)@data
  data_t0_all$source <- "t0"
  # table(data_t0_all)
  
  # Keep accepted Y or New
  data_t0_all <- data_t0_all[data_t0_all$accepted %in% c("Y", "New"), ]
  
  
  ## DATA T1 ##
  ## ------- ##
  
  # shp: https://zenodo.org/record/3060173
  data_t1 <- sf::st_read(file.path(dataDir, "T1_Belgium_Union_List_Species.shp"))
  # sp::plot(data_t1)
  
  # Keep first two words only for matching with t0 data
  data_t1$species <- sapply(strsplit(data_t1$species, split = " |\\,"), function(x) paste(x[1:2], collapse = " "))
  
  data_t1$source <- "t1"
  data_t1 <- as.data.frame(data_t1)  # drop spatial info
  data_t1$cellcode <- data_t1$CellCode
  data_t1$year <- NA
  
  
  ## COMBINE DATA ##
  ## ------------ ##
  
  
  dfCube <- rbind(data_t0_all[, c("species", "source", "cellcode", "year")], 
    data_t1[, c("species", "source", "cellcode", "year")])
  dfCube <- as.data.table(dfCube[!duplicated(dfCube), ])
  setnames(dfCube, "cellcode", "cell_code10")
  
  save(dfCube, file = file.path(packageDir, "dfCube.RData"))
  
  return(TRUE)  
  
}




#' Create data.frame with occupancy for t0 and t1 data
#' 
#' @param dfCube data.table, with species, source and cell code column
#' @author mvarewyck
#' @importFrom reshape2 dcast
createOccupancyData <- function(dfCube) {
  
  dfCube$cell_code10 <- NULL
  dfCube$year <- NULL
  dfTable <- reshape2::dcast(data = as.data.frame(table(dfCube)), 
    species ~ source, value.var = "Freq")
  dfTable$total <- dfTable$t0 + dfTable$t1
  
  dfTable <- dfTable[order(dfTable$total), ]
  dfTable$species <- factor(dfTable$species, levels = unique(dfTable$species)) # sort by freq in barchart
  
  dfTable
  
}
