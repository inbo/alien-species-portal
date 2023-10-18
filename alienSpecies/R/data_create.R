


#' Create data.frame with occupancy for t0 and t1 data per cellcode
#'  
#' @param dataDir path to folder where to find the raw data files
#' @param bucket character, name of the S3 bucket as specified in the config.yml file;
#' default value is "inbo-exotenportaal-uat-eu-west-1-default". The created data.frame will be 
#' saved in the bucket.
#' @return TRUE if creation succeeded
#' 
#' @author mvarewyck
#' @importFrom sf st_read
#' @importFrom aws.s3 s3save
#' @importFrom data.table as.data.table
#' @export
createOccupancyCube <- function(dataDir = "~/git/alien-species-portal/data/trendOccupancy",
  bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies"))

  ) {
  
  ## DATA T0 ##
  ## ------- ##
  
  # geojson: https://zenodo.org/record/3835756#.Yoc5oLxBw5m
  data_t0 <- lapply(seq(2016, 2020, by = 2), function(iYear) {
      dataYear <- sf::st_read(file.path(dataDir, paste0("ias_belgium_t0_", iYear, ".geojson")), quiet = TRUE)
      # inconsistent colnames over the years
      colnames(dataYear) <- c("cellcode", "reference", "data_partn", "accepted", "species", "notes")
      dataYear$year <- iYear
      dataYear
    })
  # sp::plot(data_t0[[1]])
  
  data_t0_all <- do.call(rbind, data_t0)
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
  
  s3save(dfCube, bucket = bucket, object = "dfCube.RData")
  
  return(TRUE)  
  
}



#' Create dictionary for combining data keys from multiple data sources
#' @inheritParams loadTabularData
#' @return TRUE if creation succeeded, data is written to \code{file.path(dataDir, "keys.csv")}
#' 
#' @author mvarewyck
#' @importFrom utils write.csv
#' @importFrom data.table fread setnames as.data.table
#' @importFrom rgbif name_usage
#' @export
createKeyData <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies"))
#dataDir = system.file("extdata", package = "alienSpecies")
) {
  
  # For R CMD check
  classKey <- NULL
  i.classKey <- NULL
  
  # Occurrence cube
  #taxaData <- fread(file.path(dataDir, "be_alientaxa_info.csv"))
  taxaData <- s3read_using(FUN = fread, object = "be_alientaxa_info.csv", bucket = bucket)
  
  
  classKeys <- as.data.table(do.call(rbind, lapply(unique(taxaData$taxonKey), function(k) {
          tmpData <- rgbif::name_usage(key = k)$data
          if (!"classKey" %in% colnames(tmpData))
            tmpData$classKey <- NA
          tmpData[, c("key", "classKey")]
        })))
  setnames(classKeys, "key", "taxonKey")
  taxaData <- taxaData[classKeys, classKey := i.classKey, on = "taxonKey"]
  
  ## clean scientific name
  taxaData$scientificName <- sapply(strsplit(taxaData$scientificName, split = " "), function(x) paste(x[1:2], collapse = " "))
  
  # Checklist
  exotenData <-loadTabularData(type = "indicators")[, c('key', 'species')]
  
  
  exotenData <- exotenData[!duplicated(exotenData), ]
  dictionary <- merge(taxaData, exotenData,
    by.x = "scientificName", by.y = "species", all = TRUE)
  setnames(dictionary, "key", "gbifKey")
  
  
  write.csv(dictionary, file = file.path(dataDir, "keys.csv"),
    row.names = FALSE)  
  
  return(TRUE)
  
}


#' Summarize timeseries data over 1km x 1km cubes
#' @inheritParams createOccupancyCube
#' @inheritParams readS3
#' @param shapeData spatialPolygonsDataFrame for utm1 grid data
#' @return TRUE if creation succeeded
#' 
#' @author mvarewyck
#' @importFrom utils write.csv
#' @importFrom data.table fread rbindlist
#' @importFrom aws.s3 put_object

createTimeseries <- function(dataDir = "~/git/alien-species-portal/data",
  bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")),
  shapeData = readShapeData()$utm1_bel_with_regions) {
  
  # created from https://github.com/trias-project/indicators/blob/master/src/05_occurrence_indicators_preprocessing.Rmd
  ## Data at 1km x 1km grid level
  rawData <- fread(file.path(dataDir, "df_timeseries.tsv"), 
    stringsAsFactors = FALSE, na.strings = "")
  # obs: number of observations for species
  # cobs: number of observations for class
  # pa_obs: presence of species in protected areas (1 = yes, 0 = no)
  # pa_cobs: presence of class in protected areas (1 = yes, 0 = no)
  
  # Merge with shapeData for region indicators
  regions <- c("flanders", "wallonia", "brussels")
  timeseries <- merge(rawData, sf::st_drop_geometry(shapeData)[, c("CELLCODE", paste0("is", simpleCap(regions)))],
    by.x = "eea_cell_code", by.y = "CELLCODE")
  
  # put_object(file = file.path(packageDir, "full_timeseries.csv"),
  #            bucket = bucket, object = "full_timeseries.csv")
  
  # put time series data RData to the bucket to speed up reading process
  
  s3save(timeseries, object = "full_timeseries.RData", bucket = bucket)
  
  return(TRUE)
  
}


#' Create .RData object in bucket based on the input files
#' 
#' @inheritParams createOccupancyCube
#' 
#' @return character, file name of created object in S3 bucket 
#' 
#' @author mvarewyck
#' @export
createShapeData <- function(
  dataDir = "~/git/alien-species-portal/data",
  bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies"))
) {
  
  
  objectName <- unique(tools::file_path_sans_ext(list.files(dataDir)))
  
  if (length(objectName) > 1) {
    
    objectName <- basename(dataDir)
    shapeFiles <- list.files(dataDir)
    names(shapeFiles) <- tools::file_path_sans_ext(shapeFiles)
    
    assign(objectName, sapply(names(shapeFiles), function(iFile) {
          
          sf::st_read(file.path(dataDir, shapeFiles[[iFile]]), layer = iFile, quiet = TRUE)
          
        }))  
        
  } else {
    
    assign(objectName, sf::st_read(dataDir, layer = objectName, quiet = TRUE))
    
  }
  
  newFile <- paste0(objectName, ".RData")
  s3save(list = objectName, bucket = bucket, object = newFile)
  
  return(newFile)
    
} 



