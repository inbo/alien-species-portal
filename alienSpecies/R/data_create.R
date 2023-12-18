


#' Create data.frame with occupancy for t0 and t1 data per cellcode
#'  
#' @param dataDir path to folder where to find the raw data files
#' @param bucket character, name of the S3 bucket as specified in the config.yml file;
#' default value is "inbo-exotenportaal-uat-eu-west-1-default". The created data.frame will be 
#' saved in the bucket.
#' @return TRUE if creation succeeded
#' 
#' @author mvarewyck
#' @importFrom sf st_read st_transform
#' @importFrom aws.s3 s3save
#' @importFrom data.table as.data.table
#' @importFrom utils capture.output
#' @export
createOccupancyCube <- function(dataDir = "~/git/alien-species-portal/data/trendOccupancy",
                                bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies"))
                                
) {
  
  ## DATA T0 ##
  ## ------- ##
  # sf::sf_use_s2(FALSE) does not work
  # geojson: https://zenodo.org/record/3835756#.Yoc5oLxBw5m
  data_t0 <- lapply(seq(2016, 2020, by = 2), function(iYear) {
    dataYear <- sf::st_read(file.path(dataDir, paste0("ias_belgium_t0_", iYear, ".geojson")), quiet = TRUE)
    # inconsistent colnames over the years
    dataYear <-  sf::st_drop_geometry(dataYear)
    colnames(dataYear) <- c("cellcode", "reference", "data_partn", "accepted", "species", "notes")
    dataYear$year <- iYear
    dataYear
  })
  # sp::plot(data_t0[[2]])
  
  
  
  data_t0_all <- do.call(rbind, data_t0)
  data_t0_all$source <- "t0"
  # table(data_t0_all)
  
  # Keep accepted Y or New
  data_t0_all <- data_t0_all[data_t0_all$accepted %in% c("Y", "New"), ]
  
  
  ## DATA T1 ##
  ## ------- ##
  
  # shp: https://zenodo.org/record/3060173
  concealOutputMessage <- capture.output( data_t1_temp <- sf::st_read(file.path(dataDir, "T1_Belgium_Union_List_Species.shp"))
  )
  # convert the crs to GEOGS format
  suppressMessages( data_t1 <- sf::st_transform(data_t1_temp, 4326))
  #sf::st_crs(data_t1)
  # sp::plot(data_t1)
  
  # Keep first two words only for matching with t0 data
  data_t1$species <- sapply(strsplit(data_t1$species, split = " |\\,"), function(x) paste(x[1:2], collapse = " "))
  
  data_t1$source <- "t1"
  data_t1 <- sf::st_drop_geometry(data_t1)  # drop spatial info
  data_t1$cellcode <- data_t1$CellCode
  data_t1$year <- NA
  
  
  ## COMBINE DATA ##
  ## ------------ ##
  
  
  dfCube <- rbind(data_t0_all[, c("species", "source", "cellcode", "year")], 
                  data_t1[, c("species", "source", "cellcode", "year")])
  
  
  dfCube <- as.data.table(dfCube[!duplicated(dfCube), ])
  setnames(dfCube, "cellcode", "cell_code10")
  
  s3save(dfCube, bucket = bucket, object = "dfCube.RData",
    opts = list(region = Sys.getenv("AWS_DEFAULT_REGION", unset = 'eu-west-1')))
  
  return(TRUE)  
  
}



#' Create dictionary for combining data keys from multiple data sources
#' @inheritParams loadTabularData
#' @param dataDir path, to folder where to read data from
#' @return TRUE if creation succeeded, data is written to \code{file.path(dataDir, "keys.csv")}
#' 
#' @author mvarewyck
#' @importFrom utils write.csv
#' @importFrom data.table fread setnames as.data.table
#' @importFrom rgbif name_usage
#' @export

createKeyData <- function(
    bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")),
    dataDir = "~/git/alien-species-portal/data"){
  
  # For R CMD check
  classKey <- NULL
  i.classKey <- NULL
  
  # Occurrence cube
  taxaData <- fread(file.path(dataDir, "be_alientaxa_info.csv"))
  #taxaData <- s3read_using(FUN = fread, object = "be_alientaxa_info.csv", bucket = bucket)
  
  
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
  
  
  # write.csv(dictionary, file = file.path(dataDir, "keys.csv"),
  #           row.names = FALSE)  
  
  aws.s3::s3write_using(dictionary, FUN = write.csv,
                bucket = bucket,
                object = "keys.csv",
                opts = list(region = Sys.getenv("AWS_DEFAULT_REGION", unset = 'eu-west-1')))
  
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
#' @export
#' 
createTimeseries <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")),
  shapeData = loadShapeData("grid.RData")$utm1_bel_with_regions) {
  
                             
  # For R CMD check
  df_ts <- NULL
  
  # created from https://github.com/inbo/aspbo/blob/uat/src/05_occurrence_indicators_preprocessing.Rmd
  ## Data at 1km x 1km grid level
  s3load("df_timeseries.Rdata", bucket = bucket)
  
  # obs: number of observations for species
  # cobs: number of observations for class
  # pa_obs: presence of species in protected areas (1 = yes, 0 = no)
  # pa_cobs: presence of class in protected areas (1 = yes, 0 = no)
  
  # Merge with shapeData for region indicators
  regions <- c("flanders", "wallonia", "brussels")
  timeseries <- merge(df_ts, sf::st_drop_geometry(shapeData)[, c("CELLCODE", paste0("is", simpleCap(regions)))],
                      by.x = "eea_cell_code", by.y = "CELLCODE")
  
  # put time series data RData to the bucket to speed up reading process
  
  s3save(timeseries, object = "full_timeseries.RData", bucket = bucket, 
    opts = list(show_progress = TRUE, 
      region = Sys.getenv("AWS_DEFAULT_REGION", unset = 'eu-west-1')))
  
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
#' 
createShapeData <- function(
    dataDir = "~/git/alien-species-portal/data/occurrenceCube",
    bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies"))
) {
  
  extension <- c(".gpkg", ".shp", ".geojson")
  
  dataFiles <- if (file.info(dataDir)$isdir)  grep(paste0("(",paste(  extension, collapse = "|"), ")$"), list.files(dataDir,  recursive = TRUE), value = TRUE) else dataDir
  objectNameList <- unique(tools::file_path_sans_ext( dataFiles ))

  if (length(objectNameList) > 1) {
    objectName <- basename(dataDir)
    shapeFiles <- list.files(dataDir,  recursive = TRUE, 
                             pattern = paste0("(",paste(  extension, collapse = "|"), ")$"))
    names(shapeFiles) <- tools::file_path_sans_ext(shapeFiles)
   
    assign(objectName, sapply(names(shapeFiles), function(iFile) {
      
      sf::st_read(file.path(dataDir, shapeFiles[[iFile]]), quiet = TRUE, layer = iFile)
      
    }, simplify = FALSE)
    )  
    
  } else {
    objectName <- basename(tools::file_path_sans_ext( dataFiles ))
    assign(objectName, sf::st_read(dataDir, layer = objectName, quiet = TRUE))
    
  }
  
  newFile <- paste0(objectName, ".RData")
  s3save(list = objectName, bucket = bucket, object = newFile,
    opts = list(region = Sys.getenv("AWS_DEFAULT_REGION", unset = 'eu-west-1')))
  
  return(TRUE)
  
} 

#' Create tabular data
#' 
#' For indicator data:
#' by default data for which \code{first_observed < 1950} is excluded.
#' For unionlist data:
#' only 'scientific name', 'english name' and 'kingdom' are retained
#' @inheritParams readS3
#' @param dataDir path, to folder where to read data from
#' @param type data type, one of:
#' \itemize{
#' \item{\code{"indicators"}:}{for indicator data, i.e. main data set}
#' \item{\code{"unionlist"}:}{for union list data, i.e. }
#' }
#' @return data.table, loaded indicator/unionlist data; 
#' and attribute 'Date', the date that this data file was created
#' @importFrom data.table fread :=
#' @importFrom utils tail
#' @export

createTabularData <- function(
    dataDir = "~/git/alien-species-portal/dataS3",
    bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")),
    type = c("indicators", "unionlist", "occurrence")) {
  
  
  # For R CMD check
  scientificName <- NULL
  i.scientificName <- NULL
  i.classKey <- NULL
  
  warningMessage <- NULL
  
  type <- match.arg(type)
  
  dataFiles <- file.path(dataDir, switch(type,
                                         "indicators" = "data_input_checklist_indicators.tsv",
                                         "unionlist" = "eu_concern_species.tsv",
                                         "occurrence" = "be_alientaxa_cube.csv")
  )
  
  
  if (type == "indicators"){
    
    # recode missing values to NA
    rawData <- fread(dataFiles, stringsAsFactors = FALSE, na.strings = "")
    # Warning if new habitat columns
    currentHabitats <- c("marine", "freshwater", "terrestrial")
    rawData$habitat[rawData$habitat == "NA"] <- NA
    
    if (!all(unlist(strsplit(rawData$habitat, split = "|", fixed = TRUE)) %in% c(NA, currentHabitats)))  
      # loadTabularData() no longer does any data manupulation, change the messag accordingly
      stop("New habitats detected. Add relevant columns in loadTabularData()")
    
    ## extract necessary columns
    rawData <- rawData[, c(
      # GBIF key - necessary to use trias function
      "key", 
      # Taxon key
      "nubKey",
      # full scientific name
      "scientificName",
      # Period - slider should use first_observed
      "first_observed", "last_observed", 
      # Taxonomy
      "kingdom", "phylum", "class", "order", "family",
      # Taxonomy keys - for search queries
      "kingdomKey", "phylumKey", "classKey", "orderKey", "familyKey",
      # Locality
      "locality", "locationId", "native_range",
      # Degree establishment
      "degree_of_establishment",
      # Pathway
      "pathway_level1", "pathway_level2",
      # Habitat
      "habitat", ## easier to use the 3 booleans below instead
      ..currentHabitats,
      # Source
      "source",
      # union list filtering
      "species", "canonicalName"
    )]
    
    ## convert english to dutch names for region
    rawData$locality <- getDutchNames(rawData$locality, type = "regio")
    
    ## Extract hyperlinks 
    
    ### Extract gbif link
    rawData$gbifLink <- sapply(strsplit(rawData$source, split = ": "), function(x) x[1])
    rawData$gbifLink <- paste0("<a href='", rawData$gbifLink, "' target = '_blank'>", 
                               sapply(strsplit(rawData$gbifLink, split = "/"), function(x) tail(x, n = 1)), "</a>")
    # common name and source: https://www.gbif.org/species/157131005
    
    ## recode `source` variable
    
    ## Remove everything up until (jjjj)<space(s)>.<space(s)>
    rawData$source <- sub("(.*?)\\(\\d{4}\\)\\s*\\.\\s*", "", rawData$source)
    ## Remove everything after the first dot
    rawData$source <- sub("\\..*$", "", rawData$source)
    
    ## re-define source into shorter names
    rawData[, source := as.factor(source)]
    levels(rawData$source) <- list(
      "Ad hoc alien species checklist" = "Ad hoc checklist of alien species in Belgium",
      "Alien Bird Checklist" = "Checklist of alien birds of Belgium",
      "Alien Fish Flanders" = "Checklist of non-native freshwater fishes in Flanders, Belgium",
      "Alien Macroinverts" = "Inventory of alien macroinvertebrates in Flanders, Belgium",
      "Alien Mollusc checklist" = "Registry of introduced terrestrial molluscs in Belgium", 
      "Belgian rust fungi" = "Catalogue of the Rust Fungi of Belgium",
      "Manual of Alien Plants" = "Manual of the Alien Plants of Belgium",
      "RINSE1" = "RINSE - Registry of non-native species in the Two Seas region countries (Great Britain, France, Belgium and the Netherlands)",
      "RINSE2" = "RINSE - Pathways and vectors of biological invasions in Northwest Europe",
      "WRiMS" = "World Register of Introduced Marine Species (WRiMS)")
    
    ### source link
    rawData$sourceLink <- ifelse(is.na(rawData$source), "", 
                                 paste0("<a href='", sapply(as.character(rawData$source), function(iSource) switch(iSource,
                                                                                                                   "Ad hoc alien species checklist" = "https://www.gbif.org/dataset/1f3505cd-5d98-4e23-bd3b-ffe59d05d7c2",
                                                                                                                   "Alien Bird Checklist" = "https://www.gbif.org/dataset/e1c3be64-2799-4342-8312-49d076993132",
                                                                                                                   "Alien Fish Flanders" = "https://www.gbif.org/dataset/98940a79-2bf1-46e6-afd6-ba2e85a26f9f",
                                                                                                                   "Alien Macroinverts" = "https://www.gbif.org/dataset/289244ee-e1c1-49aa-b2d7-d379391ce265",
                                                                                                                   "Alien Mollusc checklist" = "https://www.gbif.org/dataset/e082b10e-476f-43c1-aa61-f8d92f33029a",
                                                                                                                   "Belgian rust fungi" = "https://www.gbif.org/dataset/b043c480-dd36-4f4f-aa82-e188753ff09d",
                                                                                                                   "Manual of Alien Plants" = "https://www.gbif.org/dataset/9ff7d317-609b-4c08-bd86-3bc404b77c42",
                                                                                                                   "RINSE1" = "https://www.gbif.org/dataset/3f5e930b-52a5-461d-87ec-26ecd66f14a3",
                                                                                                                   "RINSE2" = "https://www.gbif.org/dataset/1738f272-6b5d-4f43-9a92-453a8c5ea50a",
                                                                                                                   "WRiMS" = "https://www.gbif.org/dataset/0a2eaf0c-5504-4f48-a47f-c94229029dc8")
                                 ), 
                                 "' target = '_blank'>", rawData$source, "</a>"))
    
    ## regroup native_range variable into new native_continent variable
    ## from https://en.wikipedia.org/wiki/United_Nations_geoscheme
    
    africa <- c("Africa", "Northern Africa", "Sub-Saharan Africa", "Subsaharan Africa", "Eastern Africa", "Middle Africa", "Southern Africa", "Western Africa")
    americas <- c("Americas", "Latin America and the Caribbean", "Caribbean", "Central America", "South America", "Northern America")
    asia <- c("Asia", "Central Asia", "Eastern Asia", "South-eastern Asia", "Southeastern Asia", "Southern Asia", "Western Asia")
    europe <- c("Europe", "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe")
    oceania <- c("Oceania", "Australia and New Zealand", "Melanesia", "Micronesia", "Polynesia")
    
    rawData$native_continent[rawData$native_range %in% c(africa, tolower(africa))] <- "Africa"
    rawData$native_continent[rawData$native_range %in% c(americas, tolower(americas))] <- "Americas"
    rawData$native_continent[rawData$native_range %in% c(asia, tolower(asia))] <- "Asia"
    rawData$native_continent[rawData$native_range %in% c(europe, tolower(europe))] <- "Europe"
    rawData$native_continent[rawData$native_range %in% c(oceania, tolower(oceania))] <- "Oceania"
    
    # group any undefined regions in "undefined"
    rawData$native_continent[!(rawData$native_range %in% c(africa, tolower(africa),
                                                           americas, tolower(americas),
                                                           asia, tolower(asia),
                                                           europe, tolower(europe),
                                                           oceania, tolower(oceania))) & 
                               !is.na(rawData$native_range)] <- "undefined"
    
    ## replace missing "species" with "canonicalName" if available
    # then drop "canonicalName"
    ind <- which(is.na(rawData$species) & !is.na(rawData$canonicalName))
    rawData$species[ind] <- rawData$canonicalName[ind]
    rawData$canonicalName <- NULL
    warningMessage <- c(warningMessage,
                        paste0(type, " data: Voor ", length(ind), " observaties is de 'species' onbekend. 'canonicalName' wordt gebruikt in de plaats."))
    
    
    attr(rawData, "habitats") <- currentHabitats
    
    
  } else if (type == "unionlist") {
    
    rawData <- fread(dataFiles, stringsAsFactors = FALSE, na.strings = "",
      select = c("checklist_scientificName", "english_name", "checklist_kingdom", "backbone_taxonKey"),
      col.names = c("scientificName", "englishName", "kingdom", "taxonKey")
    )
    
  } else if (type == "occurrence") {
    
    rawData <- fread(dataFiles, stringsAsFactors = FALSE, na.strings = "",
      drop = "min_coord_uncertainty")
    
    ## exclude data before 1950 - keeps values with NA for first_observed
    toExclude <- (rawData$year < 1950 & !is.na(rawData$year))
    warningMessage <- c(warningMessage,
                        paste0(type, " data: ", sum(toExclude), " observaties dateren van voor 1950 en zijn dus uitgesloten"))
    
    rawData <- rawData[!toExclude, ]
    
    # for linking scientific name & classKey
    dictionary <- loadMetaData(type = "keys")
    rawData <- rawData[dictionary, c("scientificName", "classKey") := list(
      i.scientificName,
      i.classKey), on = "taxonKey"]
    
    # attach 10km cellcode
    rawData$cell_code10 <- gsub(pattern = "1km", replacement = "10km", 
                                paste0(substr(rawData$eea_cell_code, start = 1, stop = 7),
                                       substr(rawData$eea_cell_code, start = 9, stop = 12)))
    colnames(rawData)[colnames(rawData) == "eea_cell_code"] <- "cell_code1"
    
  }
  
  attr(rawData, "Date") <- file.mtime(dataFiles)
  attr(rawData, "warning") <- warningMessage 
  
  s3save(rawData, bucket = bucket, 
         object = paste0(basename(tools::file_path_sans_ext(dataFiles)), "_processed.RData"), 
         opts = list(multipart = TRUE,
           region = Sys.getenv("AWS_DEFAULT_REGION", unset = 'eu-west-1')))
  
  
  return(TRUE)  
  
}



