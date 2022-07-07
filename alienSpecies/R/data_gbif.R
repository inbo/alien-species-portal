
#datasetKey <- "7522721f-4d97-4984-8231-c9e061ef46df"
#user <- "mvarewyck"
#pwd <- "6P.G6DrErq.mmUy"



#' Download GBIF occurrence data 
#' 
#' @param datasetKey character, key of the dataset to be downloaded from GBIF
#' @param dataDir path, where to save the occurrence data
#' @param outFile character, name of the file to save occurrence data. If NULL
#' dataset title is used
#' @param user character, username to access GBIF
#' @param pwd character, password to access GBIF
#' @param email character, email where to send email for finishing download at GBIF
#' @return TRUE if the download succeeded
#' 
#' @importFrom rgbif occ_download occ_download_wait occ_download_get occ_download_import
#' @importFrom utils write.csv
#' 
#' @author mvarewyck
#' @export
getGbifOccurrence <- function(datasetKey, 
  dataDir = system.file("extdata", "management", package = "alienSpecies"), 
  outFile = NULL,
  user, pwd, email = "machteld.varewyck@openanalytics.eu") {
  
  # Download request
  gbif_downloadKey <- occ_download(pred("datasetKey", datasetKey), 
    user = user, pwd = pwd, email = email)
  occ_download_wait(gbif_downloadKey)
  # Execute download request
  gbif_download <- occ_download_get(gbif_downloadKey, path = tempdir())
  
  ## Extract occurrence data -> clean but lacks some columns
  cleanData <- occ_download_import(x = gbif_download)
  
  if (is.null(outFile)) {
    if (length(unique(cleanData$genus)) > 1)
      stop("More than 1 genus in the occurrence data. Please specify 'outFile'.")
    outFile <- paste0(cleanData$genus[1], ".csv")
  }
  # Clean columns
  cleanData <- cleanData[, c("gbifID", "kingdom", "phylum", "class", "order", "family", "species",
      "scientificName", "year", "individualCount", "samplingProtocol",
      "decimalLatitude", "decimalLongitude")]
  colnames(cleanData)[colnames(cleanData) == "individualCount"] <- "count"
  
  
  ## Extract verbatim data -- needed for gender
  extractDir <- file.path(tempdir(), "gbifdownload", datasetKey)
  utils::unzip(gbif_download[[1]], exdir = extractDir, overwrite = TRUE)
  verbatimFile <- list.files(extractDir, pattern = "verbatim", full.names = TRUE)
  verbatimData <- read.table(file = verbatimFile, sep = "\t", header = TRUE)[,
    c("gbifID", "sex")]
  # Convert json format for sex
  verbatimData <- cbind(verbatimData, do.call(rbind, lapply(verbatimData$sex, function(x) { 
          if (x != "") {
            miniData <- as.data.frame(jsonlite::fromJSON(x))
            if (is.null(miniData$male))
              miniData$male <- NA
            if (is.null(miniData$female))
              miniData$female <- NA
            miniData
          } else
            data.frame(male = NA, female = NA) 
        })))
  verbatimData$sex <- NULL
  
  # Combine clean and verbatim data
# TODO why merge not working on gbifID?
#  df <- merge(cleanData, verbatimData)
  df <- cbind(cleanData, verbatimData)
  
  write.csv(df, file.path(dataDir, outFile), row.names = FALSE)
  
  return(TRUE)
  
}



#' Read GBIF occurrence data
#' @param dataFile character, name of the occurrence data file
#' @param dataDir path, where to find the \code{dataFile}
#' @return data.table
#' 
#' @importFrom data.table fread
#' 
#' @author mvarewyck
#' @export
loadGbif <- function(dataFile, 
  dataDir = system.file("extdata", "management", package = "alienSpecies")) {
  
  rawData <- fread(file.path(dataDir, dataFile), stringsAsFactors = FALSE, na.strings = "")
  attr(rawData, "Date") <- file.mtime(dataFile)
  
  return(rawData)
  
}
