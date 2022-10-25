
#' Download GBIF occurrence data 
#' 
#' @param datasetKey character, key of the dataset to be downloaded from GBIF
#' @param saveDir path, where to save the occurrence data
#' @param outFile character, name of the file to save occurrence data. If NULL
#' dataset title is used
#' @param user character, username to access GBIF
#' @param pwd character, password to access GBIF
#' @param email character, email where to send email for finishing download at GBIF
#' @return TRUE if the download succeeded
#' 
#' @importFrom rgbif occ_download pred occ_download_wait occ_download_get occ_download_import
#' @importFrom utils write.csv read.table
#' @importFrom data.table melt
#' 
#' @author mvarewyck
#' @export
getGbifOccurrence <- function(datasetKey, 
  saveDir = system.file("extdata", "management", package = "alienSpecies"), 
  outFile = NULL,
  user, pwd, email = "machteld.varewyck@openanalytics.eu") {
  
  # Download request
  gbif_downloadKey <- occ_download(pred("datasetKey", datasetKey), 
    user = user, pwd = pwd, email = email)
  occ_download_wait(gbif_downloadKey)
  # Execute download request
  gbif_download <- occ_download_get(gbif_downloadKey, path = tempdir())
  
  ## Extract occurrence data -> clean but lacks gender info if count > 1
  cleanData <- occ_download_import(x = gbif_download)
  # Remove casual observations
  cleanData <- cleanData[cleanData$samplingProtocol != "casual observation", ]
  
  # Clean columns
  cleanData <- cleanData[, c("gbifID", "kingdom", "phylum", "class", "order", "family", "species",
      "scientificName", "year", "individualCount", "samplingProtocol",
      "decimalLatitude", "decimalLongitude")]
  colnames(cleanData)[colnames(cleanData) == "individualCount"] <- "count"
  cleanData$gbifID <- as.character(cleanData$gbifID)
  
  ## Extract verbatim data -- needed for gender
  extractDir <- file.path(tempdir(), "gbifdownload", datasetKey)
  utils::unzip(gbif_download[[1]], exdir = extractDir, overwrite = TRUE)
  verbatimFile <- list.files(extractDir, pattern = "verbatim", full.names = TRUE)
  verbatimData <- read.table(file = verbatimFile, sep = "\t", header = TRUE)[,
    c("gbifID", "sex", "lifeStage")]
  # Convert json format for sex & lifeStage
  verbatimData <- cbind(verbatimData,
    # sex
    do.call(rbind, lapply(verbatimData$sex, function(x) { 
          if (x != "") {
            miniData <- as.data.frame(jsonlite::fromJSON(x))
            if (is.null(miniData$male))
              miniData$male <- NA
            if (is.null(miniData$female))
              miniData$female <- NA
            miniData
          } else
            data.frame(male = NA, female = NA) 
        })),
    # lifeStage
    do.call(rbind, lapply(verbatimData$lifeStage, function(x) { 
          if (x != "") {
            miniData <- jsonlite::fromJSON(x)
            secondAdult <- which(duplicated(names(miniData)))
            if (length(secondAdult) > 0) {
              if (names(miniData[secondAdult]) != "adult")
                stop("Please update code for this case ", x)
              miniData$adult <- miniData$adult + miniData[[secondAdult]]
              miniData[[secondAdult]] <- NULL
            }
            miniData <- as.data.frame(miniData)
            if (is.null(miniData$adult))
              miniData$adult <- NA
            if (is.null(miniData$juvenile))
              miniData$juvenile <- NA
            if (is.null(miniData$pulli))
              miniData$pulli <- NA
            miniData 
          } else
            data.frame(adult = NA, juvenile = NA, pulli = NA) 
        }))
  )
  verbatimData$sex <- NULL
  verbatimData$lifeStage <- NULL
  
  # Combine clean and verbatim data
  df <- merge(cleanData, verbatimData)
  # Check if all adults are male or female
  if (!all(apply(df[, c("male", "female")], 1, sum, na.rm = TRUE) == df$adult, na.rm = TRUE))
    stop("Some adults are not identified as male or female. Update the code to handle this case.")
  # Split per gender & lifeStage
  df$missing <- as.integer(df$count - 
      apply(df[, c("adult", "juvenile", "pulli")], 1, function(x) 
      sum(as.numeric(x), na.rm = TRUE)))
  df$missing <- ifelse(df$missing == 0, NA, df$missing)
  nTotal <- sum(df$count, na.rm = TRUE)
  df$count <- NULL
  # wide to long
  allCols <- colnames(df)
  categories <- c("male", "female", "juvenile", "pulli", "missing")
  df <- melt(as.data.table(df), id.vars = allCols[!allCols %in% categories], 
    measure.vars = categories, 
    variable.name = "gender", value.name = "count", na.rm = TRUE)
  
  # File to save
  if (is.null(outFile)) {
    if (length(unique(cleanData$species)) > 1)
      stop("More than 1 species in the occurrence data. Please specify 'outFile'.")
    outFile <- paste0(cleanData$species[1], ".csv")
  }
  outFile <- gsub(" ", "_", outFile)
  
  if (nTotal != sum(df$count))
    stop("Total counts is not retained during data manipulation.")
  
  write.csv(df, file.path(saveDir, outFile), row.names = FALSE)
  
  
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