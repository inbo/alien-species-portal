
#' Download GBIF occurrence data 
#' 
#' @param datasetKey character, key of the dataset to be downloaded from GBIF
#' @param bucket character, name of the S3 bucket as specified in the config.yml file;
#' default value is "inbo-exotenportaal-uat-eu-west-1-default". The created data.frame will be 
#' saved in the bucket.
#' @param outFile character, name of the file to save occurrence data. If NULL
#' dataset title is used
#' @param user character, username to access GBIF
#' @param pwd character, password to access GBIF
#' @param email character, email where to send email for finishing download at GBIF
#' @return TRUE if the download succeeded
#' 
#' @importFrom rgbif occ_download pred occ_download_wait occ_download_get occ_download_import
#' @importFrom utils write.csv read.table
#' @importFrom aws.s3 put_object
#' @importFrom data.table melt as.data.table
#' 
#' @author mvarewyck
#' @export
getGbifOccurrence <- function(datasetKey, 
    bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")),
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
      "decimalLatitude", "decimalLongitude", "level1Name")]
  colnames(cleanData)[colnames(cleanData) == "individualCount"] <- "count"
  cleanData$gbifID <- as.character(cleanData$gbifID)
  
  # boolean for regions
  cleanData$isFlanders <- cleanData$level1Name == "Vlaanderen"
  cleanData$isBrussels <- cleanData$level1Name == "Bruxelles"
  cleanData$isWallonia <- cleanData$level1Name == "Wallonie"
  cleanData$level1Name <- NULL
  
  
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
  verbatimData$gbifID <- as.character(verbatimData$gbifID)
  
  
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
  df <- data.table::melt(as.data.table(df), id.vars = allCols[!allCols %in% categories], 
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
  
  write.csv(df, file.path(tempdir(), outFile), row.names = FALSE)
  
  put_object(file = file.path(tempdir(), outFile),
             bucket = bucket, object = outFile, 
             opts = list(region = Sys.getenv("AWS_DEFAULT_REGION", unset = 'eu-west-1')))
  
  return(TRUE)
  
}



#' Read GBIF occurrence data
#' @param dataFile character, name of the occurrence data file
#' @inheritParams readS3
#' @return data.table
#' 
#' @importFrom data.table fread setnames
#' 
#' @author mvarewyck
#' @export
loadGbif <- function(dataFile, 
  bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies"))
  ) {
    
  # For R CMD check
  count <- decimalLongitude <- decimalLatitude <- NULL
  
  rawData <- readS3(FUN = fread, stringsAsFactors = FALSE, na.strings = "", bucket = bucket, 
                    file = dataFile)
  
  # Rename
  if ("individualCount" %in% colnames(rawData) & !"count" %in% colnames(rawData)) {
    data.table::setnames(rawData, "individualCount", "count")
    rawData[, count := as.numeric(count)]
  }
  if ("decimalLongitude" %in% colnames(rawData))
    rawData[, decimalLongitude := as.numeric(decimalLongitude)]
  if ("decimalLatitude" %in% colnames(rawData))
    rawData[, decimalLatitude := as.numeric(decimalLatitude)]
  
  
  attr(rawData, "Date") <- file.mtime(dataFile)
  
  return(rawData)
  
}
