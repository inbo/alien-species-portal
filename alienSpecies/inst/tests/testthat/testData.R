# Tests to create and load the data
# 
# Author: mvarewyck
###############################################################################

library(testthat)

test_that("Set up connection to S3", {
  
 setupS3()
  
})

test_that("Connection to S3", {
  
  checkS3()
  
})


test_that("Preprocess data", {
  
  skip("For local use only - will overwrite files")
  
  downloadS3()
  
# Clean the bucket
#  aws.s3::delete_object(object = "readme.md", bucket = "inbo-exotenportaal-uat-eu-west-1-default")

    # create shape data
  createShapeData(dataDir = "~/git/alien-species-portal/data/grid")
  createShapeData(dataDir = "~/git/alien-species-portal/data/Vespa_velutina_shape")
  createShapeData(dataDir = "~/git/alien-species-portal/data/occurrenceCube")
  createShapeData(dataDir = "~/git/alien-species-portal/dataS3/provinces.geojson")
  createShapeData(dataDir = "~/git/alien-species-portal/dataS3/communes.geojson")
  
  # create key data
  createKeyData(dataDir = "~/git/alien-species-portal/data")
  
  # create time series data
  createTimeseries(
    dataDir = "~/git/alien-species-portal/data", 
    shapeData = loadShapeData("grid.RData")$utm1_bel_with_regions
  )
  
  # create occupancy cube - gives error

  createOccupancyCube("~/git/alien-species-portal/data/trendOccupancy")
  
 # create tabular data
  
  createTabularData(type = "indicators")
  createTabularData(type = "unionlist")
  createTabularData(type = "occurrence")
  
})



test_that("List available files", {
  
  # List all available files on the S3 bucket
  tmpTable <- aws.s3::get_bucket_df(
    bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")))
  # unique(tmpTable$Key)
  
  # Bucket is not empty
  expect_gte(length(unique(tmpTable$Key)), 1)
  
  # Size of S3 files should be > 100
  expect_true(all(tmpTable$Size > 100), info = paste("Empty data files:", toString(unique(tmpTable$Key[tmpTable$Size < 100]))))
  
  #    # Read single file
  #    rawData <- readS3(FUN = read.csv, file = unique(tmpTable$Key)[1]) 
  #    expect_is(rawData, "data.frame")
  
})




test_that("Load data", {
  
  
    # Load latest dictionary
    keyData <- loadMetaData(type = "keys")
    
    # Load tabular data
    exotenData <- loadTabularData(type = "indicators")
    unionList <- loadTabularData(type = "unionlist")
    occurrenceData <- loadTabularData(type = "occurrence")
    
    # Translations
    loadMetaData(language = "nl")
    loadMetaData(language = "fr")
    loadMetaData(language = "en")
    
  })



test_that("Tools", {
    
    myFile <- nameFile(species = "abc", content = "test", fileExt = "csv")
    expect_type(myFile, "character")
    
    expect_warning({
        myName <- displayName(text = "test", translations = loadMetaData(language = "nl"))
        expect_type(myName, "character")
      })

    myString <- vectorToTitleString(1:10)
    expect_type(myString, "character")
    
    myYears <- yearToTitleString(year = 2002)
    expect_type(myYears, "character")
    
    myBullets <- drawBullet(color = "red")
    expect_type(myBullets, "list")
    
  })


test_that("S3 bucket connection", {
    awsFile <- "~/.aws/credentials"
    # credentials are in ~/.aws/credentials OR manually copy/paste OR using aws.signature::
    x <- rawToChar(readBin(awsFile, "raw", n = 1e5L))
#    profile <- Sys.getenv("AWS_PROFILE")
    profile <- "inbo-alien"
    credentials <- strsplit(x, profile)[[1]][2]
    
    Sys.setenv(
      AWS_DEFAULT_REGION = eval(parse(text = config::get("credentials", file = system.file("config.yml", package = "alienSpecies"))$region)),
      AWS_ACCESS_KEY_ID = strsplit(strsplit(credentials, "aws_access_key_id = ")[[1]][2], "\n")[[1]][1], 
      AWS_SECRET_ACCESS_KEY = strsplit(strsplit(credentials, "aws_secret_access_key = ")[[1]][2], "\n")[[1]][1],
      AWS_SESSION_TOKEN = strsplit(strsplit(credentials, "aws_session_token = ")[[1]][2], "\n")[[1]][1]
    )
    
   bucket <- "inbo-exotenportaal-uat-eu-west-1-default"
    
    # List all available files on the S3 bucket
    tmpTable <- aws.s3::get_bucket_df(bucket = bucket)
    expect_type(  tmpTable, "list")
    expect_gt(nrow(tmpTable), 1)
  })



bucket <- "inbo-exotenportaal-uat-eu-west-1-default"
tmpTable <- aws.s3::get_bucket_df(bucket = bucket)

# create test for each data file that is loaded in the app
# Most of these are listed in global.R
# Some specific data files are loaded for the management pages, see serverSpecies.R



test_that("Load shape data", {
  
  allShapes <- c(
    # Grid data
    loadShapeData("grid.RData"),
    loadShapeData("occurrenceCube.RData"),
    # gemeentes & provinces
    "provinces" = list(loadShapeData("provinces.RData")),
    "communes" = list(loadShapeData("communes.RData"))
  )
  
  expect_gt(length(  allShapes), 1)
  
 
  expect_setequal(
  c("gewestbel", "utm1_bel_with_regions", "utm10_bel_with_regions","be_10km", "be_1km","provinces","communes"  ) , names(allShapes)
)
 
  expect_true(all(unlist(lapply(allShapes, class)) %in% c("sf", "data.frame")))
  
  
  })



test_that("Load exotenData", {
  exotenData <- loadTabularData(type = "indicators")
  expect_s3_class(exotenData, "data.table")
})

test_that("Load unionlistData", {
  unionlistData <- loadTabularData(type = "unionlist")
  expect_s3_class(unionlistData, "data.table")
})

test_that("Load occurrenceData", {
  occurrenceData <- loadTabularData(type = "occurrence")
  expect_s3_class(occurrenceData, "data.table")
})

test_that("Load full_timeseries", {
  timeseries <- loadTabularData(type = "timeseries")
  expect_s3_class(timeseries, "data.table")
})


test_that("Load cube data", {
  occupancy <- loadOccupancyData()
  expect_true(exists("dfCube"))
  expect_s3_class(occupancy, "data.table")
})


test_that("Load Vespa_velutina_shape", {

    Vespa_velutina_shape <- loadShapeData("Vespa_velutina_shape.RData")
    expect_type(Vespa_velutina_shape, "list")

})


test_that("management data", {
  
expect_true(all(c("Oxyura_jamaicensis.csv",  "Lithobates_catesbeianus.csv", "Vespa_velutina_shape.RData", "Ondatra_zibethicus.csv") %in% tmpTable$Key))

})








