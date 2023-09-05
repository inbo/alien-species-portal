# Tests to create and load the data
# 
# Author: mvarewyck
###############################################################################


test_that("Create data in tempdir", {
    
    skip("Takes long time to run")
    
    createKeyData(dataDir = tempdir())
    createTimeseries(
      packageDir = tempdir(), 
      shapeData = readShapeData()$utm1_bel_with_regions
    )
    createOccupancyCube(packageDir = tempdir())
    
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
    
    skip("under development")
    
    awsFile <- "~/.aws/credentials"
    
    # credentials are in ~/.aws/credentials OR manually copy/paste OR using aws.signature::
    x <- rawToChar(readBin(awsFile, "raw", n = 1e5L))
#    profile <- Sys.getenv("AWS_PROFILE")
    profile <- "inbo-alien"
    credentials <- strsplit(x, profile)[[1]][2]
    
    Sys.setenv(
      AWS_DEFAULT_REGION = eval(parse(text = config::get("credentials", file = system.file("config.yml", package = "reportingGrofwild"))$region)),
      AWS_ACCESS_KEY_ID = strsplit(strsplit(credentials, "aws_access_key_id = ")[[1]][2], "\n")[[1]][1], 
      AWS_SECRET_ACCESS_KEY = strsplit(strsplit(credentials, "aws_secret_access_key = ")[[1]][2], "\n")[[1]][1],
      AWS_SESSION_TOKEN = strsplit(strsplit(credentials, "aws_session_token = ")[[1]][2], "\n")[[1]][1]
    )
    
#      bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")))
    bucket <- "inbo-exotenportaal-uat-eu-west-1-default"
    
    # List all available files on the S3 bucket
    tmpTable <- aws.s3::get_bucket_df(bucket = bucket)
    
    s3read_using(FUN = read.table, object = basename("myfile.txt"), bucket = bucket)
    put_object(file = "myfile.txt", object = "myfile.txt", bucket = bucket, multipart = TRUE)
    
  })

