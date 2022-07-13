# Tests to create and load the data
# 
# Author: mvarewyck
###############################################################################


test_that("Create data in tempdir", {
    
    skip("Takes long time to run")
    
    createKeyData(dataDir = tempdir())
    createTimeseries(packageDir = tempdir())
    createOccupancyCube(packageDir = tempdir())
    
  })


test_that("Load data", {
    
    # Load latest dictionary
    keyData <- loadMetaData(type = "keys")
    
    # Load exoten data
    expect_warning(loadTabularData(type = "indicators"))
    loadTabularData(type = "unionlist")
    expect_warning(loadTabularData(type = "occurrence"))
    
    # Translations
    loadMetaData(language = "nl")
    loadMetaData(language = "fr")
    loadMetaData(language = "en")
    
  })



test_that("Tools", {
    
    myFile <- nameFile(species = "abc", content = "test", fileExt = "csv")
    expect_type(myFile, "character")
    
    myName <- displayName(text = "test", translations = loadMetaData(language = "nl"))
    expect_type(myName, "character")

    myString <- vectorToTitleString(1:10)
    expect_type(myString, "character")
    
    myYears <- yearToTitleString(year = 2002)
    expect_type(myYears, "character")
    
    myBullets <- drawBullet(color = "red")
    expect_type(myBullets, "list")
    
  })

