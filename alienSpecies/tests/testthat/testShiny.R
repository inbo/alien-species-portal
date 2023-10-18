# Test shiny modules
# 
# Author: mvarewyck
###############################################################################


# Helper data
exotenData <- loadTabularData(type = "indicators")
occurrenceData <- loadTabularData(type = "occurrence")


translations <- loadMetaData(language = "nl")


test_that("Module countOccupancy", {
    
    readS3(file = "dfCube.RData")
    occupancy <- loadOccupancyData(dfCube = dfCube)
    
    shiny::testServer(countOccupancyServer, 
      args = list(
        uiText = reactive(translations), 
        data = reactive(occupancy)
      ), {
        
        session$setInputs(linkOccupancy = 1)
        expect_true(TRUE)
        
      })
  })


test_that("Module mapCube", {
    
    mySpecies <- "Oxyura jamaicensis"
    
    allShapes <- readShapeData()
    
    dictionary <- loadMetaData(type = "keys")
    myKey <- dictionary$taxonKey[match(mySpecies, dictionary$scientificName)]
    
    shiny::testServer(mapCubeServer, 
      args = list(
        uiText = reactive(translations),
        species = reactive(mySpecies),
        df = reactive(occurrenceData[taxonKey %in% myKey, ]),
        groupVariable = "cell_code",
        shapeData = allShapes,
        showPeriod = TRUE
      ), {        
        session$setInputs(period = c(2002, 2020))
        expect_true(!is.null(output$region))        
      })
  })



test_that("Module plotTrias", {
    shiny::testServer(plotTriasServer, 
      args = list(
        uiText = reactive(translations),
        data = reactive(exotenData),
        triasFunction = "visualize_pathways_level1"
      ), {
        
        session$setInputs(linkPlotTrias = 1)
        expect_true(TRUE)
        # Not expecting output because nested module for plot/table
        
      })
  })


test_that("Module tableIndicators", {
    
    unionlistData <- loadTabularData(type = "unionlist")
    
    shiny::testServer(tableIndicatorsServer, 
      args = list(
        exotenData = reactive(exotenData),
        unionlistData = unionlistData,
        occurrenceData = occurrenceData,
        uiText = reactive(translations)
      ), {
        
        session$setInputs(union = 1)
        expect_true(!is.null(output$table))
        
      })
  })


test_that("Module plotModule", {
    shiny::testServer(plotModuleServer, 
      args = list(
        plotFunction = "plotTrias", 
        triasFunction = "indicator_introduction_year",
        data = reactive(exotenData), 
        triasArgs = reactive(list(
            start_year_plot = 2002,
            x_lab = "Jaar",
            y_lab = "Aantal ge\u00EFntroduceerde uitheemse soorten")
        )
      ), {
        
        expect_true(!is.null(output$plot))
        
      })
  })


test_that("Module titleModule", {
    shiny::testServer(titleModuleServer, 
      args = list(
        plotFunction = "visualize_pathways_year_level1",
        uiText = reactive(translations)), 
      {
        
        expect_true(!is.null(output$title))
        
      })
  })


test_that("Module welcomeSection", {
    shiny::testServer(welcomeSectionServer, 
      args = list(
        uiText = reactive(translations)
      ), {
        
        expect_true(!is.null(output$welcomeTitle))
        
      })
  })



test_that("Module filterSelect", {
    shiny::testServer(filterSelectServer, 
      args = list(
        url = reactive(list()),
        initChoices = attr(exotenData, "habitats"),
        translations = reactive(translations)
      ), {
        
        expect_true(!is.null(output$filter))
        
      })
  })