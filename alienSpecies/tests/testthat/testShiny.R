# Test shiny modules
# 
# Author: mvarewyck
###############################################################################


# Helper data
exotenData <- loadTabularData(type = "indicators")
occurrenceData <- loadTabularData(type = "occurrence")
translationsEn <- loadMetaData(language = "en")
Vespa_velutina_shape <- loadShapeData("Vespa_velutina_shape.RData")

allShapes <- c(
  # Grid data
  
  loadShapeData("grid.RData"),
  loadShapeData("occurrenceCube.RData"),
  # gemeentes & provinces
  "provinces" = list(loadShapeData("provinces.RData")),
  "communes" = list(loadShapeData("communes.RData"))
  
)

translations <- loadMetaData(language = "nl")


test_that("Module countOccupancy", {
    
    readS3(file = "dfCube.RData")
    occupancy <- loadOccupancyData()
    
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



#########################
#test management modules
#########################





test_that("Module mapHeat",{
  
  
  combinedActive <- combineActiveData(
    activeData = Vespa_velutina_shape$actieve_haarden,
    managedData = Vespa_velutina_shape$beheerde_nesten,
    untreatedData = Vespa_velutina_shape$onbehandelde_nesten
  )
  colorsActive <- c("blue", "black", "red")
  names(colorsActive) <- c("individual", "managed nest", "untreated nest")
  
  shiny::testServer(mapHeatServer,
                    args = list(
                      uiText = reactive(translationsEn),
                      species = reactive(  "Vespa velutina"),
                      combinedData = reactive(combinedActive),
                      filter = reactive(list(nest = unique(combinedActive$filter), radius = na.omit(unique(combinedActive$radius)))),
                      colors = reactive(colorsActive),
                      blur = "individual",
                      maxDate = reactive(max( Vespa_velutina_shape$actieve_haarden$eventDate, na.rm = TRUE))
                      
                    ), {
                      session$setInputs(globe = 2)
                      expect_true(!is.null(output$spacePlot))
                      
                    })
})

# 
# 
test_that("Module mapRegions",{

  #skip("WIP")
  vespaPoints <- Vespa_velutina_shape$points
  vespaPoints$type <- "individual"
  # Columns
  regionVariables <- list(level3Name = "NAAM", level2Name = "provincie", level1Name = "GEWEST")
  for (iName in names(regionVariables))
    names(vespaPoints)[match(iName, names(vespaPoints))] <- regionVariables[[iName]]
  # Gewest
  vespaPoints$GEWEST <- ifelse(vespaPoints$GEWEST == "Vlaanderen", "flanders", 
                               ifelse(vespaPoints$GEWEST == "Bruxelles", "brussels", 
                                      ifelse(vespaPoints$GEWEST == "Wallonie", "wallonia", "")))
  # Provincie
  vespaPoints$provincie <- ifelse(vespaPoints$provincie == "Vlaams Brabant", "Vlaams-Brabant",
                                  ifelse(vespaPoints$provincie == "Bruxelles", "HoofdstedelijkGewest", 
                                         ifelse(vespaPoints$provincie == "Liège", "Luik", 
                                                ifelse(vespaPoints$provincie == "Brabant Wallon", "Waals-Brabant",
                                                       ifelse(vespaPoints$provincie == "Hainaut", "Henegouwen", vespaPoints$provincie)))))
  vespaPoints$nest_type <- "individual"
  vespaPoints$isBeheerd <- FALSE
  
  ## Nest data
  vespaNesten <-  Vespa_velutina_shape$nesten
  vespaNesten$type <- "nest"
  vespaNesten$isBeheerd <- vespaNesten$geometry %in%  Vespa_velutina_shape$beheerde_nesten$geometry
  
  keepColumns <- c("year", "type", "nest_type", "NAAM", "provincie", "GEWEST", "isBeheerd", "geometry")
  vespaBoth <- rbind(vespaPoints[, keepColumns], vespaNesten[, keepColumns])
  vespaBoth$nest_type[vespaBoth$nest_type %in% c("NA", "NULL")] <- NA 
  
  
  shiny::testServer(	mapRegionsServer,
                    args = list(
                      uiText = reactive(translationsEn),
                      species = reactive( "Vespa velutina"),
                      df = reactive({
                        vespaBoth
                        
                      }),
                      occurrenceData = NULL,
                      shapeData = allShapes,
                      sourceChoices = c("individual", "nest")
                    ), {
                      session$setInputs(globe = 2)
                      session$setInputs(year = "2018")
                      session$setInputs(unit = NULL)
                      session$setInputs(combine = FALSE)
                      session$setInputs(period = c(2017,2020))
                      session$setInputs(regionLevel = "communes")
                      session$setInputs(gewestLevel = c("flanders", "brussels", "wallonia"))
                      session$setInputs(bronMap = c("individual", "nest"))
                      expect_true(!is.null(subData ))
                      expect_true(!is.null(output$bronMap ))
                      expect_true(!is.null(summaryData()))
                      expect_true(!is.null(noData()))
                     # expect_true(!is.null(output$regionsPlot ))

                    })
})


test_that("Module countNesten",{
  
  shiny::testServer(		
    countNestenServer ,
                    args = list(
                      data = reactive(Vespa_velutina_shape$nesten),
                      uiText = reactive(translationsEn),
                      maxDate = reactive(max(Vespa_velutina_shape$nesten$observation_time, na.rm = TRUE))
                    ), {
                      session$setInputs(linkCountNesten = 1)
                      #session$setInputs(linkPlotTrias = 1)
                      session$setInputs(period = c(2017,2023))
                      session$setInputs(regionLevel = "communes")
                      session$setInputs( typeNesten = "individual")
                      expect_true(!is.null(output$descriptionCountNesten ))
                      expect_true(!is.null(output$titleCountNesten))
                      expect_true(!is.null(output$period))
                      expect_true(!is.null(output$typeNesten))
                      expect_true(!is.null(plotData()))
                    
                    })
})


context("Test Shiny Apps")


test_that("App does not crash on startup", {   
  
shiny::testServer(
    app = system.file("app/", package = "alienSpecies"),
    expr = testthat::expect_true(TRUE)
  )
  
})


test_that("Test error data load App", {   
 
  expect_type(config::get("datacheck", file = system.file("config.yml", package = "alienSpecies")),
            "logical")

  
  shiny::testServer(
    app = tryCatch(
        stop("simulated error"),
        error = function(err)
        shinyApp(ui = fluidPage(
            tags$h3("Error during Data Check"),
            HTML(err$message)
        ), server = function(input, output, session){})
    ),
    expr = testthat::expect_true(TRUE)
  )
  
})






