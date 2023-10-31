# Species page
# 
# Example species
# observations: Saponaria officinalis
# reporting: Orconectes limosus
# management: Oxyura jamaicensis
# 
# Author: mvarewyck
###############################################################################



output$species_title <- renderUI({
    
    translate(data = results$translations, id = tabChoices[3])$title  
    
  })

lapply(c("observations", "indicators", "reporting", "management", "more",
    "habitats", "risk_maps", "links", "risk_assessment", "images"), function(iName)
    titleModuleServer(
      id = paste0("species_", iName),
      uiText = reactive(results$translations),
      plotFunction = iName
    ))


# Species selection
results$species_choices <- reactive({
    
    # Observations
    taxChoices <- occurrenceData[!duplicated(taxonKey), scientificName]
    # Reporting
    reportChoices <- dfCube[!duplicated(species) & !species %in% taxChoices, species]
    
    sort(c(taxChoices, reportChoices))
        
  })


observe({
    
    # Trigger update when changing tab
    input$tabs
    
    updateSelectizeInput(session = session, inputId = "species_choice",
      choices = results$species_choices(),
      selected = as.character(results$species_choice),
      server = TRUE)    
    
  })



### Observations
### -----------------

# Taxonkey of selected species
taxonKey <- reactive({
    
    dictionary$taxonKey[match(req(input$species_choice), dictionary$scientificName)]
    
  })

# Disable tab if no info
observe({
    
    # https://stackoverflow.com/a/64324799
    shinyjs::toggleState(
      selector = '#species_tabs a[data-value="species_observations"', 
      condition = !is.na(taxonKey())
    )
    
    if (is.na(taxonKey()) & input$species_tabs == "species_observations")
      updateTabsetPanel(session = session, inputId = "species_tabs", 
        selected = "species_reporting")
    
  })


## Map + barplot
mapCubeServer(id = "observations",
  uiText = reactive(results$translations),
  species = reactive(input$species_choice),
  df = reactive({
      req(taxonKey())
      occurrenceData[taxonKey %in% taxonKey(), ]      
    }),
  groupVariable = "cell_code",
  shapeData = allShapes,
  showPeriod = TRUE
)




### Indicators
### -----------------


# Disable tab if no info
observe({
    
    # https://stackoverflow.com/a/64324799
    shinyjs::toggleState(
      selector = '#species_tabs a[data-value="species_indicators"', 
      condition = !is.na(taxonKey())
    )
    
    if (is.na(taxonKey()) & input$species_tabs == "species_indicators")
      updateTabsetPanel(session = session, inputId = "species_tabs", 
        selected = "species_reporting")
    
  })


## Emergence status GAM - Observations
plotTriasServer(id = "species_gam",
  uiText = reactive(results$translations),
  data = reactive({
      req(taxonKey())
      timeseries[taxonKey %in% taxonKey(), ]
    }),
  triasFunction = "apply_gam",
  triasArgs = reactive({
      list(
        y_var = "obs", 
        taxon_key = taxonKey(), 
        name = input$species_choice,
        x_label = translate(results$translations, "year")$title,
        y_label = translate(results$translations, "observations")$title
      )
    }),
  filters = c("bias", "protected"),
  filterRegion = TRUE
)


### Reporting
### -----------------


# Disable tab if no info
observe({
    
    req(input$species_choice)
    
    # https://stackoverflow.com/a/64324799
    shinyjs::toggleState(
      selector = '#species_tabs a[data-value="species_reporting"', 
      condition = input$species_choice %in% dfCube$species
    )
    
    if (!(input$species_choice %in% dfCube$species) & input$species_tabs == "species_reporting")
      updateTabsetPanel(session = session, inputId = "species_tabs", 
        selected = "species_observations")
    
  })


# t0 and t1
mapCubeServer(id = "reporting_t01",
  uiText = reactive(results$translations),
  species = reactive(input$species_choice),
  df = reactive(dfCube[species %in% input$species_choice, ]),
  filter = reactive(list(source = unique(dfCube$source[dfCube$species %in% input$species_choice]))),
  groupVariable = "source",
  shapeData = allShapes
)



### Management
### ----------------

# Species for which to show mapCube output
cubeSpecies <- c("Oxyura jamaicensis")
# Species for whichto show heatMap output
heatSpecies <- c("Vespa velutina")
# Other species will have mapRegions output

results$species_managementFile <- reactive({
    
    req(input$species_choice)
    expectFile <- if (input$species_choice %in% heatSpecies)
        paste0(gsub(" ", "_", input$species_choice), "_shape.RData") else 
        gsub(" ", "_", paste0(input$species_choice, ".csv"))
    availableFiles <- aws.s3::get_bucket_df(
      bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")))$Key
    
    if (expectFile %in% availableFiles)
      expectFile else 
      NULL
    
  })

# Disable tab if no info
observe({
    
    req(input$species_choice)
    
    # https://stackoverflow.com/a/64324799
    shinyjs::toggleState(
      selector = '#species_tabs a[data-value="species_management"', 
      condition = !is.null(results$species_managementFile())
    )
    
    if (is.null(results$species_managementFile()) & input$species_tabs == "species_management")
      updateTabsetPanel(session = session, inputId = "species_tabs", 
        selected = "species_observations")
    
  })

results$species_managementData <- reactive({
    
    req(input$species_choice)
    
    validate(need(results$species_managementFile(), translate(results$translations, "noData")$title))
    
    if (input$species_choice %in% heatSpecies) {
      
      readS3(file = results$species_managementFile())
            
      base::get(paste0(gsub(" ", "_", heatSpecies), "_shape"))
            
    } else {
      
      tmpData <- loadGbif(dataFile = results$species_managementFile())
      tmpData$GEWEST <- allShapes$communes$GEWEST[
        match(tmpData$NISCODE, allShapes$communes$NISCODE)]
      tmpData
      
    }
    
  })


observe({
    
    req(results$species_managementData())
    
    if (input$species_choice %in% cubeSpecies) {
      ## Map + slider barplot
      
      mapCubeServer(id = "management",
        uiText = reactive(results$translations),
        species = reactive(input$species_choice),
        df = results$species_managementData,
        filter = reactive({
            filterCandidates <- c("gender", "samplingProtocol", "lifeStage")
            filters <- filterCandidates[filterCandidates %in% colnames(results$species_managementData())]
            sapply(filters, function(iFilter)
                sort(unique(results$species_managementData()[[iFilter]])),
              simplify = FALSE)
          }),
        groupVariable = NULL,
        shapeData = NULL,
        showPeriod = TRUE
      )
      
    } else if (input$species_choice %in% heatSpecies) {
      ## heatmap
      
      ## Actieve haarden
      combinedActive <- combineActiveData(
        activeData = results$species_managementData()$actieve_haarden,
        managedData = results$species_managementData()$beheerde_nesten,
        untreatedData = results$species_managementData()$onbehandelde_nesten
      )
      colorsActive <- c("blue", "black", "red")
      names(colorsActive) <- c("individual", "managed nest", "untreated nest")
      
      mapHeatServer(id = "management2_active",
        uiText = reactive(results$translations),
        species = reactive(input$species_choice),
        combinedData = reactive(combinedActive),
        filter = reactive(list(nest = unique(combinedActive$filter))),
        colors = reactive(colorsActive),
        blur = "individual",
        maxDate = reactive(max(results$species_managementData()$actieve_haarden$eventDate, na.rm = TRUE))      
      )
      
      ## Alle observaties
      combinedObserved <- combineNestenData(
        pointsData = results$species_managementData()$points, 
        nestenData = results$species_managementData()$nesten
      )
      colorsObserved <- c("blue", "red")
      names(colorsObserved) <- c("individual", "nest")
      
      mapHeatServer(id = "management2_observed",
        uiText = reactive(results$translations),
        species = reactive(input$species_choice),
        combinedData = reactive(combinedObserved),
        filter = reactive(list(source = unique(combinedObserved$filter))),
        colors = reactive(colorsObserved),
        maxDate = reactive(max(results$species_managementData()$points$eventDate, na.rm = TRUE))      
      )
      
      # Trend region
      mapRegionsServer(
        id = "management2",
        uiText = reactive(results$translations),
        species = reactive(input$species_choice),
        df = reactive({
            
            ## Individual data
            vespaPoints <- results$species_managementData()$points
            req(vespaPoints)
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
            vespaPoints
            
            ## Nest data
            vespaNesten <- results$species_managementData()$nesten
            vespaNesten$type <- "nest"
                        
            keepColumns <- c("year", "type", "NAAM", "provincie", "GEWEST", "geometry")
            rbind(vespaPoints[, keepColumns], vespaNesten[, keepColumns])
            
          }),
        occurrenceData = NULL,
        shapeData = allShapes,
        sourceChoices = c("individual", "nest")
      )
      
      # Aantal lente nesten
      plotTriasServer(
        id = "management2_lente",
        triasFunction = "barplotLenteNesten",
        data = reactive(s3read_using(FUN = read.csv, object = "aantal_lente_nesten.csv", bucket = bucket)),
          #read.csv(system.file("extdata", "management", "Vespa_velutina", "aantal_lente_nesten.csv", package = "alienSpecies"))
        uiText = reactive(results$translations)
      )
      
     
      # Aantal nesten per provincie - figuur
      plotTriasServer(
        id = "management2_province",
        triasFunction = "countNesten",
        data = reactive(results$species_managementData()$nesten),
        uiText = reactive(results$translations),
        maxDate = reactive(max(results$species_managementData()$nesten$observation_time, na.rm = TRUE))
      )
      
      # Aantal nesten per provincie - tabel
      plotTriasServer(
        id = "management2_provinceTable",
        triasFunction = "tableNesten",
        data = reactive(results$species_managementData()$nesten),
        uiText = reactive(results$translations),
        maxDate = reactive(max(results$species_managementData()$nesten$observation_time, na.rm = TRUE)),
        outputType = "table"
      )
      
      
    } else {
      ## Map + choices barplot
      
      mapRegionsServer(
        id = "management3",
        uiText = reactive(results$translations),
        species = reactive(input$species_choice),
        df = results$species_managementData,
        occurrenceData = occurrenceData,
        shapeData = allShapes
      )
      countYearGroupServer(
        id = "management3", 
        uiText = reactive(results$translations), 
        data = results$species_managementData
      )
    } 
    
  })

output$species_managementContent <- renderUI({
    
    req(results$species_managementData())
    
    # Important: different ids needed, otherwise there is communication between both cases
    # e.g. input$legend exists for both
    if (input$species_choice %in% cubeSpecies) {
      
      mapCubeUI(id = "management", showPeriod = TRUE, showLegend = FALSE)
      
    } else if (input$species_choice %in% heatSpecies) {
      
      tagList(
        tags$a(href = "https://vespawatch.be/", target = "_blank",
          tags$img(src = 'logo_vespawatch.png', height = 50)),
        mapHeatUI(id = "management2_active"),
        mapHeatUI(id = "management2_observed"),
        mapRegionsUI(id = "management2", plotDetails = c("flanders", "region"), showUnit = FALSE),
        plotTriasUI(id = "management2_lente"),
        plotTriasUI(id = "management2_province"),
        plotTriasUI(id = "management2_provinceTable", outputType = "table")
      )
      
    } else {
      
      tagList(
        mapRegionsUI(id = "management3", plotDetails = c("flanders", "region")),
        countYearGroupUI(id = "management3")
      )
      
    }
    
  })

### More
### ----------------

# Disable tab if no info
observe({
    
    req(input$species_choice)
    
    # https://stackoverflow.com/a/64324799
    shinyjs::toggleState(
      selector = '#species_tabs a[data-value="species_more"', 
      condition = FALSE
    )
    
  })

