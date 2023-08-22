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
    dataFile <- gsub(" ", "_", paste0(input$species_choice, ".csv"))
    if (file.exists(file.path(managementDir, dataFile)))
      dataFile else if (input$species_choice %in% heatSpecies)
      # path to folder
      gsub(" ", "_", input$species_choice) else
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
    
    if (input$species_choice %in% heatSpecies) {
      
      validate(need(!is.null(list.files(results$species_managementFile())), 
          translate(results$translations, "noData")$title))
      
      readShapeData(
        extension = ".geojson", 
        dataDir = file.path(system.file("extdata", "management", package = "alienSpecies"), "Vespa_velutina")
      )
      
      
    } else {
      
      validate(need(results$species_managementFile(), translate(results$translations, "noData")$title))
      
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
      ## Heat map + diagnostics
      
      mapHeatServer(id = "management2",
        uiText = reactive(results$translations),
        species = reactive(input$species_choice),
        activeData = reactive(results$species_managementData()$actieve_haarden),
        managedData = reactive(results$species_managementData()$beheerde_nesten), 
        untreatedData = reactive(results$species_managementData()$onbehandelde_nesten),
        filter = reactive(list(nest = c("managed nest", "untreated nest")))
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
      
      mapHeatUI(id = "management2")
      
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

