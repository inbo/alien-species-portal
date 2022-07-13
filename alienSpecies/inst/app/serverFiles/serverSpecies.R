# Species page
# 
# Author: mvarewyck
###############################################################################



output$species_title <- renderUI({
    
    results$translations$title[results$translations$plotFunction == tabChoices[3]]  
    
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
  baseMap = baseMap,
  showPeriod = TRUE
)




### Indicators
### -----------------


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
        name = input$species_choice
      )
    })
)


### Reporting
### -----------------

# t1
mapCubeServer(id = "reporting_t1",
  uiText = reactive(results$translations),
  species = reactive(input$species_choice),
  df = reactive(dfCube[dfCube$source == "t1" & species %in% input$species_choice, ]),
  groupVariable = "source",
  shapeData = allShapes,
  baseMap = baseMap
)

# t0 and t1
mapCubeServer(id = "reporting_t01",
  uiText = reactive(results$translations),
  species = reactive(input$species_choice),
  df = reactive(dfCube[species %in% input$species_choice, ]),
  groupVariable = "source",
  shapeData = allShapes,
  baseMap = baseMap
)

### Management
### ----------------

## Map + barplot
results$species_managementData <- reactive({
    
    req(input$species_choice)
    dataFile <- gsub(" ", "_", paste0(input$species_choice, ".csv"))
    validate(need(file.exists(file.path(managementDir, dataFile)), "No data available"))
    loadGbif(dataFile = dataFile)
    
  })


mapCubeServer(id = "management",
  uiText = reactive(results$translations),
  species = reactive(input$species_choice),
  df = results$species_managementData,
  filter = list(
    gender = unique(results$species_managementData()$gender), 
    samplingProtocol = unique(results$species_managementData()$samplingProtocol)
  ),
  groupVariable = NULL,
  shapeData = NULL,
  baseMap = baseMap,
  showPeriod = TRUE
)


