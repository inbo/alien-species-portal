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
    
    translate(data = results$translations, id = tabChoices[3])  
    
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
  baseMap = baseMap,
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
        x_label = translate(results$translations, "year"),
        y_label = translate(results$translations, "observations")
      )
    }),
  filters = c("bias", "protected")
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

# Species for which to show mapCube output
# Other species will have mapRegions output
cubeSpecies <- c("Oxyura jamaicensis")


results$species_managementFile <- reactive({
    
    req(input$species_choice)
    dataFile <- gsub(" ", "_", paste0(input$species_choice, ".csv"))
    if (file.exists(file.path(managementDir, dataFile)))
      dataFile else
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
    
    validate(need(results$species_managementFile(), translate(results$translations, "noData")))
    loadGbif(dataFile = results$species_managementFile())
    
  })


## Map + barplot
observe({
    
    req(results$species_managementData())
    
    if (input$species_choice %in% cubeSpecies) {
      
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
        baseMap = baseMap,
        showPeriod = TRUE
      )
      
    } else {
      
      mapRegionsServer(
        id = "management",
        uiText = reactive(results$translations),
        species = reactive(input$species_choice),
        df = results$species_managementData,
        occurrenceData = occurrenceData,
        shapeData = allShapes
        )
      countYearGroupServer(
        id = "management", 
        uiText = reactive(results$translations), 
        data = results$species_managementData
      )
    } 
    
  })

observeEvent(input$species_choice, shinyjs::reset("species_managementContent"))
  
output$species_managementContent <- renderUI({
    
    req(results$species_managementData())
    
    if (input$species_choice %in% cubeSpecies) {
      mapCubeUI(id = "management", showPeriod = TRUE, showLegend = FALSE)
    } else {
      tagList(
        mapRegionsUI(id = "management"),
        countYearGroupUI(id = "management")
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

