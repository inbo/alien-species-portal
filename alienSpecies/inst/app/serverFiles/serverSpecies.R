# Species page
# 
# Author: mvarewyck
###############################################################################



output$species_title <- renderUI({
    
    results$translations$title[results$translations$plotFunction == tabChoices[3]]  
    
  })


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
  species = reactive(names(taxonKey())),
  df = reactive({
      req(taxonKey())
      occurrenceData[occurrenceData$taxonKey %in% taxonKey(), ]
    }),
  groupVariable = "cell_code",
  shapeData = allShapes,
  baseMap = baseMap,
  showPeriod = TRUE
)




### Indicators
### -----------------


## Emergence status GAM - Observations
plotTriasServer(id = "species_emergenceObservations",
  data = results$exoten_data,
  uiText = reactive(results$translations),
  triasFunction = "indicator_introduction_year",
  triasArgs = reactive({
      list(
        start_year_plot = min(results$exoten_data()$first_observed, na.rm = TRUE) - 1,
        x_lab = "Jaar",
        y_lab = "Aantal ge\u00EFntroduceerde uitheemse soorten"
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



