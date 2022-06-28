# Species page
# 
# Author: mvarewyck
###############################################################################


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
  uiText = results$translations,
  species = reactive(names(taxonKey())),
  df = reactive(occurrenceData[occurrenceData$taxonKey %in% taxonKey(), ]),
  groupVariable = "cell_code",
  shapeData = allShapes,
  showPeriod = TRUE
)




### Indicators
### -----------------


## Emergence status GAM - Observations
plotTriasServer(id = "species_emergenceObservations",
  data = results$exoten_data,
  uiText = results$translations,
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
  uiText = results$translations,
  species = reactive(input$species_choice),
  df = reactive(dfCube[dfCube$source == "t1" & species %in% input$species_choice, ]),
  groupVariable = "source",
  shapeData = allShapes
)

# t0 and t1
mapCubeServer(id = "reporting_t01",
  uiText = results$translations,
  species = reactive(input$species_choice),
  df = reactive(dfCube[species %in% input$species_choice, ]),
  groupVariable = "source",
  shapeData = allShapes
)



