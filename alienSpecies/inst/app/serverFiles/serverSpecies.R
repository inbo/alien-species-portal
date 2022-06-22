# Species page
# 
# Author: mvarewyck
###############################################################################


# Species selection
results$species_choices <- reactive({
    
    subData <- occurrenceData[!duplicated(taxonKey), ]
    choices <- subData$taxonKey
    names(choices) <- subData$scientificName
    
    choices[order(names(choices))]
    
  })


observe({
    
    # Trigger update when object is created
    input$tabs
    
    updateSelectizeInput(session = session, inputId = "species_taxonKey",
      choices = results$species_choices(),
      selected = as.character(results$species_choice),
      server = TRUE)    
    
  })



### Observations
### -----------------


## Map + barplot
mapOccurrenceServer(id = "observations",
  uiText = results$translations,
  taxonKey = reactive(results$species_choices()[results$species_choices() == req(input$species_taxonKey)]),
  taxData = occurrenceData,
  shapeData = allShapes
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


