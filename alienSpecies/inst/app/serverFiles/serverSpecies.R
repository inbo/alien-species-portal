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

