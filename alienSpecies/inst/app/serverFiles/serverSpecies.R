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


output$species_choice <- renderUI({
    
    selectInput(inputId = "species_taxonKey", label = NULL, 
      choices = results$species_choices(),
      selected = if (doDebug) c(2498252, 2769766)[1] else NULL)    
    
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

