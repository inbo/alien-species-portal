# Species page
# 
# Author: mvarewyck
###############################################################################


tagList(
  
  tags$div(class = "container",
    
    selectInput(inputId = "species_taxonKey", label = NULL, 
      choices = NULL, width = "100%"),    
    
    tabsetPanel(
      
      tabPanel("Observations", 
        tags$div(style = "margin-top: 10px;",
          mapOccurrenceUI(id = "observations")
        )
      ),
      
      tabPanel("Indicators", 
        tags$div(style = "margin-top: 10px;",
          plotTriasUI(id = "species_emergenceObservations"),
        )
      )
      

    )
  )
)