# Species page
# 
# Author: mvarewyck
###############################################################################


tagList(
  
  tags$div(class = "container",
    
    uiOutput("species_choice"),
    
    tabsetPanel(
      
      tabPanel("Observations", 
        tags$div(style = "margin-top: 10px;",
          mapOccurrenceUI(id = "observations")
        )
      )

    )
  )
)