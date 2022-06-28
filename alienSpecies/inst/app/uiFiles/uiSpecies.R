# Species page
# 
# Author: mvarewyck
###############################################################################


tagList(
  
  tags$div(class = "container",
    
    selectInput(inputId = "species_choice", label = NULL, 
      choices = NULL, width = "100%"),    
    
    tabsetPanel(
      
      tabPanel("Observations", 
        tags$div(style = "margin-top: 10px;",
          mapCubeUI(id = "observations", showPeriod = TRUE)
        )
      ),
      
      tabPanel("Indicators", 
        tags$div(style = "margin-top: 10px;",
          plotTriasUI(id = "species_emergenceObservations"),
        )
      ),
      
      tabPanel("Reporting", 
        tags$div(style = "margin-top: 10px;",
          mapCubeUI(id = "reporting_t1"),
          mapCubeUI(id = "reporting_t01"),
        )
      )
      

    )
  )
)