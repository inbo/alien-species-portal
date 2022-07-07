# Species page
# 
# Author: mvarewyck
###############################################################################


tagList(
  
  tags$div(class = "container",
    
    selectInput(inputId = "species_choice", label = NULL, 
      choices = NULL, width = "100%"),    
    
    tabsetPanel(
      
      tabPanel(titleModuleUI(id = "species_observations"), 
        tags$div(style = "margin-top: 10px;",
          mapCubeUI(id = "observations", showPeriod = TRUE)
        )
      ),
      
      tabPanel(titleModuleUI(id = "species_indicators"), 
        tags$div(style = "margin-top: 10px;",
          plotTriasUI(id = "species_gam", filters = c("bias", "protected")),
        )
      ),
      
      tabPanel(titleModuleUI(id = "species_reporting"), 
        tags$div(style = "margin-top: 10px;",
          mapCubeUI(id = "reporting_t1"),
          mapCubeUI(id = "reporting_t01"),
        )
      ),
      
      tabPanel(titleModuleUI(id = "species_management"),
        tags$div(style = "margin-top: 10px;",
          mapCubeUI(id = "management", showPeriod = TRUE, showLegend = FALSE)
        )
      ),
      
      tabPanel(titleModuleUI(id = "species_more"), 
        tabsetPanel(
          tabPanel(titleModuleUI(id = "species_habitats")),
          tabPanel(titleModuleUI(id = "species_risk_maps")),
          tabPanel(titleModuleUI(id = "species_links")),
          tabPanel(titleModuleUI(id = "species_risk_assessment")),
          tabPanel(titleModuleUI(id = "species_images"))
          )
      )
    )
  )
)