# Species page
# 
# Author: mvarewyck
###############################################################################


tagList(

  tags$div(class = "container",
    tags$div(class = "jumbotron",
      
      welcomeSectionUI(id = "species"),
      fixedRow(
        column(6,
          selectInput(inputId = "species_choice", label = NULL, choices = NULL,
            width = "100%")),
        column(6,
          selectInput(inputId = "species_gewest", label = NULL,
            choices = NULL, multiple = TRUE, width = "100%"))
      )
    )
  ),
  
  tags$div(class = "container",

    tabsetPanel(id = "species_tabs",
      
      tabPanel(titleModuleUI(id = "species_observations"),
        value = "species_observations", 
        tags$div(style = "margin-top: 10px;",
          mapCubeUI(id = "observations", showPeriod = TRUE)
        )
      ),
      
      tabPanel(titleModuleUI(id = "species_indicators"),
        value = "species_indicators",
        tags$div(style = "margin-top: 10px;",
          plotTriasUI(id = "species_gam", showPlotDefault = TRUE),
        )
      ),
      
      tabPanel(titleModuleUI(id = "species_reporting"), 
        value = "species_reporting",
        tags$div(style = "margin-top: 10px;",
          mapCubeUI(id = "reporting_t01"),
        )
      ),
      
      tabPanel(titleModuleUI(id = "species_management"),
        value = "species_management",
        tags$div(style = "margin-top: 10px;",
          uiOutput("species_managementContent")
        )
      ),
      
      tabPanel(titleModuleUI(id = "species_more"), 
        value = "species_more",
        tabsetPanel(id = "species_more",
          tabPanel(titleModuleUI(id = "species_habitats"), value = "species_habitats"),
          tabPanel(titleModuleUI(id = "species_risk_maps"), value = "species_risk_maps",
            mapRasterUI("risk")
          ),
          tabPanel(titleModuleUI(id = "species_links"), value = "species_links"),
          tabPanel(titleModuleUI(id = "species_risk_assessment"), value = "species_risk_management"),
          tabPanel(titleModuleUI(id = "species_images"), value = "species_images")
          )
      )
    ),
    
    tags$div(style = "margin-bottom: 70px;"),
    
    tags$div(class = "footer",
      tags$div(class = "footer-content",
        singleton(
          tags$head(tags$script(src = "triggerDownload.js"))
        ),
        actionButton(inputId = "species_createReport", label = "Create report", 
          icon = icon("file-pdf")),
        downloadLink("species_downloadReport", " ", class = "invisible")
      )
    )
  )
)