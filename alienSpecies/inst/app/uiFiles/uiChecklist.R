# Checklists page
# 
# Author: mvarewyck
###############################################################################

tagList(
  
  tags$div(class = "container",
    
    welcomeSectionUI(id = "checklist"),
    
    wellPanel(
      
      # https://stackoverflow.com/a/60315446
#      uiOutput("filter_taxa"),
      selectizeInput(inputId = "exoten_taxa", label = NULL, choices = NULL, multiple = TRUE),
      
      fixedRow(        
        # Select habitat
        column(3, filterSelectUI(id = "habitat")),
        
        # Select pathway 1
        column(3, uiOutput("filter_pw")),
        
        # Select degree of establishment
        column(3, filterSelectUI(id = "doe")),
        
        column(3, uiOutput("filter_native"))
        
      ),
      
      actionLink("exoten_more", label = "More", icon = icon("angle-double-down")),
      
      conditionalPanel("input.exoten_more % 2 == 1", 
        
         fixedRow(
      
          # Select time range
          column(3, 
            tags$div(class = "selection-btn-wrapper",
              actionButton("exoten_timeButton", label = "All years",
                icon = icon("caret-down"), width = "100%"),
              tags$span(id = "time-popup"))
          ),
          
          # Select union list
          column(3, filterSelectUI(id = "union")),
          
          # Select regio
          column(3, filterSelectUI(id = "region")),
          
          # Select bron
          column(3, filterSelectUI(id = "source"))
        )
      
      ),
      textOutput("nrowsFinal")
    )
  
  ),
  
  
  tags$div(class = "container", style = "margin-bottom: 10px;",
    
    tabsetPanel(
      
      tabPanel(titleModuleUI(id = "checklist_taxa"), 
        
        tags$div(style = "margin-top: 10px;",
          
          uiOutput("exoten_legendLink"),
          conditionalPanel("input.exoten_legend % 2 == 1",
            wellPanel(
              uiOutput("exoten_legendText")              
            )
          )
        ),
        
        tableIndicatorsUI("checklist")
      
      ),
      
      tabPanel(titleModuleUI(id = "checklist_trend"), 
        plotTriasUI(id = "checklist-count"),
        plotTriasUI(id = "checklist-cum"),
        countOccupancyUI(id = "checklist")
      ),
      
      tabPanel(titleModuleUI(id = "checklist_pathways"), 
        
        plotTriasUI(id = "checklist_tablePathway", outputType = "table"),
        plotTriasUI(id = "checklist_pathway1"),
        plotTriasUI(id = "checklist_pathway1Trend"),
        plotTriasUI(id = "checklist_pathway2"),
        plotTriasUI(id = "checklist_pathway2Trend")
      
      ),
      
      tabPanel(titleModuleUI(id = "checklist_origin"),
        plotTriasUI(id = "checklist_yearNativeRange")
      )
    )
  )

)
