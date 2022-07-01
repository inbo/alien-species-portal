# Checklists page
# 
# Author: mvarewyck
###############################################################################

tagList(
  
  tags$div(class = "container",
    
    welcomeSectionUI(id = "checklist"),
    
    tags$h2("Keuze menu"),
    
    wellPanel(
      
      # https://stackoverflow.com/a/60315446
      comboTreeInput("exoten_taxa", choices = taxaChoices),
      
      fixedRow(        
        # Select habitat
        column(3, uiOutput("exoten_habitat")),
        
        # Select pathway 1
        column(3, comboTreeInput("exoten_pw", choices = pwChoices,
            placeholder = "All pathways")),
        
        # TODO unionlistOptions?
#        column(3, uiOutput("exoten_unionlistOptions")),
        
        # Select degree of establishment
        column(3, selectInput("exoten_doe", label = NULL, 
            choices = c("All degree of establishment" = "", doeChoices), 
            multiple = TRUE)
          ),
        
        column(3, comboTreeInput("exoten_native", choices = nativeChoices,
            placeholder = "All native regions"))
        
      ),
        
        fixedRow(
        
          column(1, actionLink("exoten_more", label = "More", icon = icon("angle-double-right"))),
      
          conditionalPanel("input.exoten_more % 2 == 1", 
          # Select time range
          column(5, sliderInput(inputId = "exoten_time", label = NULL, 
              value = c(min(exotenData$first_observed, na.rm = TRUE), defaultYear),
              min = min(exotenData$first_observed, na.rm = TRUE),
              max = max(exotenData$first_observed, na.rm = TRUE),
              step = 1,
              sep = "")),
          
          # Select regio
          column(3, selectInput(inputId = "exoten_region", label = NULL,
              choices = c("All regions" = "", regionChoices),
              multiple = TRUE)),
          
          # Select bron
          column(3, selectInput("exoten_source", label = NULL, 
              choices = c("All sources" = "", bronChoices),
              multiple = TRUE))
        )
      
      ),
      textOutput("nrowsFinal")
    )
  
  ),
  
  
  tags$div(class = "container", style = "margin-bottom: 10px;",
    
    tabsetPanel(
      
      tabPanel(titleModuleUI(id = "checklist_taxa"), 
        
        tags$div(style = "margin-top: 10px;",
          
          actionLink(inputId = "exoten_legend", label = "Table Legend", icon = icon("angle-double-down")),
          conditionalPanel("input.exoten_legend % 2 == 1",
            wellPanel(
              tags$b("Icons"),
              p(icon("star"), "Species has at least 1 (historical) observation in Belgium"),
              p(icon("play"), "Species is on the EU-list of interest"),
              tags$b("Colors"),
              p(drawBullet(color = "black"), "Only observations"),
              p(drawBullet(color = "orange"), "Incomplete outputs"),
              p(drawBullet(color = "green"), "All outputs")
            )
          )
        ),
        
        tableIndicatorsUI("checklist")
      
      ),
      
#        countIntroductionPathwayUI(id = "checklist"),
      
      tabPanel(titleModuleUI(id = "checklist_trend"), 
        plotTriasUI(id = "checklist-count"),
        plotTriasUI(id = "checklist-cum"),
        countOccupancyUI(id = "checklist")
      ),
      
      tabPanel(titleModuleUI(id = "checklist_pathways"), 
        
        plotTriasUI(id = "checklist_tablePathway", outputType = "table"),
        plotTriasUI(id = "checklist_pathway1"),
        plotTriasUI(id = "checklist_pathway1Trend"),
        plotTriasUI(id = "checklist_pathway2")
      
      ),
      
      tabPanel(titleModuleUI(id = "checklist_origin"),
        countYearNativerangeUI(id = "checklist")
      )
    )
  )

)
