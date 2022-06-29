# Checklists page
# 
# Author: mvarewyck
###############################################################################

tagList(
  
  tags$div(class = "container",
    
    welcomeSectionUI(id = "checklist"),
    
    tags$h2("Keuze menu"),
    # TODO filter formatting: https://shiny.rstudio.com/articles/selectize.html
    
    wellPanel(
      fixedRow(
        
        # Select kingdom and other subsequent taxa choices
        column(2, tagList(uiOutput("exoten_kingdomOptions"),
            uiOutput("exoten_phylumOptions"),
            uiOutput("exoten_classOptions"),
            uiOutput("exoten_orderOptions"),
            uiOutput("exoten_familyOptions")
          )
        ),
        
        # Select habitat
        column(2, uiOutput("exoten_habitatOptions")),
        
        # Select pathway 1
        column(2, tagList(uiOutput("exoten_pw1Options"),
            uiOutput("exoten_pw2Options"))),
        
        # Select subsequent pathway 2
        column(2, uiOutput("exoten_unionlistOptions")),
        
        # Select degree of establishment
        column(2, tagList(uiOutput("exoten_doeOptions"),
#                                  selectModuleUI(id = "kingdom")
          )),
        
        column(2, uiOutput("exoten_country")),
        
        column(2, actionButton("exoten_more"))
      
      ),
      conditionalPanel("input.exoten_more % 2 == 1", 
        
        fixedRow(
          # Select time range
          column(4, uiOutput("exoten_timeOptions")),
          
          # Select regio
          column(2, uiOutput("exoten_regionOptions")),
          
          # Select bron
          column(2, uiOutput("exoten_bronOptions"))
        )
      
      ),
      fixedRow(column(4, textOutput("nrowsFinal")))
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
