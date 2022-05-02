# Checklists page
# 
# Author: mvarewyck
###############################################################################

tagList(
  
  tags$div(class = "container",
    
    welcomeSectionUI(id = "checklist"),
    
    tags$h2("Keuze menu"),
    wellPanel(
      fixedRow(
        # Select time range
        column(6, uiOutput("exoten_timeOptions")),
        
        # Select regio
        column(6, uiOutput("exoten_regionOptions")),
      
      ),
      fixedRow(
        # show n
        column(4, textOutput("nrows"))
      
      )),
    wellPanel(
      fixedRow(
        # Select bron
        column(4, uiOutput("exoten_bronOptions")),
        
        # Select habitat
        column(4, uiOutput("exoten_habitatOptions")),
        
        # Selecct degree of establishment
        column(4, tagList(uiOutput("exoten_doeOptions"),
#                                  selectModuleUI(id = "kingdom")
          ))
      
      ),
      fixedRow(                
        # Select kingdom and other subsequent taxa choices
        column(4, tagList(uiOutput("exoten_kingdomOptions"),
            uiOutput("exoten_phylumOptions"),
            uiOutput("exoten_classOptions"),
            uiOutput("exoten_orderOptions"),
            uiOutput("exoten_familyOptions")
          )
        ),
        
        # Select pathway 1
        column(4, tagList(uiOutput("exoten_pw1Options"),
            uiOutput("exoten_pw2Options"))),
        
        # Select subsequent pathway 2
        column(4, uiOutput("exoten_unionlistOptions"))
      
      ),
      
      fixedRow(column(4, textOutput("nrowsFinal"))
      )
    ),
  
  
  
  ),
  
  
  tags$div(class = "container",
    
    tabsetPanel(
      
      tabPanel("Taxa", NULL),
      
#        countIntroductionPathwayUI(id = "checklist"),
      
      tabPanel("Trend", 
        plotTriasUI(id = "checklist-count"),
        plotTriasUI(id = "checklist-cum"),
        "TODO: Trend occupancy union-list species (Trias function?)"
      ),
      
      tabPanel("Pathways", 
        
        tableIntroductionUI(id = "checklist"),
        plotTriasUI(id = "checklist_pathway1"),
        plotTriasUI(id = "checklist_pathway1_trend"),
        plotTriasUI(id = "checklist_pathway2")
      
      ),
      
      tabPanel("Origin", 
        countYearNativerangeUI(id = "checklist")
      )
    )
  )

)
