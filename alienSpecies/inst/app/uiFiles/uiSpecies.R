# Species page
# 
# Author: mvarewyck
###############################################################################

tagList(
  
  # 1. Curved Woodland Hero Banner + Floating Filter Card
  tags$div(class = "species-hero-wrapper",
           tags$div(class = "species-hero-banner",
                    tags$div(class = "container",
                             welcomeSectionUI(id = "species")
                    )
           ),
           tags$div(class = "container",
                    tags$div(class = "species-filter-card",
                             selectizeInput(
                               inputId = "species_choice",
                               label = NULL,
                               choices = NULL,
                               width = "100%",
                               options = list(
                                 placeholder = switch(
                                   results$language,
                                   "nl" = "Selecteer een soort",
                                   "fr" = "Sélectionnez une espèce",
                                   "Select a species"
                                 )
                               )
                             ),
                             checkboxInput(inputId = "species_searchVernacular", label = translate("vernacular_name")$title),
                             selectInput(
                               inputId = "species_gewest",
                               label = NULL,
                               choices = setNames(
                                 c("flanders", "wallonia", "brussels"),
                                 translate(c("flanders", "wallonia", "brussels"))$title
                               ),
                               selected = c("flanders", "wallonia", "brussels"),
                               multiple = TRUE,
                               width = "100%"
                             ),
                             uiOutput("missingFilters_message")
                    ),
                    # Disclaimer
                    uiOutput("species_disclaimer")
           )
  ),
  
  # 2. Main Workspace: Left Species Card (Photo + GRIIS) + Right Sub-tabs
  tags$div(class = "container species-workspace",
           
           tags$div(class = "species-workspace-flex",
                    
                    # Left Sidebar Card (always visible so layout never jumps)
                    tags$div(class = "species-sidebar-wrapper",
                             tags$div(class = "species-profile-card",
                                      uiOutput("species_photo_box"),
                                      tags$div(class = "species-griis-summary",
                                               griisUI("griis"),
                                               tags$script(HTML("
                                                 (function() {
                                                   var labelMap = {
                                                     // English
                                                     'GBIF Taxonomic ID (Taxonkey)': 'GBIF taxonkey',
                                                     'Year of First Observation': 'First observation',
                                                     'Year of Last Observation': 'Last observation',
                                                     'Degree of Establishment': 'Establishment',
                                                     // Dutch
                                                     'GBIF Taxonomische ID (Taxonkey)': 'GBIF taxonkey',
                                                     'Jaar van eerste waarneming': 'Eerste waarneming',
                                                     'Jaar van laatste waarneming': 'Laatste waarneming',
                                                     'Mate van vestiging': 'Vestiging',
                                                     // French
                                                     'Identifiant taxonomique GBIF (Taxonkey)': 'Taxonkey GBIF',
                                                     'Année de première observation': 'Première observation',
                                                     'Année de dernière observation': 'Dernière observation',
                                                     'Degré d\\'établissement': 'Établissement'
                                                   };
                                                   function shortenGriisLabels(container) {
                                                     container.querySelectorAll('table tbody td:first-child').forEach(function(td) {
                                                       var txt = td.textContent.trim();
                                                       if (labelMap[txt]) {
                                                         td.textContent = labelMap[txt];
                                                       }
                                                     });
                                                   }
                                                   var target = document.querySelector('.species-griis-summary');
                                                   if (target) {
                                                     new MutationObserver(function() { shortenGriisLabels(target); })
                                                       .observe(target, { childList: true, subtree: true });
                                                   }
                                                 })();
                                               "))
                                      )
                             )
                    ),
                    
                    # Right Content Area
                    tags$div(
                      id = "species_main_col",
                      class = "species-main-content",
                      
                      tabsetPanel(id = "species_tabs",
                                  
                                  tabPanel(titleModuleUI(id = "species_observations"),
                                           value = "species_observations", 
                                           tags$div(style = "margin-top: 15px;",
                                                    mapCubeUI(id = "observations", showPeriod = TRUE)
                                           )
                                  ),
                                  
                                  tabPanel(titleModuleUI(id = "species_indicators"),
                                           value = "species_indicators",
                                           tags$div(style = "margin-top: 15px;",
                                                    plotTriasUI(id = "indicators_gamObservations", showPlotDefault = TRUE),
                                                    plotTriasUI(id = "indicators_gamOccupancy", showPlotDefault = TRUE),
                                                    mapRegionsUI(id = "indicators_facet", showUnit = FALSE, facet = TRUE)
                                           )
                                  ),
                                  
                                  tabPanel(titleModuleUI(id = "species_risk_maps"), value = "species_risk_maps",
                                           tags$div(style = "margin-top: 15px;",
                                                    mapRasterUI("risk")
                                           )
                                  ),
                                  
                                  tabPanel(titleModuleUI(id = "species_reporting"), 
                                           value = "species_reporting",
                                           tags$div(style = "margin-top: 15px;",
                                                    mapCubeUI(id = "reporting_t01")
                                           )
                                  ),
                                  
                                  tabPanel(titleModuleUI(id = "species_management"),
                                           value = "species_management",
                                           tags$div(style = "margin-top: 15px;",
                                                    uiOutput("species_managementContent")
                                           )
                                  ),
                                  
                                  tabPanel(titleModuleUI(id = "species_more"), 
                                           value = "species_more",
                                           tags$div(style = "margin-top: 15px;",
                                                    tabsetPanel(id = "species_more",
                                                                tabPanel(titleModuleUI(id = "species_habitats"), value = "species_habitats"),
                                                                tabPanel(titleModuleUI(id = "species_links"), value = "species_links",
                                                                         htmlSectionUI("links")
                                                                ),
                                                                tabPanel(titleModuleUI(id = "species_risk_assessment"), value = "species_risk_management",
                                                                         htmlSectionUI("risk_assessment")
                                                                ),
                                                                tabPanel(titleModuleUI(id = "species_images"), value = "species_images")
                                                    )
                                           )
                                  )
                      )
                    )
           ),
           
           footerSectionUI(id = "species", showReport = TRUE)
  )
)