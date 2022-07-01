# Server file for exoten indicators / checklist
# 
# Author: Eva Adriaensen
###############################################################################


output$checklist_title <- renderUI({
    
    results$translations$title[results$translations$plotFunction == tabChoices[2]]  
    
  })

lapply(c("taxa", "trend", "pathways", "origin"), function(iName)
    titleModuleServer(
      id = paste0("checklist_", iName),
      uiText = reactive(results$translations),
      plotFunction = iName
    ))

welcomeSectionServer(id = "checklist", uiText = reactive(results$translations))



### Filter Data
### ---------------


#output$exoten_unionlistOptions <- renderUI({
#    selectInput("exoten_unionlist", "Union list", choices = currentChoices$unionlist, selected = currentSelected$unionlist, multiple = FALSE)
#  })


observeEvent(input$exoten_more, {
    
  if (input$exoten_more %% 2 == 1)
    updateActionLink(session = session, inputId = "exoten_more", label = "Less",
      icon = icon("angle-double-left")) else
    updateActionLink(session = session, inputId = "exoten_more", label = "More",
      icon = icon("angle-double-right"))
  
  })


#results$exoten_filterFamily <- reactive({
#    
#    if (is.null(input$exoten_family) |
#      is.null(input$exoten_order) |
#      is.null(input$exoten_class) |
#      is.null(input$exoten_phylum) |
#      is.null(input$exoten_kingdom)) {
#      
#      subData <- subset(results$subExotenData(), kingdom %in% results$exoten_filterKingdom() &
#          phylum %in% results$exoten_filterPhylum() &
#          class %in% results$exoten_filterClass() &
#          order %in% results$exoten_filterOrder() )
#      
#      c(NA, unique(sort(subData$family))) 
#    } else
#      input$exoten_family
#    
#  })


### Final Data set
### ---------------

# habitat
output$exoten_habitat <- renderUI({
    
    selectInput("exoten_habitat", label = NULL, 
      choices = c("All habitats" = "", habitatChoices), 
      selected = results$urlHabitat, multiple = TRUE)
    
})


results$exoten_data <- reactive({
    
    subData <- exotenData
    searchId <- ""
        
    # taxa
    if (!is.null(input$exoten_taxa)) {
      searchId <- paste0(searchId, "&taxa=", paste(input$exoten_taxa, collapse = ", "))
      subData <- filterCombo(exotenData = subData, inputValue = results$urlTaxa, 
        inputLevels = taxaLevels)
    }
      
    # habitat
    if (!is.null(input$exoten_habitat)) {
      searchId <- paste0(searchId, "&habitat=", paste(input$exoten_habitat, collapse = ", "))
      subData <- subData[habitat %in% input$exoten_habitat, ]
    }
    
    # pathways
    if (!is.null(input$exoten_pw))
    subData <- filterCombo(exotenData = subData, inputValue = input$exoten_pw, 
      inputLevels = c("pathway_level1", "pathway_level2"))
    
    # degree of establishment
    if (!is.null(input$exoten_doe))
      subData <- subData[degree_of_establishment %in% input$exoten_doe, ]
    
    # native
    if (!is.null(input$exoten_native))
      subData <- filterCombo(exotenData = subData, inputValue = input$exoten_native, 
        inputLevels = c("native_continent", "native_range"))
    
    # time
    if (!is.null(input$exoten_time))
      subData <- subData[first_observed >= input$exoten_time[1] & 
          first_observed <= input$exoten_time[2], ]
    
    # region
    if (!is.null(input$exoten_region))
      subData <- subData[locality %in% input$exoten_region, ]
    
    # source
    if (!is.null(input$exoten_source))
      subData <- subData[source %in% input$exoten_source, ]
      
    
    results$searchId <- searchId
    
    subData
    
  })

output$nrowsFinal <- renderText({
    
    validate(need(nrow(results$exoten_data()) > 0, "No data available"))
    paste("Total number:", nrow(results$exoten_data()))

  })


### Table
### -----------------

tmpKey <- tableIndicatorsServer(
  id = "checklist",
  exotenData = results$exoten_data,
  unionlistData = unionlistData,
  occurrenceData = occurrenceData,
  translations = results$translations
)

# Redirect to species page
observeEvent(tmpKey(), {
    
    # Strip off the timestamp
    gbifKey <- strsplit(tmpKey(), "_")[[1]][1]
    
    # Lookup key for occurrence data
    newSpecies <- dictionary$scientificName[match(gbifKey, dictionary$gbifKey)]
    
    updateNavbarPage(session = session, inputId = "tabs", selected = "Species Information")
    results$species_choice <- newSpecies
    
  })

## Copy reactive values -> not working directly!
## https://stackoverflow.com/a/48883055/5840900
#observe({
#    results$plot_basic <- tmpBasic()
#    
#  }, priority = -1) # make sure that first everything else is up to date. Before evaluating this observe




### Plots
### -----------------


## Plot number of species per year
plotTriasServer(id = "checklist-count",
  data = results$exoten_data,
  uiText = reactive(results$translations),
  triasFunction = "indicator_introduction_year",
  triasArgs = reactive({
      list(
        start_year_plot = min(results$exoten_data()$first_observed, na.rm = TRUE) - 1,
        x_lab = "Jaar",
        y_lab = "Aantal ge\u00EFntroduceerde uitheemse soorten"
      )
    })
)


## Plot cumulative number of species per year
plotTriasServer(id = "checklist-cum",
  data = results$exoten_data,
  uiText = reactive(results$translations),
  triasFunction = "indicator_total_year",
  triasArgs = reactive({
      list(
        start_year_plot = min(results$exoten_data()$first_observed, na.rm = TRUE) - 1,
        x_lab = "Jaar",
        y_lab = "Aantal ge\u00EFntroduceerde uitheemse soorten"
      )
    })
)

## Plot trend occupancy
countOccupancyServer(id = "checklist",
  data = reactive(occupancy),
  uiText = reactive(results$translations)
)


## Plot number of species per pathway
countIntroductionPathwayServer(id = "checklist", 
  data = results$exoten_data,
  uiText = reactive(results$translations),
  region = results$exoten_filterLocality,
  pathway = reactive(input$exoten_pw1),
  time = reactive(input$exoten_time)
)


## Plot number of species per year by native region
countYearNativerangeServer(id = "checklist",
  uiText = reactive(results$translations),
  data = results$exoten_data
)


plotTriasServer(id = "checklist_tablePathway",
  uiText = reactive(results$translations),
  data = results$exoten_data,
  triasFunction = "get_table_pathways",
  triasArgs = reactive(list(species_names = "species")),
  outputType = "table"
)


plotTriasServer(id = "checklist_pathway1",
  uiText = reactive(results$translations),
  data = results$exoten_data,
  triasFunction = "visualize_pathways_level1"
)

plotTriasServer(id = "checklist_pathway1Trend",
  uiText = reactive(results$translations),
  data = results$exoten_data,
  triasFunction = "visualize_pathways_year_level1"
)

plotTriasServer(id = "checklist_pathway2",
  uiText = reactive(results$translations),
  data = results$exoten_data,
  triasFunction = "visualize_pathways_level2",
  triasArgs = reactive({
      validate(need(length(input$exoten_pw1) == 1, "Please select single pathway level 1 for this plot"))
      list(chosen_pathway_level1 = input$exoten_pw1)
    })
)