# Server file for exoten indicators / checklist
# 
# Author: Eva Adriaensen
###############################################################################


output$checklist_title <- renderUI({
    
    translate(results$translations, id = tabChoices[2])  
    
  })

# Create titles
lapply(c("taxa", "trend", "pathways", "origin"), function(iName)
    titleModuleServer(
      id = paste0("checklist_", iName),
      uiText = reactive(results$translations),
      plotFunction = iName
    ))

welcomeSectionServer(id = "checklist", uiText = reactive(results$translations))



### Filter Data
### ---------------

exoten_triggerMore <- reactive({
   
    req(!is.null(input$exoten_more))
    input$exoten_more + results$switchTranslation()
    
  })

observeEvent(exoten_triggerMore(), {
    
  if (input$exoten_more %% 2 == 1)
    updateActionLink(session = session, inputId = "exoten_more", 
      label = translate(results$translations, "less"),
      icon = icon("angle-double-up")) else
    updateActionLink(session = session, inputId = "exoten_more", 
      label = translate(results$translations, "more"),
      icon = icon("angle-double-down"))
  
  })



### Filters for Data
### -----------------

urlSearch <- reactive(parseQueryString(session$clientData$url_search))

observeEvent(input$tabs, {
    
    if (input$tabs == "global_indicators")
      updateSelectizeInput(session, inputId = "exoten_taxa", choices = taxaChoices,
        selected = urlSearch()$taxa,
        server = TRUE,
        options = list(
          placeholder = translate(results$translations, "allTaxa"),
          render = I(
            '{
              option: function(item, escape) {
              return "<div>" + item.html + "</div>"; }
              }'
          ))
      )

  })


# habitat
filter_habitat <- filterSelectServer(
  id = "habitat",
  url = urlSearch,
  placeholder = reactive(translate(results$translations, "allHabitats")),
  initChoices = habitatChoices
)

# pathways
output$filter_pw <- renderUI({
    
    comboTreeInput("exoten_pw", choices = pwChoices,
      placeholder = translate(results$translations, "allPathways"), 
      selected = urlSearch()$pw)
    
  })

# doe
filter_doe <- filterSelectServer(
  id = "doe",
  url = urlSearch,
  placeholder = reactive(translate(results$translations, "allDoe")),
  initChoices = doeChoices
)

# native
output$filter_native <- renderUI({
    
    comboTreeInput("exoten_native", choices = nativeChoices,
      placeholder = translate(results$translations, "allNative"), 
      selected = urlSearch()$native)
    
  })

# time
## tricky code to make the input values apply even if the popup is not yet clicked
observeEvent(input$exoten_timeButton, {
  
        showNotification(id = "ref",
          tagList(
            tags$div(class = "variety-selector", 
              uiOutput("exoten_timeNA"),
              uiOutput("exoten_time")
        )   
          ),
          duration = NULL
        )
        shinyjs::runjs('setTimeout(function(){$("#time-popup").append($("#shiny-notification-panel"))},0);')
    
  })

observeEvent(urlSearch(), {
    
    if (!is.null(urlSearch()$timeNA))
      results$exoten_timeNA <- urlSearch()$timeNA == "true"
    if (!is.null(urlSearch()$time))
      results$exoten_time <- as.numeric(strsplit(urlSearch()$time, split = "-")[[1]])
    
  })

observe({
    
    # Trigger update
    invalidateLater(1000)
    
    myLabel <- if (all(defaultTime == results$exoten_time) & defaultTimeNA == results$exoten_timeNA)
      translate(results$translations, "allYears") else
      paste(paste(results$exoten_time, collapse = "-"), if (results$exoten_timeNA) 
          translate(results$translations, "andMissing"))
  
    updateActionButton(session = session, inputId = "exoten_timeButton", label = myLabel)
    
  })

observeEvent(input$exoten_timeNA, results$exoten_timeNA <- input$exoten_timeNA)
observeEvent(input$exoten_time, results$exoten_time <- input$exoten_time)

output$exoten_timeNA <- renderUI({
    
    checkboxInput(inputId = "exoten_timeNA", label = translate(results$translations, "includeMissing"), 
      value = results$exoten_timeNA)
    
  })

output$exoten_time <- renderUI({
    
    sliderInput(inputId = "exoten_time", label = NULL, 
      value = results$exoten_time,
      min = min(exotenData$first_observed, na.rm = TRUE),
      max = max(exotenData$first_observed, na.rm = TRUE),
      step = 1,
      sep = "")
    
  })

# union
filter_union <- filterSelectServer(
  id = "union",
  url = urlSearch,
  placeholder = reactive(translate(results$translations, "allUnion")),
  initChoices = c("Union list", "Non-union list")
)

# regions
filter_region <- filterSelectServer(
  id = "region",
  url = urlSearch,
  placeholder = reactive(translate(results$translations, "allRegions")),
  initChoices = regionChoices
)

# bron
filter_source <- filterSelectServer(
  id = "source",
  url = urlSearch,
  placeholder = reactive(translate(results$translations, "allSources")),
  initChoices = bronChoices
)


### Final Data set
### ---------------


results$exoten_data <- reactive({
    
    subData <- exotenData
    searchId <- ""
        
    # taxa
    if (!is.null(input$exoten_taxa)) {
      searchId <- paste0(searchId, "&taxa=", 
        paste(input$exoten_taxa, collapse = ","))
#        matchCombo(selected = input$exoten_taxa, longChoices = longTaxaChoices))
#      subData <- filterCombo(exotenData = subData, inputValue = input$exoten_taxa, 
#        inputLevels = taxaLevels)
      matchRow <- sapply(input$exoten_taxa, function(iChoice)
          match(iChoice, taxaChoices$value))
      subData <- filterCombo(exotenData = subData, inputValue = taxaChoices$long[matchRow],
        inputLevels = taxaLevels)
    }
      
    # habitat
    if (!is.null(filter_habitat())) {
      searchId <- paste0(searchId, "&habitat=", paste(filter_habitat(), collapse = ","))
      subData <- subData[habitat %in% filter_habitat(), ]
    }
    
    # pathways
    if (!is.null(input$exoten_pw)) {
      searchId <- paste0(searchId, "&pw=", 
        matchCombo(selected = input$exoten_pw, longChoices = longPwChoices))
      subData <- filterCombo(exotenData = subData, inputValue = input$exoten_pw, 
        inputLevels = c("pathway_level1", "pathway_level2"))
    }
    
    # degree of establishment
    if (!is.null(filter_doe())) {
      searchId <- paste0(searchId, "&doe=", paste(filter_doe(), collapse = ","))
      subData <- subData[degree_of_establishment %in% filter_doe(), ]
    }
    
    # native
    if (!is.null(input$exoten_native)) {
      searchId <- paste0(searchId, "&native=", 
        matchCombo(selected = input$exoten_native, longChoices = longNativeChoices))
      subData <- filterCombo(exotenData = subData, inputValue = input$exoten_native, 
        inputLevels = c("native_continent", "native_range"))
    }
    
    # time
    if (!results$exoten_timeNA)
      searchId <- paste0(searchId, "&timeNA=false")
    if (!all(results$exoten_time == defaultTime)) {
      searchId <- paste0(searchId, "&time=", paste(results$exoten_time, collapse = "-"))
      subData <- subData[first_observed %in% 
          c(if (results$exoten_timeNA) NA, results$exoten_time[1]:results$exoten_time[2]), ]
    }
    
    # unionlist - always save
    if (!is.null(filter_union())) {
      searchId <- paste0(searchId, "&union=", filter_union())
      if (length(filter_union()) == 1) {
        if (filter_union() == "Union list")
          subData <- subData[species %in% unionlistData$scientificName, ] else if (filter_union() == "Non-union list")
          subData <- subData[!species %in% unionlistData$scientificName, ]
      }
  }
    
    # region
    if (!is.null(filter_region())) {
      searchId <- paste0(searchId, "&region=", paste(filter_region(), collapse = ","))
      subData <- subData[locality %in% filter_region(), ]
    }
    
    # source
    if (!is.null(filter_source())) {
      searchId <- paste0(searchId, "&source=", paste(filter_source(), collapse = ","))
      subData <- subData[source %in% filter_source(), ]
    }
    
    results$searchId <- searchId
    
    subData
    
  })

output$nrowsFinal <- renderText({
    
    validate(need(nrow(results$exoten_data()) > 0, "No data available"))
    paste0(translate(results$translations, "totalSpecies"), ": ", 
      length(unique(results$exoten_data()$key)))

  })


### Legend
### -----------------

output$exoten_legendLink <- renderUI({
    
    actionLink(inputId = "exoten_legend", 
      label = translate(results$translations, "tableLegend"), 
      icon = icon("angle-double-down"))
    
  })

output$exoten_legendText <- renderUI({
    
    tagList(
    tags$b(translate(results$translations, "icons")),
    p(icon("star"), translate(results$translations, "min_1_obs")),
    p(icon("play"), translate(results$translations, "is_union")),
    tags$b(translate(results$translations, "colors")),
    p(drawBullet(color = "black"), translate(results$translations, "only_obs")),
    p(drawBullet(color = "orange"), translate(results$translations, "incomplete_out")),
    p(drawBullet(color = "green"), translate(results$translations, "all_out"))
  )
  
  })



### Table
### -----------------


## Copy reactive values -> not working directly!
## https://stackoverflow.com/a/48883055/5840900
tmpKey <- tableIndicatorsServer(
  id = "checklist",
  exotenData = results$exoten_data,
  unionlistData = unionlistData,
  occurrenceData = occurrenceData,
  uiText = reactive(results$translations)
)

# Redirect to species page
observeEvent(tmpKey(), {
    
    # Strip off the timestamp
    gbifKey <- strsplit(tmpKey(), "_")[[1]][1]
    
    # Lookup key for occurrence data
    newSpecies <- dictionary$scientificName[match(gbifKey, dictionary$gbifKey)]
    
    updateNavbarPage(session = session, inputId = "tabs", selected = "species_information")
    results$species_choice <- newSpecies
    
  })



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
        x_lab = translate(results$translations, "year"),
        y_lab = translate(results$translations, "indicator_introduction_year")
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
        x_lab = translate(results$translations, "year"),
        y_lab = translate(results$translations, "indicator_total_year")
      )
    })
)

## Plot trend occupancy
countOccupancyServer(id = "checklist",
  data = reactive(occupancy),
  uiText = reactive(results$translations)
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
  triasFunction = "visualize_pathways_level1",
  triasArgs = reactive({
      list(
        x_lab = translate(results$translations, "numberTaxa"),
        y_lab = translate(results$translations, "pathways")
      )
    })
)

plotTriasServer(id = "checklist_pathway1Trend",
  uiText = reactive(results$translations),
  data = results$exoten_data,
  triasFunction = "visualize_pathways_year_level1",
  triasArgs = reactive({
      list(
        x_lab = translate(results$translations, "period"),
        y_lab = translate(results$translations, "numberTaxa")
      )
    })
)

plotTriasServer(id = "checklist_pathway2",
  uiText = reactive(results$translations),
  data = results$exoten_data,
  triasFunction = "visualize_pathways_level2",
  triasArgs = reactive({
      validate(need(length(unique(results$exoten_data()$pathway_level1)) == 1, 
          translate(results$translations, "singlePathway")))
      list(
        chosen_pathway_level1 = unique(results$exoten_data()$pathway_level1),
        x_lab = translate(results$translations, "numberTaxa"),
        y_lab = translate(results$translations, "pathways")
      )
    })
)