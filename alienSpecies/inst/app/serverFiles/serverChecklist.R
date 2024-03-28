# Server file for exoten indicators / checklist
# 
# Author: Eva Adriaensen
###############################################################################



# Create titles
lapply(c("taxa", "trend", "pathways", "origin"), function(iName)
    titleModuleServer(
      id = paste0("checklist_", iName),
      uiText = reactive(results$translations),
      plotFunction = iName
    ))

welcomeSectionServer(id = "checklist", uiText = reactive(results$translations))

# Translate necessary columns
results$filter_exotenDataTranslated <- reactive({
    
    exotenData[, pathway_level2_translate := translate(results$translations, do.call(paste, c(.SD, sep = "_")))$title,
      .SDcols = c("pathway_level1", "pathway_level2")]
    exotenData[, ':=' (
      pathway_level1_translate = translate(results$translations, pathway_level1)$title,
      native_continent_translate = translate(results$translations, native_continent)$title,
      native_range_translate = translate(results$translations, native_range)$title,
      degree_of_establishment_translate = translate(results$translations, degree_of_establishment)$title,
      habitat_translate = translate(results$translations, habitat)$title
    )]    
  
  })

unknownValue <- reactive(translate(results$translations, "unknown")$title)


### Filter Data
### ---------------

exoten_triggerMore <- reactive({
   
    req(!is.null(input$exoten_more))
    input$exoten_more + results$switchTranslation()
    
  })

observeEvent(exoten_triggerMore(), {
    
  if (input$exoten_more %% 2 == 1)
    updateActionLink(session = session, inputId = "exoten_more", 
      label = translate(results$translations, "less")$title,
      icon = icon("angle-double-up", class = "green-icon")) else
    updateActionLink(session = session, inputId = "exoten_more", 
      label = translate(results$translations, "more")$title,
      icon = icon("angle-double-down", class = "green-icon"))
  
  })



### Filters for Data
### -----------------


observeEvent(input$tabs, {
    
    if (input$tabs == "global_indicators")
      updateSelectizeInput(session, inputId = "exoten_taxa", choices = taxaChoices,
        selected = urlSearch()$taxa,
        server = TRUE,
        options = list(
          placeholder = translate(results$translations, "allTaxa")$title,
          render = I(
            "{
              option: function(item, escape) {
              return '<div class=\"long-selectize\">' + item.html + '</div>'; }
              }"
          ))
      )

  })


# habitat
filter_habitat <- filterSelectServer(
  id = "habitat",
  url = urlSearch,
  initChoices = c("allHabitats", habitatChoices),
  translations = reactive(results$translations)
)

# pathways
results$filter_pwChoices <- reactive({
    
    createDoubleChoices(
      exotenData = results$filter_exotenDataTranslated(), 
      columns = c("pathway_level1", "pathway_level2"))
    
  })

output$filter_pw <- renderUI({
    
    comboTreeInput("exoten_pw", choices = results$filter_pwChoices(),
      placeholder = translate(results$translations, "allPathways")$title, 
      selected = urlSearch()$pw)
    
  })

# doe
filter_doe <- filterSelectServer(
  id = "doe",
  url = urlSearch,
  initChoices = c("allDoe", doeChoices),
  translations = reactive(results$translations)
)

# native
results$filter_nativeChoices <- reactive({
    
    createDoubleChoices(
      exotenData = results$filter_exotenDataTranslated(),
      columns = c("native_continent", "native_range"))
    
  })

output$filter_native <- renderUI({
    
    comboTreeInput("exoten_native", choices = results$filter_nativeChoices(),
      placeholder = translate(results$translations, "allNative")$title, 
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
      translate(results$translations, "allYears")$title else
      paste(paste(results$exoten_time, collapse = "-"), if (results$exoten_timeNA) 
          translate(results$translations, "andMissing")$title)
  
    updateActionButton(session = session, inputId = "exoten_timeButton", label = myLabel)
    
  })

observeEvent(input$exoten_timeNA, results$exoten_timeNA <- input$exoten_timeNA)
observeEvent(input$exoten_time, results$exoten_time <- input$exoten_time)

output$exoten_timeNA <- renderUI({
    
    checkboxInput(inputId = "exoten_timeNA", label = translate(results$translations, "includeMissing")$title, 
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
  initChoices = c("allUnion", "Union list", "Non-union list"),
  translations = reactive(results$translations)
)

# regions
filter_region <- filterSelectServer(
  id = "region",
  url = urlSearch,
  initChoices = c("allRegions", regionChoices),
  translations = reactive(results$translations)
)

# bron
filter_source <- filterSelectServer(
  id = "source",
  url = urlSearch,
  initChoices = c("allSources", bronChoices),
  translations = reactive(results$translations)
)


### Final Data set
### ---------------


results$exoten_data <- reactive({
    
    subData <- data.table::copy(results$filter_exotenDataTranslated())
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
      subData <- subData[grepl(paste(filter_habitat(), collapse = "|"), subData$habitat), ]
    }
    
    # pathways
    if (!is.null(input$exoten_pw)) {
      matchPw <- matchCombo(selected = input$exoten_pw, longChoices = unlist(results$filter_pwChoices()))
      searchId <- paste0(searchId, "&pw=", matchPw)
      subData <- filterCombo(exotenData = subData, inputValue = strsplit(matchPw, split = ",")[[1]], 
        inputLevels = c("pathway_level1", "pathway_level2"))
    }
    
    # degree of establishment
    if (!is.null(filter_doe())) {
      searchId <- paste0(searchId, "&doe=", paste(filter_doe(), collapse = ","))
      subData <- subData[degree_of_establishment %in% filter_doe(), ]
    }
    
    # native
    if (!is.null(input$exoten_native)) {
      matchNative <- matchCombo(selected = input$exoten_native, longChoices = unlist(results$filter_nativeChoices())) 
      searchId <- paste0(searchId, "&native=", matchNative)
      subData <- filterCombo(exotenData = subData, inputValue = strsplit(matchNative, split = ",")[[1]], 
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
          subData <- subData[nubKey %in% unionlistData$taxonKey, ] else if (filter_union() == "Non-union list")
          subData <- subData[!nubKey %in% unionlistData$taxonKey, ]
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
    
    # use translations after subsetting
    data.table::setnames(subData[, c("pathway_level1", "pathway_level2", "habitat", "degree_of_establishment") := NULL], 
      old = c("pathway_level1_translate", "pathway_level2_translate", "habitat_translate", "degree_of_establishment_translate"), 
      new = c("pathway_level1", "pathway_level2", "habitat", "degree_of_establishment"))
    
    results$searchId <- searchId
    
    subData
    
  })

output$nrowsFinal <- renderText({
    
    validate(need(nrow(results$exoten_data()) > 0, "No data available"))
    paste0(translate(results$translations, "totalSpecies")$title, ": ", 
      length(unique(results$exoten_data()$key)))

  })


### Legend
### -----------------

output$exoten_legendLink <- renderUI({
    
    actionLink(inputId = "exoten_legend", 
      label = translate(results$translations, "tableLegend")$title, 
      icon = icon("angle-double-down", class = "green-icon"))
    
  })

output$exoten_legendText <- renderUI({
    
    tagList(
    tags$b(translate(results$translations, "icons")$title),
    p(icon("star"), translate(results$translations, "is_union")$title),
    p(icon("play"), translate(results$translations, "min_1_obs")$title),
    tags$b(translate(results$translations, "colors")$title),
    p(drawBullet(color = "black"), translate(results$translations, "only_obs")$title),
#    p(drawBullet(color = "orange"), translate(results$translations, "incomplete_out")$title),
    p(drawBullet(color = "#E4E517"), translate(results$translations, "all_out")$title)
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
    gbifKey <- strsplit(tmpKey(), "_")[[1]][2]
    tabPage <- strsplit(tmpKey(), "_")[[1]][1]
    
    updateNavbarPage(session = session, inputId = "tabs", selected = "species_information")
    # 2nd update only works if the tabs already exist
    updateTabsetPanel(session = session, inputId = "species_tabs", selected = paste0("species_", tabPage))
    results$species_choice <- dictionary$taxonKey[match(gbifKey, dictionary$gbifKey)]
    
  })



### Plots
### -----------------


# Checklist tab
observeEvent(input$exoten_tabs, {
    
    req(input$exoten_tabs == "checklist_trend")
    req(!"checklist_trend" %in% results$renderedTabs)
    results$renderedTabs <- c(results$renderedTabs, "checklist_trend")
    print(input$exoten_tabs)
    
    ## Plot number of species per year
    plotTriasServer(id = "checklist-count",
      data = results$exoten_data,
      uiText = reactive(results$translations),
      triasFunction = "indicator_introduction_year",
      triasArgs = reactive({
          list(
            start_year_plot = min(results$exoten_data()$first_observed, na.rm = TRUE) - 1,
            x_lab = translate(results$translations, "year")$title,
            y_lab = translate(results$translations, "indicator_introduction_year")$title
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
            x_lab = translate(results$translations, "year")$title,
            y_lab = translate(results$translations, "indicator_total_year")$title
          )
        })
    )
    
    ## Plot trend occupancy
    countOccupancyServer(id = "checklist",
      data = reactive(occupancy),
      uiText = reactive(results$translations)
    )
    
  })




# Pathways tab
observeEvent(input$exoten_tabs, {
    
    req(input$exoten_tabs == "checklist_pathways")
    req(!"checklist_pathways" %in% results$renderedTabs)
    results$renderedTabs <- c(results$renderedTabs, "checklist_pathways")
    print(input$exoten_tabs)
    
    plotTriasServer(id = "checklist_tablePathway",
      uiText = reactive(results$translations),
      data = results$exoten_data,
      triasFunction = "get_table_pathways",
      triasArgs = reactive(list(species_names = "species")),
      outputType = "table"
    )
    
    
    results$checklist_levelsP1 <- reactive({
        
        levelsP1 <- sort(unique(results$exoten_data()$pathway_level1))
        c(grep(unknownValue(), levelsP1, value = TRUE, invert = TRUE), 
          grep(unknownValue(), levelsP1, value = TRUE)
        ) 
        
      })
    
    plotTriasServer(id = "checklist_pathway1",
      uiText = reactive(results$translations),
      data = results$exoten_data,
      triasFunction = "visualize_pathways_level1",
      triasArgs = reactive({
          list(
            x_lab = translate(results$translations, "numberTaxa")$title,
            y_lab = translate(results$translations, "pathways")$title,
            cbd_standard = FALSE,
            pathways = results$checklist_levelsP1()
          )
        })
    )
    
    plotTriasServer(id = "checklist_pathway1Trend",
      uiText = reactive(results$translations),
      data = results$exoten_data,
      triasFunction = "visualize_pathways_year_level1",
      triasArgs = reactive({
          list(
            x_lab = translate(results$translations, "period")$title,
            y_lab = translate(results$translations, "numberTaxa")$title,
            cbd_standard = FALSE,
            pathways = results$checklist_levelsP1()
          )
        })
    )
    
    plotTriasServer(id = "checklist_pathway2",
      uiText = reactive(results$translations),
      data = results$exoten_data,
      triasFunction = "visualize_pathways_level2",
      triasArgs = reactive({
          validate(need(length(unique(results$exoten_data()$pathway_level1)) == 1, 
              translate(results$translations, "singlePathway")$title))
          list(
            chosen_pathway_level1 = unique(results$exoten_data()$pathway_level1),
            x_lab = translate(results$translations, "numberTaxa")$title,
            y_lab = translate(results$translations, "pathways")$title,
            cbd_standard = FALSE,
            pathways = {
              levelsP2 <- sort(unique(results$exoten_data()$pathway_level2))
              c(grep(unknownValue(), levelsP2, value = TRUE, invert = TRUE), 
                grep(unknownValue(), levelsP2, value = TRUE)
              )          
            }
          )
        })
    )
    
    plotTriasServer(id = "checklist_pathway2Trend",
      uiText = reactive(results$translations),
      data = results$exoten_data,
      triasFunction = "visualize_pathways_year_level2",
      triasArgs = reactive({
          validate(need(length(unique(results$exoten_data()$pathway_level1)) == 1, 
              translate(results$translations, "singlePathway")$title))
          list(
            chosen_pathway_level1 = unique(results$exoten_data()$pathway_level1),
            x_lab = translate(results$translations, "period")$title,
            y_lab = translate(results$translations, "numberTaxa")$title,
            cbd_standard = FALSE,
            pathways = {
              levelsP2 <- sort(unique(results$exoten_data()$pathway_level2))
              c(grep(unknownValue(), levelsP2, value = TRUE, invert = TRUE), 
                grep(unknownValue(), levelsP2, value = TRUE)
              )          
            }
          )
        })
    )
    
  })



# Origin tab
observeEvent(input$exoten_tabs, {
    
    req(input$exoten_tabs == "checklist_origin")
    req(!"checklist_origin" %in% results$renderedTabs)
    results$renderedTabs <- c(results$renderedTabs, "checklist_origin")
    print(input$exoten_tabs)
    
    ## Plot number of species per year by native region
    plotTriasServer(id = "checklist_yearNativeRange",
      uiText = reactive(results$translations),
      data = results$exoten_data,
      triasFunction = "indicator_native_range_year",
      triasArgs = reactive({
          list(
            x_lab = translate(results$translations, "year")$title,
            y_lab = translate(results$translations, "number")$title
          )
        })
    )
    
  })