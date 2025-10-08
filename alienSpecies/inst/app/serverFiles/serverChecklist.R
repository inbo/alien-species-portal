# Server file for exoten indicators / checklist
# 
# Author: Eva Adriaensen
###############################################################################



# Create titles
lapply(c("taxa", "trend", "pathways", "origin"), function(iName)
    titleModuleServer(
      id = paste0("checklist_", iName),
      plotFunction = iName
    ))

welcomeSectionServer(id = "checklist")

# Translate necessary columns
results$filter_exotenDataTranslated <- reactive({
    
    exotenData[, pathway_level2_translate := translate(do.call(paste, c(.SD, sep = "_")))$title,
      .SDcols = c("pathway_level1", "pathway_level2")]
    exotenData[, ':=' (
      vernacular_name_col = get(paste0("vernacular_name_", results$language)),  
      pathway_level1_translate = translate(pathway_level1)$title,
      native_continent_translate = translate(native_continent)$title,
      native_range_translate = translate(native_range)$title,
      degree_of_establishment_translate = translate(degree_of_establishment)$title,
      habitat_translate = translate(habitat)$title,
      locality_translate = translate(locality)$title
    )]    
  
  })

unknownValue <- reactive(translate("unknown")$title)


### Filter Data
### ---------------

exoten_triggerMore <- reactive({
   
    req(!is.null(input$exoten_more))
    input$exoten_more + results$switchTranslation()
    
  })

observeEvent(exoten_triggerMore(), {
    
  if (input$exoten_more %% 2 == 1)
    updateActionLink(session = session, inputId = "exoten_more", 
      label = translate("less")$title,
      icon = icon("angle-double-up", class = "green-icon")) else
    updateActionLink(session = session, inputId = "exoten_more", 
      label = translate("more")$title,
      icon = icon("angle-double-down", class = "green-icon"))
  
  })



### Filters for Data
### -----------------


observe({
    
    req(input$tabs == "checklist_indicators")
    req(!is.null(input$exoten_searchVernacular))
    
    taxaChoices[ , vernacular_name_list := get(paste0("vernacular_name_", results$language, "_list"))]
    
    # Search on latin or vernacular name
    if (input$exoten_searchVernacular) {
      taxaChoices$showHtml <- sapply(seq_len(nrow(taxaChoices)), function(i)
          gsub("<b>.*</b>", paste0("<b>", 
              if (is.na(taxaChoices$vernacular_name_list[i])) "" else taxaChoices$vernacular_name_list[i], 
              "</b> <i>", taxaChoices$latin_name[i], "</i>"), taxaChoices$html[i]))
      taxaChoices[, label := vernacular_name_list] 
      setkey(taxaChoices, vernacular_name_list)
    } else {
      taxaChoices$showHtml <- sapply(seq_len(nrow(taxaChoices)), function(i)
          gsub("</b>", paste0("</b> <i>", strsplit(taxaChoices$vernacular_name_list[i],
                split = ", ")[[1]][1], "</i>"), taxaChoices$html[i])
      )     
      taxaChoices[, label := latin_name]
      setkey(taxaChoices, latin_name)
    }
    
    updateSelectizeInput(session, inputId = "exoten_taxa", choices = taxaChoices,
      selected = if ((is.null(all(isolate(results$exoten_taxa))) || all(isolate(results$exoten_taxa) == "")) & !is.null(urlSearch()$taxa))
          strsplit(urlSearch()$taxa, ",")[[1]] else
          isolate(results$exoten_taxa),
      server = TRUE,
      options = list(
        placeholder = translate("allTaxa")$title,
        render = I(
          "{
            option: function(item, escape) {
            return '<div class=\"long-selectize\">' + item.showHtml + '</div>'; }
            }"
        ))
    )
    
  })

# Save choice when leaving this tab
  observe({
      results$exoten_taxa <- input$exoten_taxa
    })


# habitat
filter_habitat <- filterSelectServer(
  id = "habitat",
  url = urlSearch,
  initChoices = reactive(c("allHabitats", habitatChoices))
)

# pathways
filter_pathwayLevel1 <- filterSelectServer(
  id = "pw_level1",
  url = urlSearch,
  initChoices = reactive(c("allPathways", pwLevel1Choices))
)


# doe
filter_doe <- filterSelectServer(
  id = "doe",
  url = urlSearch,
  initChoices = reactive(c("allDoe", doeChoices))
)

# native continent
filter_nativeContinent <- filterSelectServer(
  id = "native_continent",
  url = urlSearch,
  initChoices = reactive(c("allNative", nativeChoices))
)

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
        shinyjs::runjs('// Remove old handler first (to avoid duplicates)
            $(document).off("click.closeRef");
            
            // Add handler namespaced with .closeRef
            setTimeout(function() {
            $(document).on("click.closeRef", function(event) {
            if (!$(event.target).closest(".shiny-notification").length) {
            Shiny.setInputValue("close_ref", true, {priority: "event"});
            }
            });
            }, 0);')
      
  })

observeEvent(input$close_ref, priority = 5, {
    removeNotification(id = "ref")
    # Remove the handler once closed
    shinyjs::runjs('$(document).off("click.closeRef");')
  })

observeEvent(urlSearch(), {
    
    if ("timeNA" %in% names(urlSearch()))
      results$exoten_timeNA <- urlSearch()$timeNA == "true"
    if ("time" %in% names(urlSearch()))
      results$exoten_time <- as.numeric(strsplit(urlSearch()$time, split = "-")[[1]])
    
  })

observe({
    
    # Trigger update
    invalidateLater(1000)
    
    myLabel <- if (all(defaultTime == results$exoten_time) & defaultTimeNA == results$exoten_timeNA)
      translate("allYears")$title else
      paste(paste(results$exoten_time, collapse = "-"), if (results$exoten_timeNA) 
          translate("andMissing")$title)
  
    updateActionButton(session = session, inputId = "exoten_timeButton", label = myLabel)
    
  })

observeEvent(input$exoten_timeNA, results$exoten_timeNA <- input$exoten_timeNA)
observeEvent(input$exoten_time, results$exoten_time <- input$exoten_time)

output$exoten_timeNA <- renderUI({
    
    checkboxInput(inputId = "exoten_timeNA", label = translate("includeMissing")$title, 
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
  initChoices = reactive(c("allUnion", "Union list", "Non-union list"))
)

# regions
filter_region <- filterSelectServer(
  id = "region",
  url = urlSearch,
  initChoices = reactive(c("allRegions", regionChoices))
)

# bron
filter_source <- filterSelectServer(
  id = "source",
  url = urlSearch,
  initChoices = reactive(c("allSources", bronChoices))
)

# native range
nativeRangeChoices <- eventReactive(filter_nativeContinent(), ignoreNULL = FALSE, {
    if (is.null(filter_nativeContinent())) {
      sort(unique(exotenData$native_range))
    } else {
      sort(unique(exotenData$native_range[exotenData$native_continent %in% filter_nativeContinent()]))
    }
    
  })

filter_nativeRange <- filterSelectServer(
  id = "native_range",
  url = urlSearch,
  initChoices = reactive(c("allNativeRanges", nativeRangeChoices())),
  selected = reactive(results$filter_nativeRange)
)

currentNativeRange <- observeEvent(filter_nativeRange(), priority = 5, ignoreNULL = FALSE, {
    results$filter_nativeRange <- filter_nativeRange()    
  })


# pathway level 2
nativePw2Choices <- eventReactive(filter_pathwayLevel1(), ignoreNULL = FALSE, {
    if (is.null(filter_pathwayLevel1())) {
      sort(unique(exotenData$pathway_level2))
    } else {
      sort(unique(exotenData$pathway_level2[exotenData$pathway_level1 %in% filter_pathwayLevel1()]))
    }
    
  })

filter_pathwayLevel2 <- filterSelectServer(
  id = "pw_level2",
  url = urlSearch,
  initChoices = reactive(c("allPathwaySubCategories", nativePw2Choices())),
  selected = reactive(results$filter_pwLevel2)
)

observeEvent(filter_pathwayLevel2(), priority = 5, ignoreNULL = FALSE, {
    results$filter_pwLevel2 <- filter_pathwayLevel2()    
  })


### Final Data set
### ---------------


results$exoten_data <- reactive({
    
    subData <- data.table::copy(results$filter_exotenDataTranslated())
    
    # taxa
    if (!is.null(input$exoten_taxa)) {
      results$searchId$taxa <- paste(input$exoten_taxa, collapse = ",")
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
      results$searchId$habitat <- paste(filter_habitat(), collapse = ",")
      subData <- subData[grepl(paste(filter_habitat(), collapse = "|"), subData$habitat), ]
    }
    
    # pathways level 1
    if (!is.null(filter_pathwayLevel1())) {
      results$searchId$pw_level1 <- paste(filter_pathwayLevel1(), collapse = ",")
      subData <- subData[pathway_level1 %in% filter_pathwayLevel1(), ]
    }
    
    # degree of establishment
    if (!is.null(filter_doe())) {
      results$searchId$doe <- paste(filter_doe(), collapse = ",")
      subData <- subData[degree_of_establishment %in% filter_doe(), ]
    }
    
    # native continent
    if (!is.null(filter_nativeContinent())) {
      results$searchId$native_continent <- paste(filter_nativeContinent(), collapse = ",")
      subData <- subData[native_continent %in% filter_nativeContinent(), ]
    }
    
    # time
    if (!results$exoten_timeNA)
      results$searchId$timeNA <- "false"
    if (!all(results$exoten_time == defaultTime)) {
      results$searchId$time <- paste(results$exoten_time, collapse = "-")
      subData <- subData[first_observed %in% 
          c(if (results$exoten_timeNA) NA, results$exoten_time[1]:results$exoten_time[2]), ]
    }
    
    # unionlist - always save
    if (!is.null(filter_union())) {
      results$searchId$union <- filter_union()
      if (length(filter_union()) == 1) {
        if (filter_union() == "Union list")
          subData <- subData[nubKey %in% unionlistData$taxonKey, ] else if (filter_union() == "Non-union list")
          subData <- subData[!nubKey %in% unionlistData$taxonKey, ]
      }
    }
    
    # region
    if (!is.null(filter_region())) {
      results$searchId$region <- paste(filter_region(), collapse = ",")
      subData <- subData[locality %in% filter_region(), ]
    }
    
    # source
    if (!is.null(filter_source())) {
      results$searchId$source <- paste(filter_source(), collapse = ",")
      subData <- subData[source %in% filter_source(), ]
    }
    
    # native range
    if (!is.null(filter_nativeRange())) {
      results$searchId$native_range <- paste(filter_nativeRange(), collapse = ",")
      subData <- subData[native_range %in% filter_nativeRange(), ]
    }
    
    # pathways level 2
    if (!is.null(filter_pathwayLevel2())) {
      results$searchId$pw_level2 <- paste(filter_pathwayLevel2(), collapse = ",")
      subData <- subData[pathway_level2 %in% filter_pathwayLevel2(), ]
    }
    
    # use translations after subsetting
    data.table::setnames(subData[, c("pathway_level1", "pathway_level2", "habitat", "degree_of_establishment", "locality") := NULL], 
      old = c("pathway_level1_translate", "pathway_level2_translate", "habitat_translate", "degree_of_establishment_translate", "locality_translate"), 
      new = c("pathway_level1", "pathway_level2", "habitat", "degree_of_establishment", "locality"))
    
    
    subData
    
  })

output$nrowsFinal <- renderText({
    
    validate(need(nrow(results$exoten_data()) > 0, "No data available"))
    paste0(translate("totalSpecies")$title, ": ", 
      length(unique(results$exoten_data()$key)))

  })


### Update tabpage wrt URL link
### -----------------

# Append/Replace tabpage in URL
observe({
    
    req(input$exoten_tabs)
    
    input$exoten_tabs
    
    isolate(results$searchId$tab <- input$exoten_tabs)
  
  })


# Update tabpage wrt URL link
observe({
    
    req(urlSearch()$tab)
    updateTabsetPanel(session, inputId = "exoten_tabs",
      selected = urlSearch()$tab)
    
  })



### Table
### -----------------


## Copy reactive values -> not working directly!
## https://stackoverflow.com/a/48883055/5840900
tmpKey <- tableIndicatorsServer(
  id = "checklist",
  exotenData = results$exoten_data,
  unionlistData = unionlistData,
  occurrenceData = occurrenceData
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

results$exoten_xMajor <- reactive(optimalSteps(values = results$exoten_data()$first_observed))

occupancySelected <- reactive({
    # Filter occupancy data to selected species
    if (!is.null(input$exoten_taxa)) {
      taxaSelected <- dictionary$scientificName[match(input$exoten_taxa, dictionary$gbifKey)]
      occupancyFilt <- occupancy[occupancy$species %in% taxaSelected,]
    } else {
      occupancyFilt <- occupancy
    }
    
    if (!is.null(filter_union()) && length(filter_union()) == 1 && "Non-union list" %in% filter_union()) {
      occupancyFilt <- occupancyFilt[FALSE,]
    }
    
    occupancyFilt
  
  })

# Checklist tab
observeEvent(input$exoten_tabs, {
    
    req(input$exoten_tabs == "checklist_trend")
    req(!"checklist_trend" %in% results$renderedTabs)
    results$renderedTabs <- c(results$renderedTabs, "checklist_trend")
    
    ## Plot number of species per year
    plotTriasServer(id = "checklist-count",
      data = {
        # Retain only the smallest first_observed per key 
        # fix https://github.com/inbo/alien-species-portal/issues/128
        reactive(results$exoten_data()[order(first_observed), .SD[1,], by = "key"])
      },
      triasFunction = "indicator_introduction_year",
      triasArgs = reactive({
          list(
            start_year_plot = min(results$exoten_data()$first_observed, na.rm = TRUE) - 1,
            x_major_scale_stepsize = results$exoten_xMajor(),
            x_minor_scale_stepsize = results$exoten_xMajor()/2,
            x_lab = translate("year")$title,
            y_lab = translate("indicator_introduction_year")$title
          )
        })
    )
    
    
    ## Plot cumulative number of species per year
    plotTriasServer(id = "checklist-cum",
      data = {
        # Retain only the smallest first_observed per key 
        # fix https://github.com/inbo/alien-species-portal/issues/128
        reactive(results$exoten_data()[order(first_observed), .SD[1,], by = "key"])
      },
      triasFunction = "indicator_total_year",
      triasArgs = reactive({
          list(
            start_year_plot = min(results$exoten_data()$first_observed, na.rm = TRUE) - 1,
            x_major_scale_stepsize = results$exoten_xMajor(),
            x_minor_scale_stepsize = results$exoten_xMajor()/2,
            x_lab = translate("year")$title,
            y_lab = translate("indicator_total_year")$title
          )
        })
    )
    
    ## Plot trend occupancy
    countOccupancyServer(id = "checklist",
      data = occupancySelected
    )
    
  })


pathway1Selected <- reactive({
    if (is.null(filter_pathwayLevel1())) {
      pwLevel1Choices
    } else {
      filter_pathwayLevel1()
    }
  })

# Pathways tab
observeEvent(input$exoten_tabs, {
    
    req(input$exoten_tabs == "checklist_pathways")
    req(!"checklist_pathways" %in% results$renderedTabs)
    results$renderedTabs <- c(results$renderedTabs, "checklist_pathways")
    
    plotTriasServer(id = "checklist_tablePathway",
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
      data = results$exoten_data,
      triasFunction = "visualize_pathways_level1",
      triasArgs = reactive({
          list(
            x_lab = translate("numberTaxa")$title,
            y_lab = translate("pathways")$title,
            cbd_standard = FALSE,
            pathways = results$checklist_levelsP1()
          )
        })
    )
    
    plotTriasServer(id = "checklist_pathway1Trend",
      data = results$exoten_data,
      triasFunction = "visualize_pathways_year_level1",
      triasArgs = reactive({
          list(
            x_lab = translate("period")$title,
            y_lab = translate("numberTaxa")$title,
            cbd_standard = FALSE,
            pathways = results$checklist_levelsP1()
          )
        })
    )
    
    plotTriasServer(id = "checklist_pathway2",
      filters = reactive(list("pathway_level1" = pathway1Selected())),
      data = results$exoten_data,
      triasFunction = "visualize_pathways_level2",
      triasArgs = reactive({
          list(
            x_lab = translate("numberTaxa")$title,
            y_lab = translate("pathways")$title,
            cbd_standard = FALSE
          )
        })
    )
    
    plotTriasServer(id = "checklist_pathway2Trend",
      filters = reactive(list("pathway_level1" = pathway1Selected())),
      data = results$exoten_data,
      triasFunction = "visualize_pathways_year_level2",
      triasArgs = reactive({
          list(
            x_lab = translate("period")$title,
            y_lab = translate("numberTaxa")$title,
            cbd_standard = FALSE
          )
        })
    )
    
  })



# Origin tab
observeEvent(input$exoten_tabs, {
    
    req(input$exoten_tabs == "checklist_origin")
    req(!"checklist_origin" %in% results$renderedTabs)
    results$renderedTabs <- c(results$renderedTabs, "checklist_origin")
    
    ## Plot number of species per year by native region
    plotTriasServer(id = "checklist_yearNativeRange",
      data = reactive({
        tmpData <- results$exoten_data()
        tmpData[, ':=' (
            native_continent = native_continent_translate,
            native_range = native_range_translate
          )]
        tmpData
      }),
      triasFunction = "indicator_native_range_year",
      triasArgs = reactive({
          list(
            years = if (is.null(input$exoten_time))
                min(results$exoten_data()$first_observed, na.rm = TRUE):
                  max(results$exoten_data()$first_observed, na.rm = TRUE) else 
                input$exoten_time[1]:input$exoten_time[2],
            x_include_missing = TRUE,
            x_major_scale_stepsize = results$exoten_xMajor(),
            x_lab = translate("year")$title,
            y_lab = translate("number")$title
          )
        }),
      filters = reactive(list(
        regionLevel = c("native_continent", "native_range"),
        summarizeBy = c("absolute", "cumulative")
        )
    ))
    
  })


# Contact button
footerSectionServer(id = "checklist")
