# Species page
# 
# Example species
# observations: Saponaria officinalis
# reporting: Orconectes limosus
# management: Oxyura jamaicensis
# 
# Author: mvarewyck
###############################################################################




# Collect all results for the report
dashReport <- reactiveValues()

lapply(c("observations", "indicators", "reporting", "management", "more",
    "habitats", "risk_maps", "links", "risk_assessment", "images"), function(iName)
    titleModuleServer(
      id = paste0("species_", iName),
      plotFunction = iName
    ))

welcomeSectionServer(id = "species")


# Species selection
results$species_choices <- reactive({
    
    # Observations
    taxChoices <- occurrenceData[!duplicated(taxonKey), scientificName]
    # Reporting
    reportChoices <- dfCube[!duplicated(dfCube$species) & !dfCube$species %in% taxChoices, "species"]
    
    choiceNames <- sort(c(taxChoices, reportChoices))
    choices <- dictionary$taxonKey[match(choiceNames, dictionary$scientificName)]
    
    names(choices) <- choiceNames
    choices
    
  })


observe({
    
    # Trigger update when changing tab
    if (input$tabs == "species_information")
      updateSelectizeInput(session = session, inputId = "species_choice",
        choices = results$species_choices(),
        selected = if (results$species_choice == "" & !is.null(urlSearch()$taxonkey))
          urlSearch()$taxonkey else
          results$species_choice,
        server = TRUE)    
    
  })

# Save choice when leaving this tab
observeEvent(input$tabs, {
    
    req(input$tabs != "species_information")
    results$species_choice <- input$species_choice
    
  })


# Gewest selection
observe({
    
    choices <- c("flanders", "wallonia", "brussels")
    names(choices) <- translate(choices)$title
    
    # Trigger update when changing tab
    if (input$tabs == "species_information")
      updateSelectInput(session = session, inputId = "species_gewest", 
      choices = choices,
      selected = if (!is.null(urlSearch()$gewest)) 
          strsplit(urlSearch()$gewest, split = ",")[[1]] else if (!is.null(isolate(input$species_gewest))) input$species_gewest else 
          choices)
            
})


# Update search ID
observe(results$searchId$taxonkey <- input$species_choice)
observe(results$searchId$gewest <- paste(input$species_gewest, collapse = ","))

output$species_disclaimer <- renderUI({
    
    req(input$species_choice)
    
    disclaimerId <- paste0("obs_disclaimer_", input$species_choice)
    
    if (translate(disclaimerId)$description != "") {
      
      tags$div(
        class = "info-box",
        tags$div(class = "info-icon", "!"),
        tags$div(HTML(translate(id = disclaimerId)$description))
      )
      
    }
    
  })

output$missingFilters_message <- renderUI({
    if (nchar(input$species_choice) == 0 || is.null(input$species_gewest)) {
        tags$div(style = "color: red; font: bold;", translate("missingFilters_species")$title)
    } else {
      NULL
    }
    
  })

### Update tabpage wrt URL link
### -----------------

# Append/Replace tabpage in URL
observe({
    
    req(input$species_tabs)
    
    input$species_tabs
    
    isolate(results$searchId$tab <- input$species_tabs)
    
  })


# Update tabpage wrt URL link
observe({
    
    req(urlSearch()$tab)
    updateTabsetPanel(session, inputId = "species_tabs",
      selected = urlSearch()$tab)
    
  })



### Observations
### -----------------

# Name corresponding with the selected taxonkey
taxonName <- reactive({
    
    req(input$species_choice)
    dictionary$scientificName[match(input$species_choice, dictionary$taxonKey)]
    
  })

# Disable tab if no info
observe({
    
    req(!is.null(input$species_choice))
    
    # https://stackoverflow.com/a/64324799
    shinyjs::toggleState(
      selector = '#species_tabs a[data-value="species_observations"', 
      condition = !is.na(input$species_choice)
    )
    
    if (is.na(input$species_choice) & input$species_tabs == "species_observations")
      updateTabsetPanel(session = session, inputId = "species_tabs", 
        selected = "species_reporting")
    
  })


## Map + barplot
dashReport <- mapCubeServer(id = "observations",
  species = taxonName,
  gewest = reactive(req(input$species_gewest)),
  df = reactive({
      req(input$species_choice)
      occurrenceData[taxonKey %in% input$species_choice, ]      
    }),
  groupVariable = "cell_code",
  shapeData = allShapes,
  showPeriod = TRUE,
  dashReport = dashReport,
  triggerReport = species_createReport
)




### Indicators
### -----------------


# Disable tab if no info
observe({
    
    req(!is.null(input$species_choice))
    
    # https://stackoverflow.com/a/64324799
    shinyjs::toggleState(
      selector = '#species_tabs a[data-value="species_indicators"', 
      condition = !is.na(input$species_choice)
    )
    
    if (is.na(input$species_choice) & input$species_tabs == "species_indicators")
      updateTabsetPanel(session = session, inputId = "species_tabs", 
        selected = "species_reporting")
    
  })

results$species_gamData <- reactive({
    
    req(input$species_choice)
    summarizeTimeSeries(
      species = as.numeric(input$species_choice), 
      region = input$species_gewest)
    
  })

## Emergence status GAM - Observations
dashReport <- plotTriasServer(id = "indicators_gamObservations",
  data = results$species_gamData,
  triasFunction = "apply_gam",
  translationId = "apply_gamObservations",
  triasArgs = reactive({
      list(
        y_var = "obs", 
        taxon_key = input$species_choice, 
        name = taxonName(),
        x_label = translate("year")$title,
        y_label = translate("observations")$title,
        region = input$species_gewest
      )
    }),
  filters = reactive(list(
    correctBias = list(type = "checkbox"), 
    protectAreas = list(type = "checkbox")
  )),
  dashReport = dashReport,
  triggerReport = species_createReport
)


## Emergence status GAM - Occupancy
dashReport <- plotTriasServer(id = "indicators_gamOccupancy",
  data = results$species_gamData,
  triasFunction = "apply_gam",
  translationId = "apply_gamOccupancy",
  triasArgs = reactive({
      list(
        y_var = "ncells", 
        taxon_key = input$species_choice, 
        name = taxonName(),
        x_label = translate("year")$title,
        y_label = translate("occupancy")$title,
        region = input$species_gewest
      )
    }),
  filters = reactive(list(
    correctBias = list(type = "checkbox"), 
    protectAreas = list(type = "checkbox")
  )),
  dashReport = dashReport,
  triggerReport = species_createReport
)


## Invasion history
dashReport <- mapRegionsServer(id = "indicators_facet",
  species = taxonName,
  gewest = reactive(req(input$species_gewest)),
  regionLevels = c("communes", "provinces", "cell_code1", "cell_code10"),
  df = reactive(occurrenceData[taxonKey %in% input$species_choice, ]),
  occurrenceData = NULL,
  shapeData = allShapes,
  facet = TRUE,
  dashReport = dashReport,
  triggerReport = species_createReport
)

### Reporting
### -----------------


# Disable tab if no info
observe({
    
    req(input$species_choice)
    
    # https://stackoverflow.com/a/64324799
    shinyjs::toggleState(
      selector = '#species_tabs a[data-value="species_reporting"', 
      condition = taxonName() %in% dfCube$species
    )
    
    if (!(taxonName() %in% dfCube$species) & input$species_tabs == "species_reporting")
      updateTabsetPanel(session = session, inputId = "species_tabs", 
        selected = "species_observations")
    
  })


# t0 and t1
dashReport <- mapCubeServer(id = "reporting_t01",
  species = taxonName,
  gewest = reactive(req(input$species_gewest)),
  df = reactive(dfCube[dfCube$species %in% taxonName(), ]),
  filter = reactive(list(source = unique(dfCube$source[dfCube$species %in% taxonName()]))),
  groupVariable = "source",
  shapeData = allShapes,
  dashReport = dashReport,
  triggerReport = species_createReport
)



### Management
### ----------------

# Species for which to show mapCube output
cubeSpecies <- c("Oxyura jamaicensis")
# Species for which to show heatMap output
heatSpecies <- c("Vespa velutina")
# Other species will have mapRegions output
# e.g. Ondatra zibethicus

results$species_managementFile <- reactive({
    
    req(taxonName())
    expectFile <- if (taxonName() == "Vespa velutina")  # exclude Vespa velutina mgt #121
        "" else if (taxonName() %in% heatSpecies)
        paste0(gsub(" ", "_", taxonName()), "_shape.RData") else 
        gsub(" ", "_", paste0(taxonName(), ".csv"))
    availableFiles <- aws.s3::get_bucket_df(
      bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")))$Key
    
    if (expectFile %in% availableFiles)
      expectFile else 
      NULL
    
  })

# Disable tab if no info
observe({
    
    req(taxonName())
    
    # https://stackoverflow.com/a/64324799
    shinyjs::toggleState(
      selector = '#species_tabs a[data-value="species_management"', 
      condition = !is.null(results$species_managementFile())
    )
    
    if (is.null(results$species_managementFile()) & input$species_tabs == "species_management")
      updateTabsetPanel(session = session, inputId = "species_tabs", 
        selected = "species_observations")
    
  })

results$species_managementData <- reactive({
    
    req(taxonName())
    
    validate(need(results$species_managementFile(), translate("noData")$title))
    
    if (taxonName() %in% heatSpecies) {
      
      readS3(file = results$species_managementFile())
            
      base::get(paste0(gsub(" ", "_", heatSpecies), "_shape"))
            
    } else {
      
      loadGbif(dataFile = results$species_managementFile())
      
    }
    
  })


# TODO rmd per management type? 
# https://stackoverflow.com/a/33500524
# https://bookdown.org/yihui/rmarkdown/shiny-args.html
observe({
    
    req(results$species_managementData())
    
    if (taxonName() %in% cubeSpecies) {
      ## Map + slider barplot: Oxyura jamaicensis
      
      dashReport <- mapCubeServer(id = "management",
        species = taxonName,
        gewest = reactive(req(input$species_gewest)),
        df = results$species_managementData,
        filter = reactive({
            filterCandidates <- c("gender", "samplingProtocol", "lifeStage")
            filters <- filterCandidates[filterCandidates %in% colnames(results$species_managementData())]
            sapply(filters, function(iFilter)
                sort(unique(results$species_managementData()[[iFilter]])),
              simplify = FALSE)
          }),
        groupVariable = NULL,
        shapeData = NULL,
        showPeriod = TRUE,
        dashReport = dashReport,
        triggerReport = species_createReport
      )
      
    } else if (taxonName() %in% heatSpecies) {
      ## heatmap: Vespa velutina
      
      ## Actieve haarden
      combinedActive <- combineActiveData(
        activeData = results$species_managementData()$actieve_haarden,
        untreatedData = results$species_managementData()$onbehandelde_nesten
      )
      colorsActive <- c("blue", "black")
      names(colorsActive) <- c("individual", "untreated nest")
      
      dashReport <- mapHeatServer(id = "management2_active",
        species = taxonName,
        gewest = reactive(req(input$species_gewest)),
        combinedData = reactive(combinedActive),
        filter = reactive(list(
            nest = unique(combinedActive$filter), 
            radius = na.omit(unique(combinedActive$radius))
          )),
        colors = reactive(colorsActive),
        blur = "individual",
        maxDate = reactive({
            req(results$species_managementData())
            max(results$species_managementData()$actieve_haarden$eventDate, na.rm = TRUE)
          }) ,
        dashReport = dashReport,
        triggerReport = species_createReport
      )
      
      ## Alle observaties
      combinedObserved <- combineNestenData(
        pointsData = results$species_managementData()$points, 
        nestenData = results$species_managementData()$nesten
      # For testing only: when no observations yet, use latest available year
#        currentYear = format(max(results$species_managementData()$points$eventDate, na.rm = TRUE), "%Y")
      )
      colorsObserved <- c("blue", "red")
      names(colorsObserved) <- c("individual", "nest")
      
      dashReport <- mapHeatServer(id = "management2_observed",
        species = taxonName,
        gewest = reactive(req(input$species_gewest)),
        combinedData = reactive(combinedObserved),
        filter = reactive(list(source = unique(combinedObserved$filter))),
        colors = reactive(colorsObserved),
        maxDate = reactive({
            req(results$species_managementData()$points$eventDate)
            max(results$species_managementData()$points$eventDate, na.rm = TRUE)
          }),
        dashReport = dashReport,
        triggerReport = species_createReport
      )
      
      # Trend region
      combinedManaged <- combineVespaData(
        pointsData = req(results$species_managementData()$points),
        nestenData = req(results$species_managementData()$nesten),
        nestenBeheerdData = results$species_managementData()$beheerde_nesten
      )
      dashReport <- mapRegionsServer(
        id = "management2",
        species = taxonName,
        gewest = reactive(req(input$species_gewest)),
        df = reactive(combinedManaged),
        occurrenceData = NULL,
        shapeData = allShapes,
        filter = reactive({
            filterCandidates <- c("isBeheerd", "nest_type")
            filters <- filterCandidates[filterCandidates %in% colnames(combinedManaged)]
            sapply(filters, function(iFilter)
                sort(unique(combinedManaged[[iFilter]])),
              simplify = FALSE)
          }),
        dashReport = dashReport,
        triggerReport = species_createReport
      )
      
      # Aantal lente nesten
      dashReport <- plotTriasServer(
        id = "management2_lente",
        triasFunction = "barplotLenteNesten",
        data = reactive(aws.s3::s3read_using(FUN = read.csv, 
            object = "aantal_lente_nesten.csv",
            bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies"))
          )),
          #read.csv(system.file("extdata", "management", "Vespa_velutina", "aantal_lente_nesten.csv", package = "alienSpecies"))
        dashReport = dashReport,
        triggerReport = species_createReport
      )
      
     
      # Aantal nesten per provincie - figuur
      dashReport <- countNestenServer(
        id = "management2_province",
        data = reactive(results$species_managementData()$nesten),
        maxDate = reactive({
            req(results$species_managementData())
            max(results$species_managementData()$nesten$observation_time, na.rm = TRUE)
          }),
        dashReport = dashReport,
        triggerReport = species_createReport
      )
      
      # Aantal nesten per provincie - tabel
      dashReport <- plotTriasServer(
        id = "management2_provinceTable",
        triasFunction = "tableNesten",
        data = reactive(results$species_managementData()$nesten),
        maxDate = reactive(max(results$species_managementData()$nesten$observation_time, na.rm = TRUE)),
        outputType = "table",
        dashReport = dashReport,
        triggerReport = species_createReport
      )
      
      dashReport <- countYearGroupServer(
        id = "management2", 
        species = taxonName,
        data = reactive({
            req(results$species_managementData())
            summarizeYearGroupData(
              df = results$species_managementData()$nesten, 
              gewest = input$species_gewest)
          }),
        groupChoices = reactive({
            choices <- c("", "Behandeling")
            names(choices) <- c("", translate(choices[-1])$title)
            choices
          }),
        dashReport = dashReport,
        triggerReport = species_createReport
      )
      
    } else {
      ## Map + choices barplot: Lithobates catesbeianus
      
      dashReport <- mapRegionsServer(
        id = "management3",
        species = taxonName,
        gewest = reactive(req(input$species_gewest)),
        df = results$species_managementData,
        occurrenceData = occurrenceData,
        shapeData = allShapes,
        dashReport = dashReport,
        triggerReport = species_createReport
      )
      
      dashReport <- countYearGroupServer(
        id = "management3", 
        species = taxonName,
        data = results$species_managementData,
        groupChoices = reactive({
            choices <- c("", "lifeStage")
            names(choices) <- c("", translate(choices[-1])$title)
            choices
          }),
        dashReport = dashReport,
        triggerReport = species_createReport
      )
    } 
    
  })

output$species_managementContent <- renderUI({
    
    req(results$species_managementData())
    
    # Important: different ids needed, otherwise there is communication between both cases
    # e.g. input$legend exists for both
    if (taxonName() %in% cubeSpecies) {
      
      mapCubeUI(id = "management", showPeriod = TRUE, showLegend = FALSE)
      
    } else if (taxonName() %in% heatSpecies) {
      
      isSeason <- Sys.Date() >= as.Date(paste0("01-04-", format(Sys.Date(), "%Y")), format = "%d-%m-%Y") &
        Sys.Date() < as.Date(paste0("01-12-", format(Sys.Date(), "%Y")), format = "%d-%m-%Y")
      
      tagList(
        tags$a(href = "https://vespawatch.be/", target = "_blank",
          tags$img(src = 'logo_vespawatch.png', height = 50)),
        if (isSeason) 
            mapHeatUI(id = "management2_active") else 
            tags$div(class = "container",
              h3(HTML(translate("management2_active-mapHeat")$title)),
              helpText(translate("disclaimerVespa")$title)
            ),
        mapHeatUI(id = "management2_observed"),
        mapRegionsUI(id = "management2", plotDetails = c("flanders", "region"), showUnit = FALSE),
        plotTriasUI(id = "management2_lente"),
        countNestenUI(id = "management2_province"),
        plotTriasUI(id = "management2_provinceTable", outputType = "table", exportGraph = FALSE),
        countYearGroupUI(id = "management2", showPlotDefault = TRUE)
      )
      
    } else {
      
      tagList(
        mapRegionsUI(id = "management3", plotDetails = c("flanders", "region")),
        countYearGroupUI(id = "management3", showPlotDefault = TRUE)
      )
      
    }
    
  })

### More
### ----------------

# Disable tab if no info
observe({
    
    req(input$species_choice)
    
    # https://stackoverflow.com/a/64324799
    
    # Conditionally enable 'More'
    moreChoices <- unique(c(keysRiskMap, keysLinks, harmoniaData$gbif_taxonkey))
    shinyjs::toggleState(
      selector = '#species_tabs a[data-value="species_more"', 
      condition = input$species_choice %in% moreChoices
    )
    # Risk maps
    shinyjs::toggleState(
      selector = '#species_more a[data-value="species_risk_maps"', 
      condition = input$species_choice %in% keysRiskMap
    )
    # All other subpanels
    shinyjs::toggleState(
      selector = '#species_more a[data-value="species_habitats"', 
      condition = FALSE
    )
    shinyjs::toggleState(
      selector = '#species_more a[data-value="species_links"', 
      condition = input$species_choice %in% keysLinks
    )
    shinyjs::toggleState(
      selector = '#species_more a[data-value="species_risk_management"', 
      condition = input$species_choice %in% harmoniaData$gbif_taxonkey
    )
    shinyjs::toggleState(
      selector = '#species_more a[data-value="species_images"', 
      condition = FALSE
    )
    
    if (input$species_choice %in% moreChoices)
      updateTabsetPanel(session = session, inputId = "species_more", 
        selected = if (input$species_choice %in% keysRiskMap)
            "species_risk_maps" else if (input$species_choice %in% keysLinks)
            "species_links" else if (input$species_choice %in% harmoniaData$gbif_taxonkey)
            "species_risk_management") else
      updateTabsetPanel(session = session, inputId = "species_tabs", 
        selected = if (!is.null(urlSearch()$tab)) 
            urlSearch()$tab else 
            "species_observations")
  
  })

# Risk maps
## test with "Psittacula krameri"
observe({
    
    req(input$species_choice)
    
    mapRasterServer(
      id = "risk", 
      species = taxonName,
      gewest = reactive(input$species_gewest),
      taxonKey = reactive(input$species_choice)
    )
    
  })


# Links
## test with "Vespa velutina"
observe({
    
    req(input$species_choice)
    
    htmlSectionServer(id = "links", species = reactive(input$species_choice),
      language = reactive(results$language))
    
  })

# Risk assessment
## test with "Psittacula krameri"
observe({
    
    req(input$species_choice)
    
    matchingLinks <- which( harmoniaData$gbif_taxonkey == input$species_choice)
    
    htmlSectionServer(
      id = "risk_assessment", 
      species = reactive(input$species_choice),
      language = reactive(results$language),
      url = harmoniaData$url[matchingLinks],
      linkText = sapply(matchingLinks, function(iLink)
          switch(harmoniaData$url_type[iLink],
            harmonia = "Harmonia+ Risk Assessment",
            iasregulation = "IAS Regulation",
            harmoniaData$url_type[iLink])
      )
    )
    
  })


## Risk management
#output$species_riskManagement <- renderUI({
#    
#    req(input$species_choice)
#    # Refused to frame 'https://ias.biodiversity.be/' because an ancestor violates the following Content Security Policy directive: "frame-ancestors 'self'"
#    tags$iframe(src = harmoniaData$harmonia_url[match(input$species_choice, harmoniaData$gbif_taxonkey)])
#    
#  })


## SUBMIT & DOWNLOAD report ##
species_createReport <- footerSectionServer(id = "species")

species_reportFile <- reactiveVal()

observeEvent(species_createReport(), priority = 5, {
    
    showNotification(paste(translate(id = "createReport")$title, '...\n'),
      id = "reportWait", type = "message", duration = NULL)
    
    species_reportFile(NULL)  # reset on each button press
    
  })

species_readyForDownload <- reactive({
    
    req(is.null(species_reportFile()))
    
    # Wait for mgt output to be ready
    if (!is.null(results$species_managementFile())) {
      removeNotification(id = "reportWait")
      validate(need(any(grepl("management", names(dashReport))), "Please wait"))
    }
    
    removeNotification(id = "reportWait")   
    
    return(species_createReport())
    
  })

observeEvent(species_readyForDownload(), {
    
    withProgress(
      message = paste(translate(id = "createReport")$title, '...\n'), 
      value = 0, {
        
        oldDir <- getwd()
        setwd(tempdir())
        on.exit(setwd(oldDir))
        
        header_file <- switch(results$language,
          "en" = "header_en.yml",
          "nl" = "header_nl.yml",
          "fr" = "header_fr.yml")
        
        fromFiles <- system.file("app/www", c(
            "index.Rmd", 
            "plotSpecies.Rmd",
            "plotLandscape.Rmd",
            "logo.png",
            "logoTrias.png"
          ), package = "alienSpecies")
        toFiles <- file.path(tempdir(), basename(fromFiles))
        for (i in seq_along(fromFiles)) {
          src <- fromFiles[i]
          dest <- toFiles[i]
          
          if (basename(src) == "index.Rmd") {  # Add custom header based on language
            header_lines <- readLines(system.file("app/www", header_file, package = "alienSpecies"))
            rmd_lines <- readLines(src)
            combined <- c(header_lines, "", rmd_lines)
            writeLines(combined, dest)
          } else {
            file.copy(from = src, to = dest, overwrite = TRUE)
          }
        }
        
        tmpReport <- tempfile(fileext = ".pdf")
        suppressWarnings({rmarkdown::render(
          input = file.path(tempdir(), basename(fromFiles[1])),
          output_file = tmpReport,
          intermediates_dir = tempdir(),
          output_options = list(
            bigLogo = getPathLogo(type = "combined")
          ),
          quiet = TRUE
        )})
        
        finalReport <- tempfile(fileext = ".pdf")
        qpdf::pdf_subset(tmpReport, pages = 2:qpdf::pdf_length(tmpReport), output = finalReport)
        
        species_reportFile(finalReport)
        
        session$sendCustomMessage(type = "imageReady", 
          message = list(id = "species-downloadReport"))
        
        # Reset report content - if switching species
        for (iName in names(dashReport))
          dashReport[[iName]] <- NULL
        
      })
    
  })


# Specific id for JS trigger in shiny module footerSectionUI()
output$`species-downloadReport` <- downloadHandler(
  filename = function() 
    nameFile(species = taxonName(), content = "report", fileExt = "pdf"),
  content = function(file) 
    file.copy(species_reportFile(), file, overwrite = TRUE)
)

