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


output$species_title <- renderUI({
    
    translate(data = results$translations, id = tabChoices[3])$title  
    
  })

lapply(c("observations", "indicators", "reporting", "management", "more",
    "habitats", "risk_maps", "links", "risk_assessment", "images"), function(iName)
    titleModuleServer(
      id = paste0("species_", iName),
      uiText = reactive(results$translations),
      plotFunction = iName
    ))

welcomeSectionServer(id = "species", uiText = reactive(results$translations))


# Species selection
results$species_choices <- reactive({
    
    # Observations
    taxChoices <- occurrenceData[!duplicated(taxonKey), scientificName]
    # Reporting
    reportChoices <- dfCube[!duplicated(species) & !species %in% taxChoices, species]
    
    sort(c(taxChoices, reportChoices))
        
  })


observe({
    
    # Trigger update when changing tab
    if (input$tabs == "species_information")
      updateSelectizeInput(session = session, inputId = "species_choice",
        choices = results$species_choices(),
        selected = if (!is.null(urlSearch()$species))
          urlSearch()$species else 
          as.character(results$species_choice),
        server = TRUE)    
    
  })


# Gewest selection
observe({
    
    choices <- c("flanders", "wallonia", "brussels")
    names(choices) <- translate(results$translations, choices)$title
    
    # Trigger update when changing tab
    if (input$tabs == "species_information")
      updateSelectInput(session = session, inputId = "species_gewest", 
      choices = choices,
      selected = if (!is.null(urlSearch()$gewest)) 
          strsplit(urlSearch()$gewest, split = ",")[[1]] else 
          choices)
            
})


# Update search ID
observe({
            
    results$searchId <- paste0("&species=", input$species_choice, 
      "&gewest=", paste(input$species_gewest, collapse = ","))
            
  })



### Observations
### -----------------

# Taxonkey of selected species
taxonKey <- reactive({
    
    req(input$species_choice)
    dictionary$taxonKey[match(input$species_choice, dictionary$scientificName)]
    
  })

# Disable tab if no info
observe({
    
    # https://stackoverflow.com/a/64324799
    shinyjs::toggleState(
      selector = '#species_tabs a[data-value="species_observations"', 
      condition = !is.na(taxonKey())
    )
    
    if (is.na(taxonKey()) & input$species_tabs == "species_observations")
      updateTabsetPanel(session = session, inputId = "species_tabs", 
        selected = "species_reporting")
    
  })


## Map + barplot
dashReport <- mapCubeServer(id = "observations",
  uiText = reactive(results$translations),
  species = reactive(input$species_choice),
  gewest = reactive(req(input$species_gewest)),
  df = reactive({
      req(taxonKey())
      occurrenceData[taxonKey %in% taxonKey(), ]      
    }),
  groupVariable = "cell_code",
  shapeData = allShapes,
  showPeriod = TRUE,
  dashReport = dashReport
)




### Indicators
### -----------------


# Disable tab if no info
observe({
    
    # https://stackoverflow.com/a/64324799
    shinyjs::toggleState(
      selector = '#species_tabs a[data-value="species_indicators"', 
      condition = !is.na(taxonKey())
    )
    
    if (is.na(taxonKey()) & input$species_tabs == "species_indicators")
      updateTabsetPanel(session = session, inputId = "species_tabs", 
        selected = "species_reporting")
    
  })


## Emergence status GAM - Observations
dashReport <- plotTriasServer(id = "species_gam",
  uiText = reactive(results$translations),
  data = reactive({
      req(taxonKey())
      summarizeTimeSeries(rawData = timeseries[taxonKey %in% taxonKey(), ], 
        region = input$species_gewest)
    }),
  triasFunction = "apply_gam",
  triasArgs = reactive({
      list(
        y_var = "obs", 
        taxon_key = taxonKey(), 
        name = input$species_choice,
        x_label = translate(results$translations, "year")$title,
        y_label = translate(results$translations, "observations")$title
      )
    }),
  filters = c("bias", "protected"),
  dashReport = dashReport
)


### Reporting
### -----------------


# Disable tab if no info
observe({
    
    req(input$species_choice)
    
    # https://stackoverflow.com/a/64324799
    shinyjs::toggleState(
      selector = '#species_tabs a[data-value="species_reporting"', 
      condition = input$species_choice %in% dfCube$species
    )
    
    if (!(input$species_choice %in% dfCube$species) & input$species_tabs == "species_reporting")
      updateTabsetPanel(session = session, inputId = "species_tabs", 
        selected = "species_observations")
    
  })


# t0 and t1
dashReport <- mapCubeServer(id = "reporting_t01",
  uiText = reactive(results$translations),
  species = reactive(input$species_choice),
  gewest = reactive(req(input$species_gewest)),
  df = reactive(dfCube[species %in% input$species_choice, ]),
  filter = reactive(list(source = unique(dfCube$source[dfCube$species %in% input$species_choice]))),
  groupVariable = "source",
  shapeData = allShapes,
  dashReport = dashReport
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
    
    req(input$species_choice)
    expectFile <- if (input$species_choice %in% heatSpecies)
        paste0(gsub(" ", "_", input$species_choice), "_shape.RData") else 
        gsub(" ", "_", paste0(input$species_choice, ".csv"))
    availableFiles <- aws.s3::get_bucket_df(
      bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")))$Key
    
    if (expectFile %in% availableFiles)
      expectFile else 
      NULL
    
  })

# Disable tab if no info
observe({
    
    req(input$species_choice)
    
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
    
    req(input$species_choice)
    
    validate(need(results$species_managementFile(), translate(results$translations, "noData")$title))
    
    if (input$species_choice %in% heatSpecies) {
      
      readS3(file = results$species_managementFile())
            
      base::get(paste0(gsub(" ", "_", heatSpecies), "_shape"))
            
    } else {
      
      tmpData <- loadGbif(dataFile = results$species_managementFile())
      
      if (!"GEWEST" %in% colnames(tmpData) && "NISCODE" %in% colnames(tmpData)){
        tmpData$GEWEST <- allShapes$communes$GEWEST[
          match(tmpData$NISCODE, allShapes$communes$NISCODE)]
        tmpData <- tmpData[tmpData$GEWEST %in% input$species_gewest, ]
      }
      
      tmpData
      
    }
    
  })


# TODO rmd per management type? 
# https://stackoverflow.com/a/33500524
# https://bookdown.org/yihui/rmarkdown/shiny-args.html
observe({
    
    req(results$species_managementData())
    
    if (input$species_choice %in% cubeSpecies) {
      ## Map + slider barplot: Oxyura jamaicensis
      
      dashReport <- mapCubeServer(id = "management",
        uiText = reactive(results$translations),
        species = reactive(input$species_choice),
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
        dashReport = dashReport
      )
      
    } else if (input$species_choice %in% heatSpecies) {
      ## heatmap: Vespa velutina
      
      ## Actieve haarden
      combinedActive <- combineActiveData(
        activeData = results$species_managementData()$actieve_haarden,
        untreatedData = results$species_managementData()$onbehandelde_nesten
      )
      colorsActive <- c("blue", "black")
      names(colorsActive) <- c("individual", "untreated nest")
      
      dashReport <- mapHeatServer(id = "management2_active",
        uiText = reactive(results$translations),
        species = reactive(input$species_choice),
        gewest = reactive(req(input$species_gewest)),
        combinedData = reactive(combinedActive),
        filter = reactive(list(
            nest = unique(combinedActive$filter), 
            radius = na.omit(unique(combinedActive$radius))
          )),
        colors = reactive(colorsActive),
        blur = "individual",
        maxDate = reactive(max(results$species_managementData()$actieve_haarden$eventDate, na.rm = TRUE)) ,
        dashReport = dashReport
      )
      
      ## Alle observaties
      combinedObserved <- combineNestenData(
        pointsData = results$species_managementData()$points, 
        nestenData = results$species_managementData()$nesten,
        uiText = results$translations
      # For testing only: when no observations yet, use latest available year
#        currentYear = format(max(results$species_managementData()$points$eventDate, na.rm = TRUE), "%Y")
      )
      colorsObserved <- c("blue", "red")
      names(colorsObserved) <- c("individual", "nest")
      
      dashReport <- mapHeatServer(id = "management2_observed",
        uiText = reactive(results$translations),
        species = reactive(input$species_choice),
        gewest = reactive(req(input$species_gewest)),
        combinedData = reactive(combinedObserved),
        filter = reactive(list(source = unique(combinedObserved$filter))),
        colors = reactive(colorsObserved),
        maxDate = reactive(max(results$species_managementData()$points$eventDate, na.rm = TRUE)),
        dashReport = dashReport
      )
      
      # Trend region
      combinedManaged <- combineVespaData(
        pointsData = req(results$species_managementData()$points),
        nestenData = req(results$species_managementData()$nesten),
        nestenBeheerdData = results$species_managementData()$beheerde_nesten
      )
      dashReport <- mapRegionsServer(
        id = "management2",
        uiText = reactive(results$translations),
        species = reactive(input$species_choice),
        gewest = reactive(req(input$species_gewest)),
        df = reactive(combinedManaged),
        occurrenceData = NULL,
        shapeData = allShapes,
        sourceChoices = c("individual", "nest"),
        dashReport = dashReport
      )
      
      # Facet invasion
      dashReport <- mapRegionsServer(id = "management2_facet",
        uiText = reactive(results$translations),
        species = reactive(input$species_choice),
        gewest = reactive(req(input$species_gewest)),
        df = reactive(combinedManaged),
        occurrenceData = NULL,
        shapeData = allShapes,
        facet = TRUE,
        dashReport = dashReport
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
        uiText = reactive(results$translations),
        dashReport = dashReport
      )
      
     
      # Aantal nesten per provincie - figuur
      dashReport <- countNestenServer(
        id = "management2_province",
        data = reactive(results$species_managementData()$nesten),
        uiText = reactive(results$translations),
        maxDate = reactive(max(results$species_managementData()$nesten$observation_time, na.rm = TRUE)),
        dashReport = dashReport
      )
      
      # Aantal nesten per provincie - tabel
      dashReport <- plotTriasServer(
        id = "management2_provinceTable",
        triasFunction = "tableNesten",
        data = reactive(results$species_managementData()$nesten),
        uiText = reactive(results$translations),
        maxDate = reactive(max(results$species_managementData()$nesten$observation_time, na.rm = TRUE)),
        outputType = "table",
        dashReport = dashReport
      )
      
      dashReport <- countYearGroupServer(
        id = "management2", 
        uiText = reactive(results$translations), 
        data = reactive(summarizeYearGroupData(
            df = results$species_managementData()$nesten, 
            gewest = input$species_gewest)),
        groupChoices = reactive({
            choices <- c("", "Behandeling")
            names(choices) <- c("", translate(results$translations, choices[-1])$title)
            choices
          }),
        dashReport = dashReport
      )
      
    } else {
      ## Map + choices barplot: Lithobates catesbeianus
      
      dashReport <- mapRegionsServer(
        id = "management3",
        uiText = reactive(results$translations),
        species = reactive(input$species_choice),
        gewest = reactive(req(input$species_gewest)),
        df = results$species_managementData,
        occurrenceData = occurrenceData,
        shapeData = allShapes,
        dashReport = dashReport
      )
      
      # Facet invasion
      dashReport <- mapRegionsServer(id = "management3_facet",
        uiText = reactive(results$translations),
        species = reactive(input$species_choice),
        gewest = reactive(req(input$species_gewest)),
        df = results$species_managementData,
        occurrenceData = NULL,
        shapeData = allShapes,
        facet = TRUE,
        dashReport = dashReport
      )
      
      dashReport <- countYearGroupServer(
        id = "management3", 
        uiText = reactive(results$translations), 
        data = results$species_managementData,
        groupChoices = reactive({
            choices <- c("", "lifeStage")
            names(choices) <- c("", translate(results$translations, choices[-1])$title)
            choices
          }),
        dashReport = dashReport
      )
    } 
    
  })

output$species_managementContent <- renderUI({
    
    req(results$species_managementData())
    
    # Important: different ids needed, otherwise there is communication between both cases
    # e.g. input$legend exists for both
    if (input$species_choice %in% cubeSpecies) {
      
      mapCubeUI(id = "management", showPeriod = TRUE, showLegend = FALSE)
      
    } else if (input$species_choice %in% heatSpecies) {
      
      isSeason <- Sys.Date() >= as.Date(paste0("01-04-", format(Sys.Date(), "%Y")), format = "%d-%m-%Y") &
        Sys.Date() < as.Date(paste0("01-12-", format(Sys.Date(), "%Y")), format = "%d-%m-%Y")
      
      tagList(
        tags$a(href = "https://vespawatch.be/", target = "_blank",
          tags$img(src = 'logo_vespawatch.png', height = 50)),
        if (isSeason) 
            mapHeatUI(id = "management2_active") else 
            tags$div(class = "container",
              h3(HTML(translate(results$translations, "management2_active-mapHeat")$title)),
              helpText(translate(results$translations, "disclaimerVespa")$title)
            ),
        mapHeatUI(id = "management2_observed"),
        mapRegionsUI(id = "management2", plotDetails = c("flanders", "region"), showUnit = FALSE),
        mapRegionsUI(id = "management2_facet", showUnit = FALSE, facet = TRUE),
        plotTriasUI(id = "management2_lente"),
        countNestenUI(id = "management2_province"),
        plotTriasUI(id = "management2_provinceTable", outputType = "table"),
        countYearGroupUI(id = "management2")
      )
      
    } else {
      
      tagList(
        mapRegionsUI(id = "management3", plotDetails = c("flanders", "region")),
        mapRegionsUI(id = "management3_facet", showUnit = FALSE, facet = TRUE),
        countYearGroupUI(id = "management3")
      )
      
    }
    
  })

### More
### ----------------

# Disable tab if no info
observe({
    
    req(taxonKey())
    
    # https://stackoverflow.com/a/64324799
    
    # Conditionally enable 'More'
    shinyjs::toggleState(
      selector = '#species_tabs a[data-value="species_more"', 
      condition = taxonKey() %in% keysRiskMap
    )
    # Risk maps
    shinyjs::toggleState(
      selector = '#species_more a[data-value="species_risk_maps"', 
      condition = taxonKey() %in% keysRiskMap
    )
    # All other subpanels
    shinyjs::toggleState(
      selector = '#species_more a[data-value="species_habitats"', 
      condition = FALSE
    )
    shinyjs::toggleState(
      selector = '#species_more a[data-value="species_links"', 
      condition = FALSE
    )
    shinyjs::toggleState(
      selector = '#species_more a[data-value="species_risk_management"', 
      condition = FALSE
    )
    shinyjs::toggleState(
      selector = '#species_more a[data-value="species_images"', 
      condition = FALSE
    )
    
  })


observe({
    
    req(taxonKey())
    
    mapRasterServer(
      id = "risk", 
      uiText = reactive(results$translations),
      species = reactive(input$species_choice),
      taxonKey = taxonKey
    )
    
  })



## SUBMIT & DOWNLOAD report ##

species_reportFile <- reactiveVal()

observe({
    
    updateActionButton(inputId = "species_createReport", 
      label = translate(data = results$translations, id = "createReport")$title)
    
  })

observeEvent(input$species_createReport, {
    
    species_reportFile(NULL)  # reset on each button press
    
    withProgress(message = paste(translate(data = results$translations, id = "createReport")$title, '...\n'), value = 0, {
        
        oldDir <- getwd()
        setwd(tempdir())
        on.exit(setwd(oldDir))
        
        fromFiles <- system.file("app/www", c("reportSpecies.Rmd", "plotSpecies.Rmd"), package = "alienSpecies")
        file.copy(from = fromFiles, to = file.path(tempdir(), basename(fromFiles)), overwrite = TRUE)
        
        species_reportFile(
          rmarkdown::render(
            input = file.path(tempdir(), basename(fromFiles[1])),
            output_file = tempfile(fileext = ".pdf"),
            intermediates_dir = tempdir(),
            output_options = list(
              bigLogo = getPathLogo(type = "combined")
            )
          )
        )
        
        # report is ready, trigger download
        setProgress(1)
        
        session$sendCustomMessage(type = "imageReady", 
          message = list(id = "species_downloadReport"))
        
      })
    
  })


output$species_downloadReport <- downloadHandler(
  filename = function() 
    nameFile(species = input$species_choice, content = "report", fileExt = "pdf"),
  content = function(file) 
    file.copy(species_reportFile(), file, overwrite = TRUE)
)

