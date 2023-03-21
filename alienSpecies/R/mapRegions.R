
#' Create summary data per region
#' @param data data.table with management data
#' @param shapeData list, each object is \code{SpatialPolygonsDataFrame}. 
#' Needed for the \code{regionLevel} specified
#' @param regionLevel character, should be one of \code{c("communes", "provinces")}
#' @param year integer, year of interest
#' @param unit character, should be one of \code{c("cpue", "absolute")},
#' catch per unit of effort or absolute count
#' 
#' @return data.frame
#' 
#' @author mvarewyck
#' @import tidyverse
#' @importFrom data.table copy
#' @export
createSummaryRegions <- function(data, shapeData, 
  regionLevel = c("communes", "provinces"),
  year = NULL, unit = c("cpue", "absolute")) {
  
  # For R CMD check
  region <- NULL
  eventID <- NULL
  n_fuiken <- NULL
  count <- NULL
  effort <- NULL
  n <- NULL
  
  unit <- match.arg(unit)
  
  data <- as.data.frame(data)
  
  if (is.null(year))
    myYear <- unique(data$year) else 
    myYear <- year  # rename for tidyverse filtering
  
  regionVar <- switch(regionLevel,
    communes = "NAAM",
    provinces = "provincie"
  )
  
  if (regionLevel == "flanders")
    data$region <- "flanders" else if (!regionVar %in% colnames(data))
    return(NULL) else
    colnames(data)[colnames(data) == regionVar] <- "region"
  
  if (unit == "cpue") {
    
    summaryData <- data %>% 
      filter(year %in% myYear, !is.na(region), region != "NA") %>% 
      group_by(eventID, region, year) %>% 
      summarise(effort = max(n_fuiken, na.rm = TRUE),
        n = sum(count, na.rm = TRUE)/effort) %>% 
      group_by(region, year) %>% 
      summarise(effort = sum(effort, na.rm = TRUE),
        n = sum(n, na.rm = TRUE))
    
    summaryData$group <- cut(x = summaryData$effort, 
      breaks = c(0, 10, 100, 200, 300, 400, Inf),
      labels = c("1-10", "11-100", "101-200", "201-300", "300-400", 
        paste0("401-", max(500, ceiling(max(summaryData$n)))))
    )
    
    summaryData$outcome <- summaryData$effort
    
  } else {
    
    summaryData <- data %>%
      filter(year %in% myYear, !is.na(region), region != "NA") %>% 
      group_by(region, year) %>% 
      summarise(n = sum(count, na.rm = TRUE))
      
      
    summaryData$group <- cut(x = summaryData$n, 
      breaks = c(0, 1000, 5000, 10000, Inf),
      labels = c("1-1000", "1001-5000", "5001-10000", 
        paste0("10001-", max(50000, ceiling(max(summaryData$n)))))
    )
    
    summaryData$outcome <- ceiling(summaryData$n)
    
  }
  
  # Add names & times with 0 observations
  fullData <- cbind(expand.grid(
      year = myYear,
      region = if (regionLevel == "flanders") 
          "flanders" else 
          unique(shapeData[[regionLevel]]@data$NAAM)))
  allData <- merge(summaryData, fullData, all.x = TRUE, all.y = TRUE)
  allData$outcome[is.na(allData$outcome)] <- 0
  
  attr(allData, "unit") <- unit
  
  return(allData)
  
}


#' Map with occurrence and management for single species
#' @param managementData data.frame, management data
#' @param occurrenceData data.frame, occurrence data
#' @param shapeData list with spatial data (grid and regions)
#' @param uiText data.frame, for translations
#' @param regionLevel character, region level to color polygons
#' @param legend character, where to place legend
#' @param addGlobe boolean, whether to have background map
#' @return leaflet object
#' 
#' @author mvarewyck
#' @import leaflet
#' @export
mapRegions <- function(managementData, occurrenceData, shapeData, uiText = NULL, 
  regionLevel = c("communes", "provinces"),
  legend = "topright", addGlobe = FALSE) {
  
  spatialData <- shapeData[[regionLevel]]
  
  palette <- colorFactor(palette = "YlOrBr", levels = levels(managementData$group), 
    na.color = "transparent")
  valuesPalette <- managementData$group[match(spatialData@data$NAAM, managementData$region)]
  
  spread <- createCubeData(df = occurrenceData, shapeData = shapeData,
    groupVariable = "cell_code")
  
# MAP: Bullfrog management in `r jaar`
#  Suggested filters:
#  - year
#  - region-scale (province/commune)
#  - unit (absolute/cpue)
  
  
  myMap <- leaflet(spatialData) %>%
    addPolylines(data = spread$cell_code1, 
      color = "red",
      weight = 1) %>% 
    addPolygons(
      weight = 1,
      color = "gray",
      fillColor = ~ palette(valuesPalette),
      fillOpacity = 0.8,
      layerId = spatialData@data$NAAM,
      group = "region"
    )
  
  # Add provinces borders
  if (regionLevel == "communes") {
    
    myMap <- myMap %>% 
      addPolylines(
        data = shapeData$provinces, 
        weight = 3,
        color = "black",
        opacity = 0.8,
        group = "borderRegion"
    )
  
  }
  
  # Add legend
  if (legend != "none") { 
    
    myMap <- addLegend(
      map = myMap,
      position = legend,
      pal = palette, 
      values = valuesPalette,
      opacity = 0.8,
      title = translate(uiText, "legend")$title,
      layerId = "legend"
    )
    
    myMap <- addLegend(
      map = myMap,
      position = legend,
      colors = "red",
      labels = translate(uiText, "occurrence")$title,
      opacity = 1,
      layerId = "legend2"
      )
    
  }
  
  # Add background
  if (addGlobe) {
    
    myMap <- addTiles(myMap)
    
  }
    
  myMap  
  
}





#' Shiny module for creating the plot \code{\link{mapCube}} - server side
#' 
#' @inheritParams welcomeSectionServer
#' @inheritParams createCubeData
#' @inheritParams mapCubeServer
#' @inheritParams mapCubeUI
#' @param species reactive character, readable name of the selected species
#' @param df reactive data.frame, data as loaded by \code{\link{loadGbif}}
#' @param occurrenceData data.table, as obtained by \code{loadTabularData(type = "occurrence")}
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import leaflet
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot webshot
#' @export
mapRegionsServer <- function(id, uiText, species, df, occurrenceData, shapeData) {
  
  moduleServer(id,
    function(input, output, session) {
      
      # For R CMD check
      year <- NULL
      currentYear <- as.numeric(format(Sys.Date(), "%Y")) - 1
      
      ns <- session$ns
      
      results <- reactiveValues()
      
      noData <- reactive(translate(uiText(), "noData"))
      tmpTranslation <- reactive(translate(uiText(), "management-mapOccurrence"))
      
      output$titleMapRegions <- renderUI({
          
          period <- if (!is.null(input$period))
            c(input$period, req(input$year)) else
            req(input$year)
          
          h3(HTML(paste(tmpTranslation()$title, req(species()), yearToTitleString(period))))
          
        })
      
      output$descriptionMapRegions <- renderUI(HTML(tmpTranslation()$description))
      

      # Filters
      output$year <- renderUI({
          
          req(df())
          
          choices <- range(df()$year, na.rm = TRUE)
          
          div(class = "sliderBlank",
            sliderInput(
              inputId = ns("year"), 
              label = paste0(
                translate(uiText(), "year")$title,
                " (", translate(uiText(), "map")$title, ")"),
              min = choices[1],
              max = choices[2],
              value = 2018,
              step = 1,
              sep = "", 
              width = "100%"
            )
          )
          
        })
      
      output$period <- renderUI({
          
          req(nrow(df()) > 0)
          
          # initialize
          if (is.null(results$period_value))
            results$period_value <- range(df()$year, na.rm = TRUE)
          
          sliderInput(inputId = ns("period"), 
            label = paste0(
              translate(uiText(), "period")$title,
              " (", translate(uiText(), "graph")$title, ")"),
            value = results$period_value,
            min = min(df()$year, na.rm = TRUE),
            max = max(df()$year, na.rm = TRUE),
            step = 1,
            sep = "")
          
        })
      
      ## Periode (grafiek)
      # freeze value - when input$regionLevel changes
      observeEvent(input$regionLevel, {
          
          if (is.null(input$period)) {
            results$period_value <- range(df()$year, na.rm = TRUE)
          } else {
            results$period_value <- input$period
          }
          
        })
      
      output$unit <- renderUI({
          
          choices <- c("cpue", "absolute")
          names(choices) <- translate(uiText(), choices)$title
          
          selectInput(inputId = ns("unit"), label = translate(uiText(), "unit")$title, 
            choices = choices)
          
        })
      
      output$regionLevel <- renderUI({
          
          choices <- c("communes", "provinces")
          names(choices) <- translate(uiText(), choices)$title
          
          selectInput(inputId = ns("regionLevel"), label = translate(uiText(), "regionLevel")$title,
            choices = choices)
          
        })
      
      output$region <- renderUI({
          
          selectInput(inputId = ns("region"), label = translate(uiText(), "regions")$title,
              choices = sort(unique(shapeData[[req(input$regionLevel)]]$NAAM)), multiple = TRUE)
          
        })
      
      # Map attributes
      output$legend <- renderUI({
          
          legendChoices <- c("topright", "bottomright", "topleft", "bottomleft", "none")
          names(legendChoices) <- sapply(legendChoices, function(x) translate(uiText(), x)$title)
          
          selectInput(inputId = ns("legend"), 
            label = translate(uiText(), "legend")$title,
            choices = legendChoices)
          
        })
      
      
      # Subset on filters
      summaryData <- reactive({
          
          createSummaryRegions(data = df(), 
            shapeData = shapeData,
            regionLevel = req(input$regionLevel),
            year = req(input$year), unit = req(input$unit))
          
        })
      
      # Filter Occurrence data
      subOccurrence <- reactive({
          
          # Filter on taxonKey and year
          occurrenceData <- occurrenceData[occurrenceData$scientificName == species() & year == req(input$year), ]
          
        })
 
      
      # Send map to the UI
      output$regionsPlot <- renderLeaflet({
          
          validate(need(nrow(req(summaryData())) > 0, noData()))
          
          mapRegions(managementData = summaryData(), occurrenceData = subOccurrence(), 
            shapeData = shapeData, uiText = uiText(), regionLevel = input$regionLevel)
          
        })
      
      # Shape data for regionLevel
      spatialData <- reactive({
          
          shapeData[[req(input$regionLevel)]]
          
        })
      
      # Define text to be shown in the pop-ups
      textPopup <- reactive({
          
          validate(need(nrow(req(summaryData())) > 0, noData()))
          
          textPopup <- paste0("<h4>", summaryData()$region, "</h4>",
            "<strong>", translate(uiText(), "year")$title, "</strong>: ", input$year,
            "<br><strong>", translate(uiText(), input$unit)$title, "</strong>: ", 
            if (input$unit == "cpue") 
                round(summaryData()$effort, 2) else
                round(summaryData()$n, 2)
          )
          
          
          return(textPopup)
          
        })
      
      # Add popups
      observe({
          
          validate(need(textPopup(), noData()))
          
          currentMap <- leafletProxy("regionsPlot") 
          currentMap %>% clearPopups()
          
          event <- input$regionsPlot_shape_click
          
          if (!is.null(event)) {
            
            if (!is.null(event$id)) {
              
              if (event$id %in% summaryData()$region) {
                
                textSelected <- textPopup()[
                  summaryData()$region == event$id]
                
                isolate({
                    
                    currentMap %>% 
                      addPopups(event$lng, event$lat, popup = textSelected)
                    
                  }) 
                
              }
              
            }
            
          }
          
        })
      
      # Add world map
      observe({
          
          proxy <- leafletProxy("regionsPlot")
          
          if (!is.null(input$globe) & !is.null(proxy)){
            
            if (input$globe %% 2 == 1){
              
              updateActionLink(session, inputId = "globe", 
                label = translate(uiText(), "hideGlobe")$title)
              
              proxy %>% addTiles()
              
            } else {
              
              updateActionLink(session, inputId = "globe", 
                label = translate(uiText(), "showGlobe")$title)
              
              proxy %>% clearTiles()
              
            }
            
          }
          
        })
      
      # Add legend
      observe({
          
          req(input$legend)
          req(summaryData())
          
          proxy <- leafletProxy("regionsPlot")
          proxy %>% 
            removeControl(layerId = "legend") %>% 
            removeControl(layerId = "legend2")
          
          if (input$legend != "none") {
            
            palette <- colorFactor(palette = "YlOrBr", levels = levels(summaryData()$group), 
              na.color = "transparent")
            valuesPalette <- summaryData()$group[match(spatialData()$NAAM, summaryData()$region)]
            
            
            proxy %>% addLegend(
              position = input$legend,
              pal = palette, 
              values = valuesPalette,
              opacity = 0.8,
              title = translate(uiText(), "legend")$title,
              layerId = "legend"
            ) %>%  
            addLegend(
              position = input$legend,
              colors = "red",
              labels = translate(uiText(), "occurrence")$title,
              opacity = 1,
              layerId = "legend2"
            )
            
          }
          
        })
            
      # Which region(s) are selected?
      observe({
          
          event <- input$regionsPlot_shape_click
          
          if (!is.null(event)) {
            
            if (!is.null(event$id)) {
              
              currentSelected <- isolate(input$region)
              
              if (event$id %in% currentSelected) {
                # Remove from list
                
                updateSelectInput(session, inputId = "region", 
                  selected = currentSelected[- which(currentSelected == event$id)])
                
                
              } else {
                # Add to list
                
                updateSelectInput(session, inputId = "region", 
                  selected = c(currentSelected, event$id))
                
              }
              
            }
            
          }
          
        })
      
      # Plot thick border for selected regions
      observe({
          
          if (length(input$region) > 0) {
            
            leafletProxy("regionsPlot") %>%
              clearGroup(group = "regionLines") %>%
              addPolylines(
                data = subset(spatialData(), spatialData()$NAAM %in% input$region), 
                color = "gray", weight = 5,
                group = "regionLines")
            
          } else {
            
            leafletProxy("regionsPlot") %>%
              clearGroup(group = "regionLines")
            
          }
          
        })
      
      # Create final map (for download)
      finalMap <- reactive({
          
          newMap <- mapRegions(managementData = summaryData(), occurrenceData = subOccurrence(), 
            shapeData = shapeData, uiText = uiText(), regionLevel = input$regionLevel,
            legend = input$legend, addGlobe = input$globe %% 2 == 1)
      
          
          # save the zoom level and centering to the map object
          newMap <- newMap %>% setView(
            lng = input$regionsPlot_center$lng,
            lat = input$regionsPlot_center$lat,
            zoom = input$regionsPlot_zoom
          )
          
          tmpFile <- tempfile(fileext = ".html")
          
          # write map to temp .html file
          htmlwidgets::saveWidget(newMap, file = tmpFile, selfcontained = FALSE)
          
          # output is path to temp .html file containing map
          tmpFile
          
        }) 
      
      
      # Download the map
      output$downloadMapButton <- renderUI({
          downloadButton(ns("download"), 
            label = translate(uiText(), "downloadMap")$title, 
            class = "downloadButton")
        })
      
      output$download <- downloadHandler(
        filename = function()
          nameFile(species = species(),
            period = input$year, 
            content = "management", fileExt = "png"),
        content = function(file) {
          
          # convert temp .html file into .png for download
          webshot::webshot(url = finalMap(), file = file,
            vwidth = 1000, vheight = 500, cliprect = "viewport")
          
        }
      )
      
      output$downloadData <- downloadHandler(
        filename = function()
          nameFile(species = species(),
            period = input$year, 
            content = "management_data", fileExt = "csv"),
        content = function(file) {
          
          ## write data to exported file
          write.table(x = summaryData(), file = file, quote = FALSE, row.names = FALSE,
            sep = ";", dec = ",")
          
        })
      
      
      
      ## Time plot for Flanders (reference) ##
      ## ---------------------------------- ##
      
      timeDataFlanders <- reactive({
          
          req(input$period)
          
          createSummaryRegions(
            data = df(), 
            shapeData = shapeData,
            regionLevel = "flanders",
            year = input$period[1]:input$period[2],
            unit = req(input$unit)
          )
          
        })
      
      plotModuleServer(id = "timePlotFlanders",
        plotFunction = "trendYearRegion", 
        data = timeDataFlanders,
        uiText = uiText,
        period = reactive(input$period)
      )
      
      
      
      ## Time plot for selected region ##
      ## ----------------------------- ##
      
      observeEvent(input$regionLevel, {
          
          updateCheckboxInput(session = session, inputId = "combine",
            label = paste(translate(uiText(), "combineRegions")$title, 
              translate(uiText(), input$regionLevel)$title))
          
        })  
      
      # Create data for map, time plot
      timeData <- reactive({
          
          req(input$period)
          
          createSummaryRegions(
            data = df(),
            shapeData = shapeData,
            regionLevel = req(input$regionLevel),
            year = input$period[1]:input$period[2],
            unit = req(input$unit)
          )
          
        })
      
      plotModuleServer(id = "timePlot",
        plotFunction = "trendYearRegion", 
        data = reactive({
            timeData()[timeData()$region %in% req(input$region), ]
          }),
        uiText = uiText,
        period = reactive(input$period),
        combine = reactive(input$combine)
      )
      
      
    })  
} 



#' Shiny module for creating the plot \code{\link{mapCube}} - UI side
#' @inheritParams welcomeSectionServer
#' @param plotDetails character vector, which plots to be shown below the map
#' @return UI object
#' 
#' @author mvarewyck
#' @import shiny
#' @importFrom leaflet leafletOutput
#' @export
mapRegionsUI <- function(id, plotDetails = NULL) {
  
  ns <- NS(id)
  
  # Occurrence Map
  
  tags$div(class = "container",
    
    uiOutput(ns("titleMapRegions")),
    
    uiOutput(ns("descriptionMapRegions")),
    
    wellPanel(
      fixedRow(
        column(4, uiOutput(ns("regionLevel"))),
        column(8, uiOutput(ns("region")))
      ),
      fixedRow(
        column(6, uiOutput(ns("year"))),
        column(6, uiOutput(ns("period")))
      ),
      fixedRow(
        column(6, uiOutput(ns("legend"))),
        column(6, uiOutput(ns("unit")))
      ),
      if ("region" %in% plotDetails)
        checkboxInput(inputId = ns("combine"), 
          label = "Combine all selected regions"),
      actionLink(inputId = ns("globe"), label = "Show globe", icon = icon("globe"))
    ),
    withSpinner(leafletOutput(ns("regionsPlot"))),
    
    tags$br(),
    
    tags$div(uiOutput(ns("downloadMapButton")), style = "display:inline-block;"),
    downloadButton(ns("downloadData"), label = "Download data", class = "downloadButton"),
    
    fixedRow(
      if ("flanders" %in% plotDetails) 
        column(6,
          plotModuleUI(id = ns("timePlotFlanders"), height = "400px"),
          optionsModuleUI(id = ns("timePlotFlanders"), exportData = TRUE,
            doWellPanel = FALSE)
        ),
      if ("region" %in% plotDetails)
        column(6,
          plotModuleUI(id = ns("timePlot"), height = "400px"),
          optionsModuleUI(id = ns("timePlot"), exportData = TRUE,
            doWellPanel = FALSE)
        )
    ),
  
    
    tags$hr()
  
  )
  
}
