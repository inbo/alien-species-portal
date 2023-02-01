
#' Create summary data per region
#' @param data data.table with management data
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
createSummaryRegions <- function(data, regionLevel = c("communes", "provinces"),
  year, unit = c("cpue", "absolute")) {
  
  # For R CMD check
  region <- NULL
  eventID <- NULL
  n_fuiken <- NULL
  count <- NULL
  effort <- NULL
  n <- NULL
  
  unit <- match.arg(unit)
  
  data <- as.data.frame(data)
  myYear <- year  # for tidyverse filtering
  
  regionVar <- switch(regionLevel,
    communes = "NISCODE",
    provinces = "provincie"
  )
  
  colnames(data)[colnames(data) == regionVar] <- "region"
  
  if (unit == "cpue") {
    
    summaryData <- data %>% 
      filter(year == myYear, !is.na(region), region != "NA") %>% 
      group_by(eventID, region) %>% 
      summarise(effort = max(n_fuiken, na.rm = TRUE),
        n = sum(count, na.rm = TRUE)/effort) %>% 
      group_by(region) %>% 
      summarise(effort = sum(effort, na.rm = TRUE),
        n = sum(n, na.rm = TRUE))
    
    summaryData$group <- cut(x = summaryData$effort, 
      breaks = c(-Inf, 10, 100, 200, 300, 400, Inf),
      labels = c("0-10", "11-100", "101-200", "201-300", "300-400", paste0("401-", max(500, max(summaryData$n))))
    )
    
  } else {
    
    summaryData <- data %>%
      filter(year == myYear, !is.na(region), region != "NA") %>% 
      group_by(region) %>% 
      summarise(n = sum(count, na.rm = TRUE)) 
      
      
    summaryData$group <- cut(x = summaryData$n, 
      breaks = c(-Inf, 1000, 5000, 10000, Inf),
      labels = c("0-1000", "1001-5000", "5001-10000", paste0("10001-", max(50000, max(summaryData$n))))
    )
    
  }
  
  return(summaryData)
  
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
  
  regionVar <- switch(regionLevel,
    communes = "NISCODE",
    provinces = "NAAM"
  )
  spatialData <- shapeData[[regionLevel]]
  
  palette <- colorFactor(palette = "YlOrBr", levels = levels(managementData$group), 
    na.color = "transparent")
  valuesPalette <- managementData$group[match(spatialData@data[, regionVar], managementData$region)]
  
  spread <- createCubeData(df = occurrenceData, shapeData = shapeData,
    groupVariable = "cell_code")
  
# MAP: Bullfrog management in `r jaar`
#  Suggested filters:
#  - year
#  - region-scale (province/commune)
#  - unit (absolute/cpue)
  
  
  myMap <- leaflet(spatialData) %>%
    addPolylines(weight = 1, color = "black") %>% 
    addPolylines(data = spread$cell_code1, 
      color = "red",
      weight = 1) %>% 
    addPolygons(
      weight = 1,
      color = ~ palette(valuesPalette),
      fillColor = ~ palette(valuesPalette),
      fillOpacity = 0.8,
      layerId = spatialData@data[, regionVar],
      group = "region"
    )
  
  # Add provinces borders
  if (regionLevel == "communes") {
    
    myMap <- myMap %>% 
      addPolylines(data = shapeData$provinces, 
      weight = 2,
      color = "black")
  
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
    
    myMap <- addProviderTiles(myMap, "OpenStreetMap.HOT")
    
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
      
      noData <- reactive(translate(uiText(), "noData"))
      tmpTranslation <- reactive(translate(uiText(), "management-mapOccurrence"))
      
      output$titleMapRegions <- renderUI({
          
          h3(HTML(paste(tmpTranslation()$title, req(species()), yearToTitleString(req(input$year)))))
          
        })
      
      output$descriptionMapRegions <- renderUI(HTML(tmpTranslation()$description))
      

      # Filters
      output$year <- renderUI({
          
          req(df())
          
          choices <- range(df()$year, na.rm = TRUE)
          
          div(class = "sliderBlank",
            sliderInput(
              inputId = ns("year"), 
              label = NULL,
              min = choices[1],
              max = choices[2],
              value = 2018,
              step = 1,
              sep = "", 
              width = "100%"
            )
          )
          
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
          
          selectInput(inputId = ns("regionLevel"), label = translate(uiText(), "region")$title,
            choices = choices)
          
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
          
          createSummaryRegions(data = df(), regionLevel = req(input$regionLevel),
            year = req(input$year), unit = req(input$unit))
          
        })
      
      # Filter Occurrence data
      subOccurrence <- reactive({
          
          # Filter on taxonKey and year
          occurrenceData <- occurrenceData[occurrenceData$scientificName == species() & year == req(input$year), ]
          
        })
 
      
      # Send map to the UI
      output$regionsPlot <- renderLeaflet({
          
          validate(need(nrow(summaryData()) > 0, noData()))
          
          mapRegions(managementData = summaryData(), occurrenceData = subOccurrence(), 
            shapeData = shapeData, uiText = uiText(), regionLevel = input$regionLevel)
          
        })
      
      
      # Define text to be shown in the pop-ups
      textPopup <- reactive({
          
          validate(need(nrow(summaryData()) > 0, noData()))
          
          if (input$regionLevel == "communes") {
            regionNames <- spatialData()$NAAM[match(summaryData()$region, spatialData()$regionName)]
          } else
            regionNames <- summaryData()$region
          
          textPopup <- paste0("<h4>", regionNames, "</h4>",
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
      
      spatialData <- reactive({
          
          regionVar <- switch(req(input$regionLevel),
            communes = "NISCODE",
            provinces = "provincie"
          )
          
          toReturn <- shapeData[[req(input$regionLevel)]]
          names(toReturn)[names(toReturn) == regionVar] <- "regionName"
          
          toReturn
          
        })
      
      
      # Add legend
      observe({
          
          req(input$legend)
          
          proxy <- leafletProxy("regionsPlot")
          proxy %>% 
            removeControl(layerId = "legend") %>% 
            removeControl(layerId = "legend2")
          
          if (input$legend != "none") {
            
            palette <- colorFactor(palette = "YlOrBr", levels = levels(summaryData()$group), 
              na.color = "transparent")
            valuesPalette <- summaryData()$group[match(spatialData()$regionName, summaryData()$region)]
            
            
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
      
    })  
} 



#' Shiny module for creating the plot \code{\link{mapCube}} - UI side
#' @inheritParams welcomeSectionServer
#' @return UI object
#' 
#' @author mvarewyck
#' @import shiny
#' @importFrom leaflet leafletOutput
#' @export
mapRegionsUI <- function(id) {
  
  ns <- NS(id)
  
  # Occurrence Map
  
  tags$div(class = "container",
    
    uiOutput(ns("titleMapRegions")),
    
    uiOutput(ns("descriptionMapRegions")),
    
    wellPanel(
      fixedRow(
        column(6, uiOutput(ns("regionLevel"))),
        column(6, uiOutput(ns("unit")))
      ),
      fixedRow(
        column(6, uiOutput(ns("year"))),
        column(6, uiOutput(ns("legend")))
      ),
      actionLink(inputId = ns("globe"), label = "Show globe", icon = icon("globe"))
    ),
    withSpinner(leafletOutput(ns("regionsPlot"))),
    
    tags$br(),
    
    tags$div(uiOutput(ns("downloadMapButton")), style = "display:inline-block;"),
    downloadButton(ns("downloadData"), label = "Download data", class = "downloadButton"),
    
    tags$hr()
  
  )
  
}
