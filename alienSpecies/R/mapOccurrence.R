#' Create occurrence shape data for \code{\link{mapOccurrence}}
#' 
#' @param occurrenceData data.frame with occurrence data 
#' @param shapeData list with sf data.frame for each region level to be plotted
#' @return list with sf data.frame for each region level to be plotted,
#' contains boolean column 'occurrence' whether the species was observed in the grid or not
#' 
#' @author mvarewyck
#' @importFrom sf st_as_sf st_transform
#' @export
createOccurrenceData <- function(occurrenceData, shapeData) {
  
  sapply(c("cell_code1", "cell_code10"), function(iCode) {
      
      isOccurred <- unique(occurrenceData[[iCode]])
      iShape <- shapeData[[paste0("be_", gsub("cell_code", "", iCode), "km")]]
      tmp <- st_as_sf(iShape[iShape$CELLCODE %in% isOccurred, ], 
          coords = c("decimalLongitude", "decimalLatitude"),
          crs = 4326) %>%
        st_transform(crs = 4326)
      
    }, simplify = FALSE)
  
}



#' Create occurrence barplot 
#' 
#' Number of cells with at least one observation per year
#' @param df data.frame, data.frame with occurrence data for selected species (taxonKey) 
#' @param spatialLevel character, should be one of \code{c("1km", "10km")} 
#' @param minYear numeric, start year of the barplot
#' @param period numeric vector of length 2, selected period is colored blue,
#' other years are colored gray
#' @return plotly
#' 
#' @author mvarewyck
#' @import plotly
#' @export
countOccurrence <- function(df, spatialLevel = c("1km", "10km"), minYear = 1950,
  period = c(2000, 2018)) {
  
  
  # For R CMD check
  year <- NULL
  selected <- NULL
  currentYear <- as.numeric(format(Sys.Date(), "%Y"))
  
  spatialLevel <- match.arg(spatialLevel)
  iCode <- switch(spatialLevel,
    '1km' = "cell_code1",
    '10km' = "cell_code10"
  )
  
  nOccurred <- df[, .(count = length(unique(get(iCode)))), by = year]
  # Filter data
  nOccurred <- nOccurred[year > minYear, ][, selected := year >= period[1] & year <= period[2]]
  
  myPlot <- plot_ly(data = nOccurred, x = ~year, y = ~count, type = "bar",
      marker = list(color = ~c("grey", "blue")[selected + 1]),
      hoverinfo = "x+y") %>%
    layout(
      xaxis = list(title = "Year", range = c(minYear, currentYear)),
      yaxis = list(title = ""),
      showlegend = FALSE)
  
  
  list(plot = myPlot, data = nOccurred)
  
}



#' Necessary info for the color palette of \code{\link{mapOccurrence}}
#' @param units character vector, spatial levels in the data, e.g. \code{c(1, 10)}
#' @return list with colors, character vector and levels, character vector. 
#' Each item has same length as \code{units}
#' 
#' @author mvarewyck
#' @importFrom grDevices palette
#' @export
paletteMap <- function(units) {
  
  myColors <- rev(palette()[seq_along(units)])
  valuesPalette <- factor(paste0("UTM ", units, "x", units, " km squares"))
  
  list(
    colors = myColors,
    levels = valuesPalette
  )
  
}


#' Create leaflet map for the occurrence data
#' @param occurrenceShape list with sf data.frame as returned by
#' \code{\link{createOccurrenceData}}
#' @param legend character, legend placement; default is "none", no legend
#' @param addGlobe boolean, whether to add world map to background; default is FALSE 
#' @return leaflet map
#' 
#' @author mvarewyck
#' @import leaflet
#' @importFrom sp proj4string CRS spTransform
#' @importFrom rgdal readOGR
#' @importFrom leaflet.extras setMapWidgetStyle
#' @export
mapOccurrence <- function(occurrenceShape, legend = "none", addGlobe = FALSE) {
  
  
  myColors <- paletteMap(units =  gsub("cell_code", "", names(occurrenceShape)))
  palette <- colorFactor(palette = myColors$colors, levels = myColors$levels)
  
#  ## TODO extract basemap and use as input
#  crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs ")
#  crs_bel <- CRS("+proj=lcc +lat_1=51.16666723333333 +lat_2=49.8333339 +lat_0=90 +lon_0=4.367486666666666 +x_0=150000.013 +y_0=5400088.438 +ellps=intl +towgs84=-106.869,52.2978,-103.724,0.3366,-0.457,1.8422,-1.2747 +units=m +no_defs ")
#  
#  bioreg_bel_clip <- readOGR(system.file("extdata", "grid", "bioreg_bel_clip.geojson", package = "alienSpecies"), "bioreg_bel_clip", stringsAsFactors = FALSE)
#  bel_borders <- readOGR(system.file("extdata", "grid", "Belgie.geojson", package = "alienSpecies"), "Belgie", stringsAsFactors = FALSE)
#  
#  proj4string(bel_borders) <- crs_bel
#  
#  bioreg_pal <- colorFactor(palette = c("darkgrey", "white"), domain = bioreg_bel_clip$BIOGEO, levels = c("Continental", "Atlantic"))
#  
#  basemap <- leaflet(bioreg_bel_clip) %>% 
#    addPolygons(fillColor = ~bioreg_pal(BIOGEO),
#      fillOpacity = 0.5,
#      stroke = FALSE) %>% 
#    addPolylines(data = spTransform(bel_borders, crs_wgs),
#      color = "black",
#      opacity = 1,
#      weight = 2) %>% 
#    addScaleBar(position = "bottomleft") %>% 
#    setMapWidgetStyle(list(background= "white"))
  
  # TODO use basemap
  myMap <- leaflet()
#  myMap <- basemap
  
  for (i in length(occurrenceShape):1)
    
    myMap <- myMap %>%
      addPolygons(color = ~ palette(myColors$levels[i]),
        popup = ~CELLCODE,
        data = occurrenceShape[[i]],
        group = myColors$levels[i],
        opacity = 1,
        fillOpacity = 0,
        weight = 1
      )
  
  
  # Add legend
  if (legend != "none") { 
    
    myMap <- addLegend(
      map = myMap,
      position = legend,
      pal = palette, 
      values = myColors$levels,
      opacity = 0.8,
      title = "Legende",
      layerId = "legend"
    )
    
  }
  
  # Add background map
  if (addGlobe) {
    
    myMap <- addTiles(myMap)
    
  }
  
  
  myMap
  
}



#' Shiny module for creating the plot \code{\link{mapOccurrence}} - server side
#' @inheritParams welcomeSectionServer
#' @inheritParams createOccurrenceData
#' @param taxonKey reactive integer, selected species
#' @param taxData data.frame as reactive object, data for \code{\link{countIntroductionPathway}}
#' @param showGlobeDefault boolean, whether the globe is shown by default 
#' when the map is first created; default value is TRUE
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import leaflet
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot webshot
#' @export
mapOccurrenceServer <- function(id, uiText, taxonKey, taxData, shapeData,
  showGlobeDefault = TRUE
) {
  
  moduleServer(id,
    function(input, output, session) {
      
      # For R CMD check
      year <- NULL
      currentYear <- as.numeric(format(Sys.Date(), "%Y")) - 1
      
      ns <- session$ns
      
      subText <- reactive({
          uiText[uiText$plotFunction == "mapOccurrence", ]
        })
      
      output$titleMapOccurrence <- renderUI({
          
          h3(HTML(paste(subText()$title, species(),
                yearToTitleString(req(input$period))
              )))
          
        })
      
      species <- reactive(names(taxonKey()))
      
      subData <- reactive({
          
          req(taxData)
          req(taxonKey())
          
          taxData[taxData$taxonKey %in% taxonKey(), ]
          
        })
      
      output$period <- renderUI({
          
          req(subData())
          
          periodChoice <- range(subData()$year, na.rm = TRUE)
          
          div(class = "sliderBlank", style = "margin-left:50px; margin-right:10px;",
            sliderInput(
              inputId = ns("period"), 
              label = NULL,
              min = 1950,
              max = currentYear,
              value = periodChoice,
              sep = "", 
              width = "100%"
            )
          )
          
        })
      
 
      
      # Create data for map
      occurrenceShape <- reactive({
          
          req(input$period)
          validate(need(subData(), "Geen data beschikbaar"))
          
          createOccurrenceData(
            occurrenceData = subData()[year >= input$period[1] & year <= input$period[2], ],
            shapeData = shapeData
          )
          
        })
      
      # Send map to the UI
      output$spacePlot <- renderLeaflet({
          
          req(shapeData)
          
          validate(need(occurrenceShape(), "Geen data beschikbaar"))
          
          mapOccurrence(occurrenceShape = occurrenceShape())          
          
        })
      
      # Add world map
      observe({
          
          validate(need(occurrenceShape(), "Geen data beschikbaar"))
          
          proxy <- leafletProxy("spacePlot")
          
          if (!is.null(input$globe) & !is.null(proxy)){
            
            if (input$globe %% 2 == as.numeric(showGlobeDefault) - 1){
              
              updateActionLink(session, inputId = "globe", 
                label = "Verberg landkaart")
              
              proxy %>% addTiles()
              
            } else {
              
              updateActionLink(session, inputId = "globe", 
                label = "Voeg landkaart toe")
              
              proxy %>% clearTiles()
              
            }
            
          }
          
        })
      
      
      # Add legend
      observe({
          
          validate(need(occurrenceShape(), "Geen data beschikbaar"))
          
          req(input$legend)
          
          proxy <- leafletProxy("spacePlot")
          proxy %>% removeControl(layerId = "legend")
          
          if (input$legend != "none") {
            
            myColors <- paletteMap(units = gsub("cell_code", "", names(occurrenceShape())))
            palette <- colorFactor(palette = myColors$colors, levels = myColors$levels)
            
            proxy %>% addLegend(
              position = input$legend,
              pal = palette, 
              values = myColors$levels,
              opacity = 0.8,
              title = "Legende",
              layerId = "legend"
            )                      
            
          }
          
        })
      
      
      # Create final map (for download)
      finalMap <- reactive({
          
          validate(need(occurrenceShape(), "Geen data beschikbaar"))
          
          newMap <- mapOccurrence(
            occurrenceShape = occurrenceShape(),
            legend = input$legend,
            addGlobe = input$globe %% 2 == as.numeric(showGlobeDefault) - 1
          )
          
          # save the zoom level and centering to the map object
          newMap <- newMap %>% setView(
            lng = input$spacePlot_center$lng,
            lat = input$spacePlot_center$lat,
            zoom = input$spacePlot_zoom
          )
          
          tmpFile <- tempfile(fileext = ".html")
          
          # write map to temp .html file
          htmlwidgets::saveWidget(newMap, file = tmpFile, selfcontained = FALSE)
          
          # output is path to temp .html file containing map
          tmpFile
          
        }) 
      
      
      # Download the map
      output$download <- downloadHandler(
        filename = function()
          nameFile(species = species(),
            period = input$period, 
            content = "kaart", fileExt = "png"),
        content = function(file) {
          
          # convert temp .html file into .png for download
          webshot::webshot(url = finalMap(), file = file,
            vwidth = 1000, vheight = 500, cliprect = "viewport")
          
        }
      )
      
      output$downloadData <- downloadHandler(
        filename = function()
          nameFile(species = species(),
            period = input$period, 
            content = "kaartData", fileExt = "csv"),
        content = function(file) {
          
          myData <- as.data.frame(occurrenceShape())
          # TODO change variable names
#          names(myData)[names(myData) == "freq"] <- results$unitText()
#          names(myData)[names(myData) == "group"] <- "groep"
          
          ## write data to exported file
          write.table(x = myData, file = file, quote = FALSE, row.names = FALSE,
            sep = ";", dec = ",")
          
        })
      
      
      ## Barplot for Occurrence ##
      ## ---------------------- ##
      
      plotModuleServer(id = "countOccurrence",
        plotFunction = "countOccurrence", 
        data = subData,
        period = reactive(input$period)
      )
      
    })  
} 



#' Shiny module for creating the plot \code{\link{mapOccurrence}} - UI side
#' @inheritParams welcomeSectionServer
#' @param sources character vector, sources to select from
#' @return UI object
#' 
#' @author mvarewyck
#' @import shiny
#' @importFrom leaflet leafletOutput
#' @export
mapOccurrenceUI <- function(id, sources = c("All")) {
  
  ns <- NS(id)
  
  # Occurrence Map
  
  tags$div(class = "container",
    
    uiOutput(ns("titleMapOccurrence")),
    
    wellPanel(
      fixedRow(
        column(6, 
          selectInput(inputId = ns("source"), label = "Source",
            choices = sources),
          actionLink(inputId = ns("globe"), label = "Voeg landkaart toe",
            icon = icon("globe"))
        ),
        column(6, 
          selectInput(inputId = ns("legend"), label = "Legend",
            choices = c(
              "Bovenaan rechts" = "topright",
              "Onderaan rechts" = "bottomright",
              "Bovenaan links" = "topleft",
              "Onderaan links" = "bottomleft",
              "<geen>" = "none"))
        )
      )
    ),
    withSpinner(leafletOutput(ns("spacePlot"))),
    
    
    plotModuleUI(id = ns("countOccurrence"), height = "400px"),
    uiOutput(ns("period")),
    
    tags$hr()
  
  )
  
}
