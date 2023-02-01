#' Create occurrence shape data for \code{\link{mapCube}}
#' 
#' @param df data.frame with occurrence or occupancy data
#' @param shapeData list with sf data.frame for each region level to be plotted
#' @param groupVariable character, defines for which groups to create cube data;
#' \code{groupVariable} should match a column name in \code{df};
#' exception if \code{cell_code} then groups are created per cellcode (spatial level)
#' @return list with sf data.frame for each group/cube level to be plotted
#' 
#' When \code{groupVariable} is defined, return a list with data.frame of cellcodes
#' per group level, the combined groups and the cellcodes which didn't have
#' any occurrence.
#' When no \code{groupVariable} is defined, return a list with data.frame of cellcodes
#' per cube level
#' 
#' @author mvarewyck
#' @importFrom sf st_as_sf st_transform
#' @importFrom data.table data.table rbindlist copy
#' @export
createCubeData <- function(df, shapeData, groupVariable) {
  
  cellCodes <- c("cell_code1", "cell_code10")
  cellCodes <- cellCodes[cellCodes %in% colnames(df)]
    
  if (!is.null(groupVariable) && groupVariable != "cell_code") {
    
    if (length(cellCodes) != 1)
      stop("Only suitable for single grid level")
    
    allGroups <- unique(df[[groupVariable]])
    # t0 and t1
    if (length(allGroups) > 1) {
      combinedGroup <- paste(allGroups, collapse = " & ")
      combinedData <- df[duplicated(df, by = cellCodes), ][, source := combinedGroup]
      df <- rbind(df[!df[[cellCodes]] %in% combinedData[[cellCodes]], ], combinedData)
    } else combinedGroup <- NULL
    # neither
    allCodes <- shapeData[[paste0("be_", gsub("cell_code", "", cellCodes), "km")]]$CELLCODE
    notData <- data.table(
      source = "negative", 
      cell_code = allCodes[!allCodes %in% df[[cellCodes]]] 
    )
    setnames(notData, "cell_code", cellCodes)
    df <- rbindlist(list(df, notData), fill = TRUE)
    # refactor
    df <- df[, source := factor(source, levels = c(allGroups, combinedGroup, "negative"),
        labels = c(paste("only", allGroups), combinedGroup, "negative"))]
    dfList <- split(df, list(df[[groupVariable]]))
    
  } else {
    
    dfList <- sapply(cellCodes, function(iCode)
        copy(df)[, cellCodes[!cellCodes %in% iCode] := NULL],
      simplify = FALSE)
    
  }
  
  # Add spatial info
  toReturn <- sapply(dfList, function(iData) {
      
      iCode <- cellCodes[cellCodes %in% colnames(iData)]
      isOccurred <- unique(iData[[iCode]])
      iShape <- shapeData[[paste0("be_", gsub("cell_code", "", iCode), "km")]]
      st_as_sf(iShape[iShape$CELLCODE %in% isOccurred, ], 
          coords = c("decimalLongitude", "decimalLatitude"),
          crs = 4326) %>%
        st_transform(crs = 4326)
            
    }, simplify = FALSE)
  
  # For unsplit - download data in the app
  tmpFactor <- sapply(toReturn, nrow)
    splitFactor <- unlist(sapply(seq_along(tmpFactor), function(i) {
          rep(names(tmpFactor)[i], each = tmpFactor[i])
        }))
  attr(toReturn, "splitFactor") <- splitFactor
  
  
  toReturn

}



#' Create occurrence barplot 
#' 
#' Number of cells with at least one observation per year
#' @param df data.frame, data.frame with occurrence data for selected species (taxonKey) 
#' @param spatialLevel character, should be one of \code{c("1km", "10km")};
#' if not in colnames of \code{df} then the sum over available count variable is calculated
#' @param minYear numeric, start year of the barplot
#' @param period numeric vector of length 2, selected period is colored blue,
#' other years are colored gray
#' @return plotly
#' 
#' @author mvarewyck
#' @import plotly
#' @importFrom INBOtheme inbo_lichtgrijs inbo_steun_blauw
#' @export
countOccurrence <- function(df, spatialLevel = c("1km", "10km"), minYear = 1950,
  period = c(2000, 2018)) {
  
  
  # For R CMD check
  count <- NULL
  year <- NULL
  selected <- NULL
  currentYear <- as.numeric(format(Sys.Date(), "%Y"))
  
  spatialLevel <- match.arg(spatialLevel)
  iCode <- switch(spatialLevel,
      '1km' = "cell_code1",
      '10km' = "cell_code10"
  )
  
  if (!iCode %in% colnames(df)) {
    
    nOccurred <- df[, .(count = sum(count)), by = year]
    
  } else {
    
    nOccurred <- df[, .(count = length(unique(get(iCode)))), by = year]
    
  } 

  # Filter data
  nOccurred <- nOccurred[year > minYear, ][, selected := year >= period[1] & year <= period[2]]
  
  myColors <- c(inbo_lichtgrijs, inbo_steun_blauw)
  
  myPlot <- plot_ly(data = nOccurred, x = ~year, y = ~count, type = "bar",
      marker = list(color = ~myColors[selected + 1]),
      hoverinfo = "x+y") %>%
    layout(
      xaxis = list(title = "Year", range = c(minYear, currentYear)),
      yaxis = list(title = ""),
      showlegend = FALSE
  )
  
  
  list(plot = myPlot, data = nOccurred)
  
}



#' Necessary info for the color palette of \code{\link{mapCube}}
#' @param groupNames character vector, labels to be shown in the color legend
#' @param groupVariable character, variable for which the \code{groupNames} are defined
#' @return list with colors, character vector and levels, character vector. 
#' Each item has same length as \code{units}
#' 
#' @author mvarewyck
#' @importFrom INBOtheme inbo_palette
#' @export
paletteMap <- function(groupNames, groupVariable) {
  
  # Actually only needed if groupVariable == "cell_codes"
  groupNames <- gsub(groupVariable, "", groupNames)
  
  myPalette <- c("black", inbo_palette())
  myColors <- rev(myPalette[seq_along(groupNames)]) 
  
  if (groupVariable == "cell_code")
    valuesPalette <- factor(paste0("UTM ", groupNames, "x", groupNames, " km squares")) else
    valuesPalette <- groupNames
  
  list(
    colors = myColors,
    levels = valuesPalette
  )
  
}


#' Create base map of Belgium
#' @return leaflet object
#' @author mvarewyck
#' @import leaflet
#' @importFrom sp proj4string CRS spTransform
#' @importFrom rgdal readOGR  
#' @export
createBaseMap <- function() {
  
  crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs ")
  crs_bel <- CRS("+proj=lcc +lat_1=51.16666723333333 +lat_2=49.8333339 +lat_0=90 +lon_0=4.367486666666666 +x_0=150000.013 +y_0=5400088.438 +ellps=intl +towgs84=-106.869,52.2978,-103.724,0.3366,-0.457,1.8422,-1.2747 +units=m +no_defs ")
  
  bioreg_bel_clip <- readOGR(system.file("extdata", "grid", "bioreg_bel_clip.geojson", package = "alienSpecies"), "bioreg_bel_clip", stringsAsFactors = FALSE)
  bel_borders <- readOGR(system.file("extdata", "grid", "Belgie.geojson", package = "alienSpecies"), "Belgie", stringsAsFactors = FALSE)
  
  sp::proj4string(bel_borders) <- crs_bel
  
  bioreg_pal <- colorFactor(palette = c("darkgrey", "white"), 
    domain = bioreg_bel_clip$BIOGEO, levels = c("Continental", "Atlantic"))
  
  baseMap <- leaflet(bioreg_bel_clip) %>% 
    addPolygons(fillColor = ~bioreg_pal(BIOGEO),
      fillOpacity = 0.5,
      stroke = FALSE) %>% 
#    addPolylines(data = bel_borders, 
    addPolylines(data = spTransform(bel_borders, crs_wgs),
      color = "black",
      opacity = 1,
      weight = 2) %>% 
    addScaleBar(position = "bottomleft")
#    setMapWidgetStyle(list(background= "white"))
  
  baseMap
  
}


#' Create leaflet map for the occurrence **cube** data
#' 
#' @param cubeShape list with sf data.frame as returned by
#' \code{\link{createCubeData}}
#' @param baseMap leaflet object as created by \code{createBaseMap}
#' @param legend character, legend placement; default is "none", no legend
#' @param addGlobe boolean, whether to add world map to background; default is FALSE 
#' @inheritParams createCubeData
#' @return leaflet map
#' 
#' @author mvarewyck
#' @import leaflet
#' @export
mapCube <- function(cubeShape, baseMap = createBaseMap(), legend = "none", 
  addGlobe = FALSE,
  groupVariable) {
  
  
  myColors <- paletteMap(groupNames = names(cubeShape), groupVariable = groupVariable)
  palette <- colorFactor(palette = myColors$colors, levels = myColors$levels)
  
#  myMap <- leaflet()
  myMap <- baseMap
  
  for (i in length(cubeShape):1)
    
    myMap <- myMap %>%
      addPolygons(color = ~ palette(myColors$levels[i]),
        popup = ~CELLCODE,
        data = cubeShape[[i]],
        group = myColors$levels[i],
        opacity = 1,
        fillOpacity = if (groupVariable != "cell_code" && i != length(cubeShape)) 0.5 else 0,
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


#' Create leaflet map for the occurrence **management** data
#' 
#' @param occurrenceData data.table, as loaded via \code{\link{loadGbif}}
#' @inheritParams mapCube
#' @return leaflet map
#' 
#' @author mvarewyck
#' @importFrom leaflet addMarkers addTiles `%>%` markerClusterOptions
#' @import data.table
#' @export
mapOccurrence <- function(occurrenceData, baseMap = createBaseMap(),
  addGlobe = FALSE) {
  
  # For R CMD check
  count <- decimalLongitude <- decimalLatitude <- NULL
  
  ## Sum counts over ID
  occurrenceData <- occurrenceData[, .(count = sum(count)),
    by = .(decimalLongitude, decimalLatitude)] 
  
  
  myMap <- baseMap
  
  # Add background map - needed for clusters to be shown and before addMarkers()
  if (addGlobe) {
    
    myMap <- addTiles(myMap)
    
  } else warning("Clusters will not be displayed.")
  
  myMap <- myMap %>%
    addMarkers(
      data = occurrenceData,
      lng = ~decimalLongitude,
      lat = ~decimalLatitude,
      popup = ~as.character(count),
      label = ~as.character(count),
      clusterOptions = if (addGlobe) markerClusterOptions()
    )
  
 
  
  
  myMap
  
}




#' Shiny module for creating the plot \code{\link{mapCube}} - server side
#' 
#' @param filter reactive list with filters to be shown in the app;
#' names should match a plotFunction in \code{uiText}; 
#' values define the choices in \code{selectInput}
#' @inheritParams welcomeSectionServer
#' @inheritParams createCubeData
#' @inheritParams mapCubeServer
#' @inheritParams mapCubeUI
#' @param species reactive character, readable name of the selected species
#' @param df reactive data.frame, data as loaded by \code{\link{loadGbif}}
#' @param baseMap leaflet object as returned by \code{\link{createBaseMap}}
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
mapCubeServer <- function(id, uiText, species, df, shapeData, baseMap,
  filter = reactive(list(source = c("all"))), 
  groupVariable, showPeriod = FALSE, showGlobeDefault = TRUE
) {
  
  moduleServer(id,
    function(input, output, session) {
      
      # For R CMD check
      year <- NULL
      currentYear <- as.numeric(format(Sys.Date(), "%Y")) - 1
      
      ns <- session$ns
      
      noData <- reactive(translate(uiText(), "noData")$title)
      tmpTranslation <- reactive(translate(uiText(), ns("mapOccurrence")))
      
      output$descriptionMapOccurrence <- renderUI(tmpTranslation()$description)
      
      output$titleMapOccurrence <- renderUI({
          
          req(species())
          
          tmpTitle <- tmpTranslation()$title
          
          myTitle <- if (showPeriod) {
            req(input$period)
            paste(tmpTitle, species(), yearToTitleString(req(input$period))) 
          } else {
            paste(tmpTitle, species())
          }
          
          h3(HTML(myTitle))
        
        })
      
      output$filters <- renderUI({
          
          lapply(names(filter()), function(filterName) {
              
              choices <- filter()[[filterName]]
              names(choices) <- translate(uiText(), choices)$title
              
              column(6, 
                selectInput(inputId = ns(filterName), 
                  label = translate(uiText(), filterName)$title,
                  choices = choices,
                  multiple = TRUE, selected = filter()[[filterName]])
              )
            })
          
        })
      
      output$period <- renderUI({
          
          req(df())
          
          periodChoice <- range(df()$year, na.rm = TRUE)
          
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
      
      output$legend <- renderUI({
          
          legendChoices <- c("topright", "bottomright", "topleft", "bottomleft", "none")
          names(legendChoices) <- sapply(legendChoices, function(x) translate(uiText(), x)$title)
          
          selectInput(inputId = ns("legend"), 
            label = translate(uiText(), "legend")$title,
            choices = legendChoices)
          
        })
      
      
      # Subset on filters
      filterData <- reactive({
          
          filterData <- df()
          
          # Other filters
          for (iFilter in names(filter())) {
            if (iFilter != "source")
              filterData <- filterData[filterData[[iFilter]] %in% input[[iFilter]], ]
          }
          
          filterData
          
        })
      
      # Subset on period
      subData <- reactive({
          
          # Filter on time
          if (showPeriod) {
              
              req(input$period)
              filterData()[year >= input$period[1] & year <= input$period[2], ]
              
            } else filterData()
          
        })
      
      
      
      # Create data for map
      cubeShape <- reactive({
          
          validate(need(subData(), noData()),
            need(nrow(subData()) > 0, noData()))
          
          createCubeData(
            df = subData(),
            shapeData = shapeData,
            groupVariable = groupVariable
          )
          
        })
      
      # Send map to the UI
      output$spacePlot <- renderLeaflet({
          
          if (is.null(shapeData)) {
            
            validate(need(nrow(subData()) > 0, noData()))
            
            mapOccurrence(occurrenceData = subData(), baseMap = baseMap,
              addGlobe = TRUE)
            
          } else {
            
            validate(need(cubeShape(), noData()))
            
            mapCube(cubeShape = cubeShape(), baseMap = baseMap, 
              groupVariable = groupVariable, addGlobe = TRUE, legend = "topright")
            
          }
          
        })
      
      # Add world map
      observe({
          
          validate(need(cubeShape(), noData()))
          
          proxy <- leafletProxy("spacePlot")
          
          if (!is.null(input$globe) & !is.null(proxy)){
            
            if (input$globe %% 2 == as.numeric(showGlobeDefault) - 1){
              
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
          
          validate(need(cubeShape(), noData()))
          
          req(input$legend)
          
          proxy <- leafletProxy("spacePlot")
          proxy %>% removeControl(layerId = "legend")
          
          if (input$legend != "none") {
            
            myColors <- paletteMap(groupNames = names(cubeShape()), 
              groupVariable = groupVariable)
            palette <- colorFactor(palette = myColors$colors, levels = myColors$levels)
            
            proxy %>% addLegend(
              position = input$legend,
              pal = palette, 
              values = myColors$levels,
              opacity = 0.8,
              title = translate(uiText(), "legend")$title,
              layerId = "legend"
            )                      
            
          }
          
        })
      
      
      # Create final map (for download)
      finalMap <- reactive({
          
          if (is.null(shapeData)) {
            
            newMap <- mapOccurrence(
              occurrenceData = subData(), 
              baseMap = baseMap,
              addGlobe = input$globe %% 2 == as.numeric(showGlobeDefault) - 1
            )
            
          } else {
            
            newMap <- mapCube(
              cubeShape = cubeShape(),
              groupVariable = groupVariable,
              baseMap = baseMap,
              legend = input$legend,
              addGlobe = input$globe %% 2 == as.numeric(showGlobeDefault) - 1
            )
            
          }
          
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
      output$downloadMapButton <- renderUI({
          downloadButton(ns("download"), 
            label = translate(uiText(), "downloadMap")$title, 
            class = "downloadButton")
        })
      
      output$download <- downloadHandler(
        filename = function()
          nameFile(species = species(),
            period = input$period, 
            content = id, fileExt = "png"),
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
            content = paste0(id, "_data"), fileExt = "csv"),
        content = function(file) {
          myData <- do.call(rbind, cubeShape())
          myData$source <- attr(cubeShape(), "splitFactor")
          myData$geometry <- NULL          
          ## write data to exported file
          write.table(x = myData, file = file, quote = FALSE, row.names = FALSE,
            sep = ";", dec = ",")
          
        })
      
      
      ## Barplot for Occurrence ##
      ## ---------------------- ##
      
    
      
      plotModuleServer(id = "countOccurrence",
        plotFunction = "countOccurrence", 
        data = filterData,
        period = reactive(input$period)
      )
      
    })  
} 



#' Shiny module for creating the plot \code{\link{mapCube}} - UI side
#' @inheritParams welcomeSectionServer
#' @param showLegend boolean, whether to show selector for map legend; default TRUE
#' @param showGlobe boolean, whether to show selector for background globe; default TRUE
#' @param showPeriod boolean, whether to show time selector; default FALSE
#' @return UI object
#' 
#' @author mvarewyck
#' @import shiny
#' @importFrom leaflet leafletOutput
#' @export
mapCubeUI <- function(id, showLegend = TRUE, showGlobe = TRUE, showPeriod = FALSE) {
  
  ns <- NS(id)
  
  # Occurrence Map
  
  tags$div(class = "container",
    
    uiOutput(ns("titleMapOccurrence")),
    uiOutput(ns("descriptionMapOccurrence")),
    
    wellPanel(
      fixedRow(uiOutput(ns("filters")),
        if (showLegend)
          column(6, 
            uiOutput(ns("legend"))
          ),
        if (showGlobe)
        column(6, 
          actionLink(inputId = ns("globe"), label = "Show globe",
            icon = icon("globe"))
        )
      )
    ),
    withSpinner(leafletOutput(ns("spacePlot"))),
    
    if (showPeriod) {
      tagList(
        plotModuleUI(id = ns("countOccurrence"), height = "400px"),
        uiOutput(ns("period"))
      )
    },
    
    tags$br(),
    
    tags$div(uiOutput(ns("downloadMapButton")), style = "display:inline-block;"),
    downloadButton(ns("downloadData"), label = "Download data", class = "downloadButton"),
    
    tags$hr()
  
  )
  
}
