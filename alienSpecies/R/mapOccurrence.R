#' Create occurrence shape data for \code{\link{mapCube}}
#' 
#' @param df data.frame with occurrence or occupancy data
#' @param shapeData list with sf data.frame for each region level to be plotted
#' @param groupVariable character, defines for which groups to create cube data;
#' \code{groupVariable} should match a column name in \code{df};
#' exception if \code{cell_code} then groups are created per cellcode (spatial level)
#' @param region character, regions for which to create data;
#' default is \code{c("flanders", "wallonia", "brussels")}
#' @return list with sf data.frame for each group/cube level to be plotted
#' 
#' When \code{groupVariable} is defined, return a list with data.frame of cellcodes
#' per group level, the combined groups and the cellcodes which didn't have
#' any occurrence.
#' When no \code{groupVariable} is defined, return a list with data.frame of cellcodes
#' per cube level
#' 
#' @author mvarewyck
#' @importFrom sf st_as_sf st_transform st_drop_geometry
#' @importFrom data.table data.table rbindlist copy
#' @export
createCubeData <- function(df, shapeData, groupVariable, 
  region = c("flanders", "wallonia", "brussels")) {
  
  cellCodes <- c("cell_code1", "cell_code10")
  cellCodes <- cellCodes[cellCodes %in% colnames(df)]
  
  # Filter shapeData
  regionCols <- paste0("is", simpleCap(region))
  shapeData <- shapeData[grepl("utm", names(shapeData))]
  shapeData <- sapply(shapeData, function(iShape)
      iShape[apply(sf::st_drop_geometry(iShape[, regionCols]), 1, sum) > 0, ],
    simplify = FALSE, USE.NAMES = TRUE)
  
    
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
    allCodes <- shapeData[[paste0("utm", gsub("cell_code", "", cellCodes), "_bel_with_regions")]]$CELLCODE
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
      iShape <- shapeData[[paste0("utm", gsub("cell_code", "", iCode), "_bel_with_regions")]]
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
#' @inheritParams trendYearRegion
#' @return plotly
#' 
#' @author mvarewyck
#' @import plotly
#' @importFrom INBOtheme inbo_lichtgrijs inbo_steun_blauw
#' @importFrom data.table setkey
#' @export
countOccurrence <- function(df, spatialLevel = c("1km", "10km"), minYear = 1950,
  period = c(2000, 2018), uiText, combine = FALSE) {
  
  
  # For R CMD check
  count <- year <- selected <- region <- NULL
  currentYear <- as.numeric(format(Sys.Date(), "%Y"))
  
  spatialLevel <- match.arg(spatialLevel)
  iCode <- switch(spatialLevel,
      '1km' = "cell_code1",
      '10km' = "cell_code10"
  )
  
  # Filter data
  df <- df[year > minYear, ][, selected := year >= period[1] & year <= period[2]]
  
  # Filter & color by regions
  regions <- c("flanders", "brussels", "wallonia")
  regionCols <- paste0("is", simpleCap(regions))
  if (any(regionCols %in% colnames(df))) {
    
    if (combine) {
      
      df$region <- factor(c("not selected", "selected")[df$selected + 1],
        levels = c("not selected", "selected"))
      
    } else {
      
      keepRegions <- regionCols %in% colnames(df)
      regions <- regions[keepRegions]
      regionCols <- regionCols[keepRegions]
      
      # Filter on regions
      df <- df[apply(df[, regionCols, with = FALSE], 1, sum) > 0, ]
      
      df$region <- factor(
        ifelse(!df$selected, "not selected", 
          ifelse(apply(df[, regionCols, with = FALSE], 1, sum) > 1, "multipleRegions",
            apply(df[, regionCols, with = FALSE], 1, function(x) regions[x > 0]))),
        levels = c("not selected", regions, "multipleRegions"))
      
    }
    
    # Rename
    newLevels <- as.list(levels(droplevels(df$region)))
    names(newLevels) <- translate(uiText, unlist(newLevels))$title
    levels(df$region) <- newLevels
    
  }
  
  
  if (!"count" %in% colnames(df))
    df[, count := length(unique(base::get(iCode))), by = .(year, region, selected)]
  
  if ("region" %in% colnames(df)) {
    # with region information
    
    nOccurred <- df[, .(count = sum(count)), by = .(year, region, selected)]
    setkey(nOccurred, year, region)
    returnData <- nOccurred[, .(year, region, count)]
    
  } else {
    
    nOccurred <- df[, .(count = sum(count)), by = .(year, selected)]
    setkey(nOccurred, year)
    returnData <- nOccurred
    
  }
  
  
  
  myPlot <- plot_ly(data = nOccurred[nOccurred$selected, ], 
      x = ~year, y = ~count, type = "bar",
      color = if (!is.null(nOccurred$region)) ~region, 
      text = if (!is.null(nOccurred$region)) ~region, 
      colors = inbo_palette(n = max(1, nlevels(df$region))), 
      hoverinfo = "x+y+text") %>%
    add_trace(data = nOccurred[!nOccurred$selected, ], 
        x = ~year, y = ~count, showlegend = FALSE,
        marker = list(color = inbo_lichtgrijs)) %>%
    layout(
      xaxis = list(title = translate(uiText, "year")$title, range = c(minYear, currentYear)),
      yaxis = list(title = ""),
      showlegend = !combine & !is.null(nOccurred$region),
      barmode = "stack",
      legend = list(orientation = 'h', x = 0.5, y = 1, xanchor = "center")
  )
  
  
  list(plot = myPlot, data = returnData)
  
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
    valuesPalette <- factor(paste0(groupNames, "x", groupNames, " km squares")) else
    valuesPalette <- groupNames
  
  list(
    colors = myColors,
    levels = valuesPalette
  )
  
}


#' Create base map of Belgium
#' @param map leaflet object, map to which a layer with belgian boundaries should be added
#' @param regions character vector, selected regions
#' @inheritParams countOccurrence
#' @return leaflet object
#' 
#' @author mvarewyck
#' @import leaflet
#' @importFrom rgdal readOGR
#' @importFrom rgeos gUnaryUnion
#' @export
addBaseMap <- function(map = leaflet(), 
  regions = c("flanders", "brussels", "wallonia"), combine = FALSE) {
  
  # For R CMD check
  GEWEST <- NULL
  
  
  if (is.null(regions))
    return(map)
  
  gewestBel <- readOGR(system.file("extdata", "grid", "gewestbel.shp", package = "alienSpecies"), 
    layer = "gewestbel", verbose = FALSE, stringsAsFactors = FALSE)
  
  matchingRegions <- data.frame(name = c("flanders", "brussels", "wallonia"), 
    shape = c("Vlaams", "Brussels", "Waals"))
  gewestBel <- subset(gewestBel, GEWEST %in% matchingRegions$shape[match(regions, matchingRegions$name)])
  
  if (combine)
    gewestBel <- rgeos::gUnaryUnion(gewestBel)
    
  map %>% 
    clearGroup("borderRegion") %>%
    addPolylines(
      data = gewestBel,
      color = "black", 
      opacity = 0.8, 
      weight = 3, 
      group = "borderRegion"
    ) %>% 
    addScaleBar(
      position = "bottomleft"
    )
  
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
mapCube <- function(cubeShape, baseMap = addBaseMap(), legend = "none", 
  addGlobe = FALSE,
  groupVariable) {
  
  
  myColors <- paletteMap(groupNames = names(cubeShape), groupVariable = groupVariable)
  palette <- colorFactor(palette = myColors$colors, levels = myColors$levels)
  
  myMap <- baseMap
  
  for (i in length(cubeShape):1)
   
     myMap <- myMap %>%
      addPolygons(
        data = cubeShape[[i]],
        weight = 1,
        color = ~ palette(myColors$levels[i]),
        fillOpacity = if (groupVariable != "cell_code" && i != length(cubeShape)) 0.5 else 0,
        popup = ~CELLCODE,
        group = myColors$levels[i]
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
#' @importFrom leaflet addMarkers addTiles `%>%` markerClusterOptions leaflet
#' @import data.table
#' @export
mapOccurrence <- function(occurrenceData, baseMap = addBaseMap(),
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
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import leaflet
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot webshot
#' @importFrom sf st_drop_geometry
#' @export
mapCubeServer <- function(id, uiText, species, df, shapeData,
  filter = reactive(NULL), groupVariable, showPeriod = FALSE
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
          
          if (!is.null(filter()))
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
      
      output$region <- renderUI({
          
          choices <- c("flanders", "wallonia", "brussels")
          names(choices) <- translate(uiText(), choices)$title
          
          selectInput(inputId = ns("region"), label = translate(uiText(), "regions"),
            choices = choices, multiple = TRUE, selected = choices)
          
        })
           
      output$period <- renderUI({
          
          req(df())
          
          periodChoice <- range(df()$year, na.rm = TRUE)
          
          div(style = "margin-left:50px; margin-right:10px;",
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
          if (!is.null(filter()))
            for (iFilter in names(filter())) {
              req(input[[iFilter]])
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
            need(nrow(subData()) > 0, noData()),
            need(input$region, noData()))
          
          createCubeData(
            df = subData(),
            shapeData = shapeData,
            groupVariable = groupVariable,
            region = input$region
          )
          
        })
      
      # Send map to the UI
      output$spacePlot <- renderLeaflet({
          
          if (is.null(shapeData)) {
            
            validate(need(nrow(subData()) > 0, noData()))
            
            mapOccurrence(occurrenceData = subData(),
              # when switching species, need to create correct basemap
              baseMap = addBaseMap(regions = input$region, combine = input$combine),
              addGlobe = TRUE)
            
          } else {
            
            validate(need(cubeShape(), noData()))
            
            mapCube(cubeShape = cubeShape(), groupVariable = groupVariable, 
              # when switching species, need to create correct basemap
              baseMap = addBaseMap(regions = isolate(input$region), combine = isolate(input$combine)),
              addGlobe = FALSE, legend = "topright")
            
          }
          
        })
      
      # Add border region
      observe({
          
          validate(need(input$region, noData()),
            need(!is.null(input$combine), noData()))
          
          proxy <- leafletProxy("spacePlot")
          
          addBaseMap(map = proxy, regions = input$region, combine = input$combine)
          
        })
      
      # Add world map
      observe({
          
          validate(need(cubeShape(), noData()))
          
          proxy <- leafletProxy("spacePlot")
          
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
              baseMap = addBaseMap(regions = req(input$region), combine = input$combine),
              addGlobe = input$globe %% 2 == 1
            )
            
          } else {
            
            newMap <- mapCube(
              cubeShape = cubeShape(),
              groupVariable = groupVariable,
              baseMap = addBaseMap(regions = req(input$region), combine = input$combine),
              legend = input$legend,
              addGlobe = input$globe %% 2 == 1
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
            vwidth = 1200, vheight = 600, cliprect = "viewport")
          
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
      
      observe({
          
          updateCheckboxInput(session = session, inputId = "combine",
            label = translate(uiText(), "combineRegions")$title)
          
        })  
      
      plotModuleServer(id = "countOccurrence",
        plotFunction = "countOccurrence", 
        data = reactive({
            validate(need(input$region, noData()))
            if (!is.null(shapeData))
              merge(filterData(), 
                # attach regions for coloring
                sf::st_drop_geometry(shapeData$utm1_bel_with_regions)[, c("CELLCODE", paste0("is", simpleCap(input$region)))], 
                by.x = "cell_code1", by.y = "CELLCODE", all.x = TRUE) else
              filterData()
          }),
        period = reactive(input$period),
        combine = reactive(input$combine),
        uiText = uiText
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
        column(6, uiOutput(ns("region"))),
        if (showLegend)
          column(6, 
            uiOutput(ns("legend"))
          ),
        if (showGlobe)
        column(6, 
          actionLink(inputId = ns("globe"), label = "Show globe",
            icon = icon("globe"))
        ),
      column(6, checkboxInput(inputId = ns("combine"), label = "Combine all selected regions"))
      )
    ),
    withSpinner(leafletOutput(ns("spacePlot"), height = "600px")),
    
    if (showPeriod) {
      tagList(
        plotModuleUI(id = ns("countOccurrence"), height = "200px"),
        uiOutput(ns("period"))
      )
    },
    
    tags$br(),
    
    tags$div(uiOutput(ns("downloadMapButton")), style = "display:inline-block;"),
    downloadButton(ns("downloadData"), label = "Download data", class = "downloadButton"),
    
    tags$hr()
  
  )
  
}
