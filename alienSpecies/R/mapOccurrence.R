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
      combinedData <- setDT(df)[duplicated(df, by = cellCodes), ][, source := combinedGroup]
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
#' @param regions character vector to filter data wrt certain 'gewest',
#' available choices is (subset of) \code{c("flanders", "brussels", "wallonia")}; 
#' NULL by default; if NULL all available regions are seleted
#' @param addYLabel boolean whether to add a Y label
#' @inheritParams trendYearRegion
#' @return plotly
#' 
#' @author mvarewyck
#' @import plotly
#' @importFrom INBOtheme inbo_lichtgrijs inbo_steun_blauw
#' @importFrom data.table setkey uniqueN
#' @export
countOccurrence <- function(df, spatialLevel = c("1km", "10km"), minYear = 1950,
  period = c(2000, 2018), combine = FALSE, 
  regions = NULL, addYLabel = FALSE) {
  
  
  # For R CMD check
  count <- year <- selected <- region <- . <- NULL
  
  if (is.null(regions))
    regions <- c("flanders", "brussels", "wallonia")
  currentYear <- as.numeric(format(Sys.Date(), "%Y"))
  
  spatialLevel <- match.arg(spatialLevel)
  iCode <- switch(spatialLevel,
    '1km' = "cell_code1",
    '10km' = "cell_code10"
  )
  
  yLabel <- ifelse(
    addYLabel,
    translate("countOccurrence_yLabel")$title,
    ""
  )
  
  # Filter on selected period
  df <- df[year > minYear, ][, selected := year >= period[1] & year <= period[2]]
  
  # Filter & color by regions
  allColors <- c(inbo_lichtgrijs, inbo_palette(n = 4))
  names(allColors) <- c("not selected", if (combine) "selected", 
    regions, if (!combine) "multipleRegions")
  regionCols <- paste0("is", simpleCap(regions))
  
  if (any(regionCols %in% colnames(df))) {
  
    # Filter on selected regions
    keepRegions <- regionCols %in% colnames(df)
    regions <- regions[keepRegions]
    regionCols <- regionCols[keepRegions]
    df <- df[apply(df[, regionCols, with = FALSE], 1, sum) > 0, ]
    
    if (combine) {
      
      df$region <- factor(c("not selected", "selected")[df$selected + 1],
        levels = c("not selected", "selected"))
      
    } else {
      
      df$region <- factor(
        ifelse(!df$selected, "not selected", 
          ifelse(apply(df[, regionCols, with = FALSE], 1, sum) > 1, "multipleRegions",
            apply(df[, regionCols, with = FALSE], 1, function(x) regions[x > 0]))),
        levels = c("not selected", regions, "multipleRegions"))
      
    }
    
    # Rename
    newLevels <- as.list(levels(droplevels(df$region)))
    names(newLevels) <- translate(unlist(newLevels))$title
    names(allColors) <- translate(names(allColors))$title
    levels(df$region) <- newLevels
    
  }
  
  if (!"count" %in% colnames(df))
    df <- df[, .(count = uniqueN(base::get(iCode))), by = .(year, region, selected)]
  
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
      textposition = "none",
      colors = allColors, 
      hoverinfo = "x+y+text") %>%
    add_trace(data = nOccurred[!nOccurred$selected, ], 
        x = ~year, y = ~count, showlegend = FALSE,
        marker = list(color = inbo_lichtgrijs)) %>%
    layout(
      xaxis = list(title = translate("year")$title, range = c(minYear, currentYear)),
      yaxis = list(title = list(
          text = yLabel,
          font = list(size = 10)
        )),
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
  
  myPalette <- c(adjustcolor("black", alpha.f = 0.1), "red", inbo_palette())

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
#' @importFrom sf st_union
#' @export
addBaseMap <- function(map = leaflet(), 
  regions = c("flanders", "brussels", "wallonia"), combine = FALSE) {
  
  # For R CMD check
  GEWEST <- NULL
  gewestbel <- NULL
  
  if (is.null(regions))
    return(map)
    
  readS3(file = "gewestbel.RData", envir = environment())
  
  matchingRegions <- data.frame(name = c("flanders", "brussels", "wallonia"), 
    shape = c("Vlaams", "Brussels", "Waals"))
  gewestbel <- subset(gewestbel, GEWEST %in% matchingRegions$shape[match(regions, matchingRegions$name)])
  
  if (!is.null(combine) && combine)
    gewestbel <- sf::st_union(gewestbel)
    
  map %>% 
    clearGroup("borderRegion") %>%
    addPolylines(
      data = gewestbel,
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
  addGlobe = FALSE, groupVariable) {
  
  
  myColors <- paletteMap(groupNames = names(cubeShape), groupVariable = groupVariable)
  palette <- colorFactor(palette = myColors$colors, levels = myColors$levels)
  
  myMap <- baseMap
  
  fillOpacities <- numeric(length(cubeShape))
  for (i in length(cubeShape):1) {
    fillOpacity <- if (groupVariable != "cell_code" && i != length(cubeShape)) 
        0.5 
      else if (i != length(cubeShape)) 
        0.35
      else 
        0
    
    fillOpacities[i] <- fillOpacity
    myMap <- myMap %>%
      addPolygons(
        data = cubeShape[[i]],
        weight = if (i != length(cubeShape) && groupVariable == "cell_code") 2 else 1,
        color = if (i != length(cubeShape)) ~ palette(myColors$levels[i]) else "black",
        fillOpacity = fillOpacity,
        popup = ~CELLCODE,
        group = myColors$levels[i]
      )
  }
  
  # Add legend
  if (legend != "none") { 
  
      myMap <- leaflegend::addLegendFactor(
        map = myMap,
        position = legend,
        pal = palette, 
        values = myColors$levels,
        opacity = 1,
        fillOpacity = rev(fillOpacities),
        title = translate("legend")$title,
        layerId = "legendCustom"
      ) 
  }
  
  # Add background map
  if (addGlobe) {
    
    myMap <- addProviderTiles(myMap, providers$CartoDB.Positron)
    
  }
  
  
  list(map = myMap, opacity = fillOpacities)
  
}


#' Create leaflet map for the occurrence **management** data
#' 
#' @param occurrenceData data.table, as loaded via \code{\link{loadGbif}}
#' @inheritParams mapCube
#' @return leaflet map
#' 
#' @author mvarewyck
#' @importFrom leaflet addMarkers addProviderTiles `%>%` markerClusterOptions leaflet
#' @import data.table
#' @export
mapOccurrence <- function(occurrenceData, baseMap = addBaseMap(),
  addGlobe = FALSE) {
  
  # For R CMD check
  count <- decimalLongitude <- decimalLatitude <- . <- NULL
  
  if (!all(c("count", "decimalLongitude", "decimalLatitude") %in% colnames(occurrenceData)))
    return(NULL)
  
  ## Sum counts over ID
  occurrenceData <- occurrenceData[, .(count = sum(count)),
    by = .(decimalLongitude, decimalLatitude)] 
  
  
  myMap <- baseMap
  
  # Add background map - needed for clusters to be shown and before addMarkers()
  if (addGlobe) {
    
    myMap <- addProviderTiles(myMap, providers$CartoDB.Positron)
    
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
#' values define the choices in \code{selectInput}
#' @inheritParams welcomeSectionServer
#' @inheritParams createCubeData
#' @inheritParams mapCubeUI
#' @param species reactive character, readable name of the selected species
#' @param gewest reactive character, name of the selected region(s)
#' @param df reactive data.frame, data as loaded by \code{\link{loadGbif}}
#' @param dashReport reactive value, contains all objects for creating the report;
#' plot and parameters for current plot will be added with id \code{ns("mapOccurrence")}
#' @param triggerReport reactive object, updates when downloading the report and
#' creates all missing (non-triggered) info for the report
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import leaflet
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot2 webshot
#' @importFrom sf st_drop_geometry
#' @export
mapCubeServer <- function(id, species, gewest, df, shapeData,
  filter = reactive(NULL), groupVariable, showPeriod = FALSE, dashReport = NULL,
  triggerReport = reactive(NULL)
) {
  
  moduleServer(id,
    function(input, output, session) {
      
      # For R CMD check
      year <- NULL
      currentYear <- as.numeric(format(Sys.Date(), "%Y"))
      
      ns <- session$ns
      tmpFile <- tempfile(fileext = ".html")
      results <- reactiveValues()

      noData <- reactive(translate("noData")$title)
      tmpTranslation <- reactive(translate(ns("mapOccurrence")))
      
      tmpTranslation <- reactive({
          tmpID <- ns(paste0("mapOccurrence_", gsub(" ", "_", tolower(species()))))
          if (id == "management" && (translate(tmpID)$title != tmpID)) {
            translate(tmpID)
          } else {
            translate(ns("mapOccurrence"))
          }
        })
      
      output$descriptionMapOccurrence <- renderUI(
        decodeText(tmpTranslation()$description, params = list(species = species())))
      
      title <- reactive({
          
          req(species())
          
          decodeText(tmpTranslation()$title, 
            params = c(
              list(species = species()), 
              if (showPeriod && !is.null(input$period)) 
                list(period = input$period)
            )
          )
          
        })
      
      output$titleMapOccurrence <- renderUI(h3(HTML(title())))
      
      output$filters <- renderUI({
          
          if (!is.null(filter()))
            lapply(names(filter()), function(filterName) {
                
                choices <- filter()[[filterName]]
                names(choices) <- translate(choices)$title
                
                column(6, 
                  selectInput(inputId = ns(filterName), 
                    label = translate(filterName)$title,
                    choices = choices,
                    multiple = TRUE, selected = filter()[[filterName]])
                )
              })
          
        })
      
      output$period <- renderUI({
          
          req(df())
          
          periodChoice <- c(1950, currentYear)
          
          tagList(
            tags$script(sprintf("
                  $(document).ready(function() {
                  $('#%s').on('blur', function() {
                  Shiny.setInputValue('%s', this.value, {priority: 'event'});
                  });
                  });
                  ", ns("periodStart"), ns("periodStart_blur"))),
            tags$script(sprintf("
                  $(document).ready(function() {
                  $('#%s').on('blur', function() {
                  Shiny.setInputValue('%s', this.value, {priority: 'event'});
                  });
                  });
                  ", ns("periodEnd"), ns("periodEnd_blur"))),
            div(style = "margin-left:10px; margin-right:10px;",
              tagList(
                sliderInput(
                  inputId = ns("period"), 
                  label = NULL,
                  min = 1950,
                  max = currentYear,
                  value = periodChoice,
                  sep = "", 
                  width = "100%"
                ),
                fluidRow(
                  column(2, numericInput(ns("periodStart"), translate("startYear")$title, value = 1950)),
                  column(2, offset = 8, numericInput(ns("periodEnd"), translate("endYear")$title, value = currentYear))
                
                )
              )
            )
          )
          
        })
      
      observeEvent(input$period, priority = 5, {
          if (input$period[1] != input$periodStart) {
            updateNumericInput(session = session, inputId = "periodStart", value = input$period[1])
          }
          if (input$period[2] != input$periodEnd) {
            updateNumericInput(session = session, inputId = "periodEnd", value = input$period[2])
          }
          
        })
      
      observeEvent(input$periodStart_blur, priority = 5, {
          if (input$period[1] != input$periodStart) {
            updateSliderInput(session = session, inputId = "period", value = c(input$periodStart, input$period[2]))
          }
          
        })
      
      observeEvent(input$periodEnd_blur, priority = 5, {
          if (input$period[2] != input$periodEnd) {
            updateSliderInput(session = session, inputId = "period", value = c(input$period[1], input$periodEnd))
          }
          
        })
      
      
      output$legend <- renderUI({
          
          legendChoices <- c("topright", "bottomright", "topleft", "bottomleft", "none")
          names(legendChoices) <- sapply(legendChoices, function(x) translate(x)$title)
          
          selectInput(inputId = ns("legend"), 
            label = translate("legend")$title,
            choices = legendChoices)
          
        })
      
      
      # Subset on filters
      filterData <- reactive({
          
          filterData <- df()
          
          # Other filters
          if (!is.null(filter()))
            for (iFilter in names(filter())) {
              if (!is.null(input[[iFilter]]))
                filterData <- filterData[filterData[[iFilter]] %in% input[[iFilter]], ]
            }
          
          filterData
          
        })
      
      # Subset on period
      subData <- reactive({
          
          # Filter on time
          if (showPeriod && !is.null(input$period)) {
              
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
            groupVariable = groupVariable,
            region = gewest()
          )
          
        })
      
      mapOccurrenceLeaflet <- reactive({
          
          req(is.null(shapeData))
          
          validate(need(nrow(subData()) > 0, noData()))
          
          mapOccurrence(occurrenceData = subData(),
            # when switching species, need to create correct basemap
            baseMap = addBaseMap(regions = gewest(), combine = input$combine),
            addGlobe = isolate(input$globe %% 2 == 0))
          
        })
      
      mapCubeLeaflet <- reactive({
          
          req(!is.null(shapeData))
          if (showPeriod)
            req(input$period)
          
          validate(need(cubeShape(), noData()))
          
          results$opacities <- NULL
          
          outp <- mapCube(cubeShape = cubeShape(), groupVariable = groupVariable, 
            # when switching species, need to create correct basemap
            baseMap = addBaseMap(regions = isolate(gewest()), combine = isolate(input$combine)),
            addGlobe = FALSE, legend = "topright")
          
          results$opacities <- outp$opacity
          
          outp$map
        })
      
      # Send map to the UI
      output$spacePlot <- renderLeaflet({
          result <- tryCatch({
              
              if (is.null(shapeData))
                mapOccurrenceLeaflet() %>%
                  leaflet.extras::addFullscreenControl()
              else {
                
                myMap <- mapCubeLeaflet() %>%
                  leaflet.extras::addFullscreenControl()
                
                myMap
              }
            
            }, error = function(e) {
              NULL
            })
          
          return(result)
        })
      
      output$spacePlotMessage <- renderUI({
          msg <- tryCatch({
              if (is.null(shapeData))
                mapOccurrenceLeaflet()
              else
                mapCubeLeaflet()
              NULL
            }, error = function(e) conditionMessage(e))
          
          if (is.null(msg)) {
            return(NULL)
          } else {
            div(style = "color:#595959; margin: 1em 0;",
              msg)
          }
        })
      
      # Add border region
      observe({
          
          validate(need(!is.null(input$combine), noData()))
          
          proxy <- leafletProxy("spacePlot")
          
          addBaseMap(map = proxy, regions = gewest(), combine = input$combine)
          
        })
      
      # Add world map
      observe({
          
          validate(need(cubeShape(), noData()))
          
          proxy <- leafletProxy("spacePlot")
          
          if (!is.null(input$globe) & !is.null(proxy)){
            
            if (input$globe %% 2 == 0){
              
              updateActionLink(session, inputId = "globe", 
                label = translate("hideGlobe")$title)
              
              proxy %>% addProviderTiles(providers$CartoDB.Positron)
              
            } else {
              
              updateActionLink(session, inputId = "globe", 
                label = translate("showGlobe")$title)
              
              proxy %>% clearTiles()
              
            }
            
          }
          
        })
      
      
      # Add legend
      observe({
          
          validate(need(cubeShape(), noData()))
          
          req(input$legend)
          
          proxy <- leafletProxy("spacePlot")
          proxy %>% removeControl(layerId = "legendCustom")
          
          if (input$legend != "none") {
            req(results$opacities)
            
            myColors <- paletteMap(groupNames = names(cubeShape()), 
              groupVariable = groupVariable)
            palette <- colorFactor(palette = myColors$colors, levels = myColors$levels)
            
            proxy %>% leaflegend::addLegendFactor(
                position = input$legend,
                pal = palette, 
                values = myColors$levels,
                opacity = 1,
                fillOpacity = rev(results$opacities),
                title = translate("legend")$title,
                width = 15, height = 15,
                layerId = "legendCustom"
              ) 
          }              
          
        })
          
      
      # Create final map (for download)
      finalMap <- reactive({
          
          if (is.null(shapeData)) {
            
            newMap <- mapOccurrence(
              occurrenceData = req(subData()), 
              baseMap = addBaseMap(regions = req(gewest()), combine = input$combine),
              addGlobe = if (is.null(input$globe)) 
                  TRUE else 
                  input$globe %% 2 == 0
            ) 
            
          } else {
            
            newMap <- mapCube(
              cubeShape = req(cubeShape()),
              groupVariable = groupVariable,
              baseMap = addBaseMap(regions = req(gewest()), combine = input$combine),
              legend = if (is.null(input$legend)) "topright" else input$legend,
              addGlobe = if (is.null(input$globe)) 
                  TRUE else 
                  input$globe %% 2 == 0
            )$map 
            
          }
          
          # save the zoom level and centering to the map object
          if (!is.null(input$spacePlot_center))
            newMap <- newMap %>% setView(
              lng = input$spacePlot_center$lng,
              lat = input$spacePlot_center$lat,
              zoom = input$spacePlot_zoom
            )
          
          # write map to temp .html file
          req(newMap)
          htmlwidgets::saveWidget(newMap, file = tmpFile, selfcontained = FALSE)
          
          # output is path to temp .html file containing map
          tmpFile
          
        }) 
      
      
      # Download the map
      output$downloadMapButton <- renderUI({
          downloadButton(ns("download"), 
            label = translate("downloadMap")$title, 
            class = "downloadButton")
#          actionButton(ns("download"), 
#            label = translate("downloadMap")$title, 
#            icon = icon("download"),
#            class = "btn-default shiny-download-link downloadButton", type = "button")
        })
      
#      observeEvent(input$download, {
#          browser()
#          leafletProxy("spacePlot") %>% 
#            removeControl(layerId = "legendCustom") %>% leaflet.extras2::easyprintMap(
#            sizeModes = "CurrentSize",
#            filename = nameFile(species = species(),
#              period = input$period, 
#              content = id, fileExt = "png")
#          )
#          
#        })
      
      output$download <- downloadHandler(
        filename = function()
          nameFile(species = species(),
            period = input$period, 
            content = id, fileExt = "png"),
        content = function(file) {
          
          # convert temp .html file into .png for download
          webshot2::webshot(url = finalMap(), file = file,
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
            label = translate("combineRegions")$title)
          
        })  
      
      barplot <- plotModuleServer(id = "countOccurrence",
        plotFunction = "countOccurrence", 
        data = reactive({
            validate(need(gewest(), noData()))
            req(filterData())
            filterData()
          }),
        period = reactive(input$period),
        combine = reactive(input$combine),
        regions = gewest,
        addYLabel = grepl("observations", id)
      )
      
      
      ## Report Objects ##
      ## -------------- ##
            
      observeEvent(triggerReport(), {
          
          if (any(startsWith(id, c("management", "observation")))) {
            tmpFile <- tempfile(fileext = ".html")
            htmlwidgets::saveWidget(barplot()$plot %>% layout(font = list(size = 30)), file = tmpFile, selfcontained = FALSE)
            tmp_png <- tempfile(fileext = ".png")
            dir.create(dirname(tmp_png), showWarnings = FALSE, recursive = TRUE)
            webshot2::webshot(tmpFile, file = tmp_png, vwidth = 2300, vheight = 500)
          } else {
            tmp_png <- NULL
          }
          
          # Return the static values
          dashReport[[ns("mapOccurrence")]] <- c(
                list(
                  plot = isolate(finalMap()),
                  barplot = isolate(tmp_png),
                  title = isolate(title()),
                  description = isolate(decodeText(tmpTranslation()$description, params = list(species = species()))),
                  showPeriod = (showPeriod && !is.null(input$period))
                ),
                reactiveValuesToList(input)
              )
          
        })
      
      
      return(dashReport)
      
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
    
    if (!grepl("observations", id)) {
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
            ),
          column(6, checkboxInput(inputId = ns("combine"), label = "Combine all selected regions"))
        )
      )
    },
    uiOutput(ns("spacePlotMessage")),
    withSpinner(leafletOutput(ns("spacePlot"), height = "600px")),
    
    if (!grepl("observations", id) && showPeriod) {
      tagList(
        plotModuleUI(id = ns("countOccurrence"), height = "200px"),
        uiOutput(ns("period"))
      )
    },
    
    if (grepl("observations", id))
      tagList(
        plotModuleUI(id = ns("countOccurrence"), height = "200px"),
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
              ),
            column(6, checkboxInput(inputId = ns("combine"), label = "Combine all selected regions")),
            if (showPeriod)
              column(12, uiOutput(ns("period"))
          )
        )
      )),
    
    tags$br(),
    
    tags$div(uiOutput(ns("downloadMapButton")), style = "display:inline-block;"),
    downloadButton(ns("downloadData"), label = "Download data", class = "downloadButton"),
    
    tags$hr()
  
  )
  
}
