

#' Combine all nesten data for regions summary
#' 
#' Returned object can be used as `data` in \code{\link{createSummaryRegions}}
#' @inheritParams combineNestenData
#' @param nestenBeheerdData sf data.frame, beheerde nesten data 
#' @return data.frame
#' 
#' @author mvarewyck
#' @export
combineVespaData <- function(pointsData, nestenData, nestenBeheerdData) {
  
  ## Individual data
  pointsData$type <- "individual"
  # Columns
  regionVariables <- list(level3Name = "NAAM", level2Name = "provincie", level1Name = "GEWEST")
  for (iName in names(regionVariables))
    names(pointsData)[match(iName, names(pointsData))] <- regionVariables[[iName]]
  # Gewest
  pointsData$GEWEST <- ifelse(pointsData$GEWEST == "Vlaanderen", "flanders", 
    ifelse(pointsData$GEWEST == "Bruxelles", "brussels", 
      ifelse(pointsData$GEWEST == "Wallonie", "wallonia", "")))
  # Provincie
  pointsData$provincie <- ifelse(pointsData$provincie == "Vlaams Brabant", "Vlaams-Brabant",
    ifelse(pointsData$provincie == "Bruxelles", "HoofdstedelijkGewest", 
      ifelse(pointsData$provincie == "Liège", "Luik", 
        ifelse(pointsData$provincie == "Brabant Wallon", "Waals-Brabant",
          ifelse(pointsData$provincie == "Hainaut", "Henegouwen", pointsData$provincie)))))
  pointsData$nest_type <- "individual"
  pointsData$isBeheerd <- FALSE
  
  ## Nest data
  nestenData$type <- "nest"
  nestenData$isBeheerd <- nestenData$geometry %in% nestenBeheerdData$geometry
  
  keepColumns <- c("year", "type", "nest_type", "NAAM", "provincie", "GEWEST", "isBeheerd", "geometry")
  vespaBoth <- rbind(pointsData[, keepColumns], nestenData[, keepColumns])
  vespaBoth$nest_type[vespaBoth$nest_type %in% c("NA", "NULL")] <- NA 
  
  vespaBoth
  
}



#' Create summary data per region
#' @param data data.table with management data
#' @param shapeData list, each object is \code{SpatialPolygonsDataFrame}. 
#' Needed for the \code{regionLevel} specified
#' @param regionLevel character, should be one of \code{c("communes", "provinces")}
#' @param year integer, year of interest
#' @param unit character, should be one of \code{c("cpue", "absolute")},
#' catch per unit of effort or absolute count
#' @param groupingVariable character, split the number of counts per group value
#' 
#' @return data.frame
#' 
#' @author mvarewyck
#' @import tidyverse
#' @importFrom data.table copy dcast
#' @importFrom reshape2 dcast
#' @importFrom stats as.formula
#' @export
createSummaryRegions <- function(data, shapeData, 
  regionLevel = c("communes", "provinces", "gewest"),
  year = NULL, unit = c("absolute", "cpue", "difference"), groupingVariable = NULL) {
  
  # For R CMD check
  region <- NULL
  eventID <- NULL
  n_fuiken <- NULL
  count <- NULL
  effort <- NULL
  n <- NULL
  
  unit <- match.arg(unit)
  
  data <- as.data.frame(data)
  
  
  if (is.null(year)) {
    
    myYear <- unique(data$year)
    
  } else if (is.list(year)) { 
    
    # create periods
    data$year <- cut(data$year, 
      breaks = c(year[[1]][1], sapply(year, function(x) tail(x, n = 1))), 
      include.lowest = TRUE,
      labels = sapply(year, function(x) if (length(x) == 1) x else paste(x[1], "-", tail(x, n = 1)))
    )
    myYear <- levels(data$year)
    
  } else {
    
    myYear <- year  # rename for tidyverse filtering
    
  }
  
  
  regionVariable <- switch(regionLevel,
    communes = "NAAM",
    provinces = "provincie",
    gewest = "GEWEST"
  )
  
  if (!regionVariable %in% colnames(data))
    return(NULL) else
    colnames(data)[colnames(data) == regionVariable] <- "region"
  
  if (is.null(data$count))
    data$count <- 1
  
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
    
  } else if (unit == "difference") {
    
    currentData <- data %>%
      filter(year %in% myYear, !is.na(region), region != "NA") %>% 
      group_by(region, year) %>% 
      summarise(nCurrent = sum(count, na.rm = TRUE))
    previousData <- data %>%
      filter(year %in% (myYear - 1), !is.na(region), region != "NA") %>% 
      group_by(region, year) %>% 
      summarise(nPrevious = sum(count, na.rm = TRUE))
    previousData$year <- previousData$year + 1
    summaryData <- merge(currentData, previousData)
    summaryData$n <- summaryData$nCurrent - summaryData$nPrevious
    
    summaryData$group <- cut(x = summaryData$n, 
      breaks = c(-Inf, -20, -10, 0, 10, 20, Inf),
      labels = c(paste0(min(-50, floor(min(summaryData$n, na.rm = TRUE))), ", -20"), "-20, -10", "-10, 0", 
          "0, 10", "10, 20", paste0("20, ", max(50, ceiling(max(summaryData$n, na.rm = TRUE)))))
    )
    
    summaryData$outcome <- ceiling(summaryData$n)
    
  } else {
    
    if (!is.null(groupingVariable)) {
      
      summaryData <- data %>%
        filter(year %in% myYear, !is.na(region), region != "NA", region != "")
      
      if (nrow(summaryData) == 0)
        return(NULL)
      
      myFormula <- as.formula(paste("region + year ~", paste(groupingVariable, collapse = " + ")))
      summaryData <- reshape2::dcast(summaryData, myFormula, 
        value.var = "count", fun.aggregate = sum)

      summaryData$n <- apply(summaryData[, -(1:2), drop = FALSE], 1, sum, na.rm = TRUE)     
      
    } else {
      
      summaryData <- data %>%
        filter(year %in% myYear, !is.na(region), region != "NA") %>% 
        group_by(region, year) %>% 
        summarise(n = sum(count, na.rm = TRUE))
      
    }
    
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
          "flanders" else if (regionLevel == "gewest")
          unique(shapeData$communes$GEWEST) else
          unique(shapeData[[regionLevel]]$NAAM)))
  allData <- merge(summaryData, fullData, all.x = TRUE, all.y = TRUE)
  allData$outcome[is.na(allData$outcome)] <- 0
  
  attr(allData, "unit") <- unit
  
  return(allData)
  
}


#' Map with occurrence and management for single species
#' @param managementData data.frame, management data; 
#' as returned by \code{\link{createSummaryRegions}}
#' @param occurrenceData data.frame, occurrence data
#' @param shapeData list with spatial data (grid and regions)
#' @inheritParams mapHeat
#' @param uiText data.frame, for translations
#' @param regionLevel character, region level to color polygons
#' @param palette character, color palette to be used, see also \code{\link[leaflet]{colorFactor}}
#' @param legend character, where to place legend
#' @param addGlobe boolean, whether to have background map
#' @return leaflet object
#' 
#' @author mvarewyck
#' @import leaflet
#' @export
mapRegions <- function(managementData, occurrenceData = NULL, shapeData, 
  baseMap = addBaseMap(), uiText = NULL,
  regionLevel = c("communes", "provinces"), palette = "YlOrBr",
  legend = "topright", addGlobe = FALSE) {
  
  spatialData <- shapeData[[regionLevel]]
  
  paletteFunction <- colorFactor(palette = palette, levels = levels(managementData$group), 
    na.color = "transparent",
    reverse = (palette != "YlOrBr"))
  valuesPalette <- managementData$group[match(spatialData$NAAM, managementData$region)]
  
  # Add borders
  if (regionLevel == "communes") {
    
    # TODO include in addBaseMap()
    myMap <- leaflet() %>% 
      addPolylines(
        data = shapeData$provinces, 
        weight = 3,
        color = "black",
        opacity = 0.8,
        group = "borderRegion"
      ) 
    
  } else if (regionLevel == "provinces") {
    
    myMap <- baseMap
    
  }
  
  
  myMap <- myMap %>% 
    addPolygons(
      data = spatialData,
      weight = 1,
      color = "gray",
      fillColor = ~ paletteFunction(valuesPalette),
      fillOpacity = 0.8,
      layerId = spatialData$NAAM,
      group = "region"
    )
  
  
  if (!is.null(occurrenceData)) {
    
    spread <- createCubeData(df = occurrenceData, shapeData = shapeData,
      groupVariable = "cell_code")
    myMap <- myMap %>%
      addPolylines(data = spread$cell_code1, 
        color = "blue",
        weight = 1) 
    
  }
  
  
  
  # Add legend
  if (legend != "none") { 
    
    myMap <- addLegend(
      map = myMap,
      position = legend,
      pal = paletteFunction, 
      values = valuesPalette,
      opacity = 0.8,
      title = translate(uiText, "legend")$title,
      layerId = "legend"
    )
    
    if (!is.null(occurrenceData))
      myMap <- addLegend(
        map = myMap,
        position = legend,
        colors = "blue",
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


#' Map with management for single species
#' @inheritParams mapRegions 
#' @return ggplot object
#' 
#' @author mvarewyck
#' @import ggplot2
#' @importFrom ggspatial annotation_map_tile
#' @export
mapRegionsFacet <- function(managementData, shapeData, uiText = NULL,
  regionLevel = c("communes", "provinces"), palette = "YlOrBr",
  legend = "right", addGlobe = FALSE) {
  
  # For R CMD check
  group <- NULL
  
  plotData <- merge(shapeData[[regionLevel]], managementData,
    by.x = "NAAM", by.y = "region", all.x = TRUE)
  
  # Facet plot
  myPlot <- ggplot() + 
    geom_sf(data = plotData, aes(fill = group), size = 0.5) + 
    facet_wrap(~ year, ncol = 3) +
    scale_fill_brewer(palette = palette) +
    theme_inbo(transparent = TRUE) + 
    theme(
      legend.position = legend,
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()) +
    guides(fill = guide_legend(title = translate(uiText, "legend")$title))
  
  if (addGlobe)
    # Add background globe
    myPlot <- myPlot + ggspatial::annotation_map_tile(
        zoom = 7, alpha = 0.5, forcedownload = FALSE,
        cachedir = system.file("extdata", package = "alienSpecies")) +
      # redraw polygons
      geom_sf(data = plotData, aes(fill = group), size = 0.5) +
      labs(caption = "\u00a9 OpenStreetMap contributors")
  
  if (regionLevel == "communes")
    # Add province borders
    myPlot <- myPlot +
      geom_sf(data = shapeData$provinces, fill = NA, color = "black", size = 1)
  
  
  myPlot
  
}


#' Create popup text to display in \code{\link{mapRegions}}
#' @param summaryData data.frame, as returned by \code{\link{createSummaryRegions}}
#' @inheritParams mapRegionsServer 
#' @inheritParams createSummaryRegions
#' @param bronMap character vector, sources to be shown in the popup
#' @return character vector with popup text for each row in \code{summaryData}
#' 
#' @author mvarewyck
#' @importFrom xtable xtable
#' @importFrom reshape2 melt dcast
#' @export
mapPopup <- function(summaryData, uiText, year, unit, bronMap) {
  
  
  paste0("<h4>", summaryData$region, "</h4>",
    "<strong>", translate(uiText, "year")$title, "</strong>: ", year, "<br>",
    if (!is.null(unit)) 
      paste0("<strong>", translate(uiText, unit)$title, "</strong>: "), 
    if (!is.null(bronMap)) {
      lapply(split(summaryData, summaryData$region), function(iData) {
            tmpData <- suppressWarnings(reshape2::melt(iData, id.vars = colnames(iData)[1:2]))
            tmpData$nest <- sapply(strsplit(as.character(tmpData$variable), split = "_"), function(x) x[1])
            tmpData$isBeheerd <- sapply(strsplit(as.character(tmpData$variable), split = "_"), function(x) { 
                if (length(x) > 1) {
                  if (x[2] == "TRUE")
                  "managed nest" else if (x[2] == "FALSE")
                  "untreated nest"
              } else NA
              })
            tmpData <- tmpData[!is.na(tmpData$isBeheerd), ]
            formattedTable <- reshape2::dcast(tmpData[, c("nest", "isBeheerd", "value")], nest ~ isBeheerd, value.var = "value")
            formattedTable$nest[formattedTable$nest == "NA"] <- "unknown"
            formattedTable$nest <- translate(uiText, formattedTable$nest)$title
            formattedTable <- formattedTable[order(formattedTable$nest), ]
            colnames(formattedTable) <- translate(uiText, colnames(formattedTable))$title
            
            as.character(print(xtable::xtable(formattedTable), 
                include.rownames = FALSE, type = "html", print.results = FALSE))
          })
    } else {
    if (!is.null(unit) && unit == "cpue") 
        round(summaryData$effort, 2) else
        round(summaryData$n, 2)
    }
  )
  
}

#' Shiny module for creating the plot \code{\link{mapCube}} - server side
#' 
#' @inheritParams welcomeSectionServer
#' @inheritParams createCubeData
#' @inheritParams mapCubeServer
#' @inheritParams mapCubeUI
#' @inheritParams createSummaryRegions
#' @param df reactive data.frame, data as loaded by \code{\link{loadGbif}}
#' @param occurrenceData data.table, as obtained by \code{loadTabularData(type = "occurrence")}
#' @param sourceChoices character vector, choices for the data source;
#' default value is NULL then no choices are shown
#' @param facet boolean, if TRUE a static facet plot is created; if FALSE an
#' interactive leaflet map is created
#' 
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import leaflet
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot webshot
#' @importFrom sf st_drop_geometry
#' @importFrom ggplot2 ggsave
#' @export
mapRegionsServer <- function(id, uiText, species, gewest, df, occurrenceData, shapeData,
  sourceChoices = NULL, facet = FALSE) {
  
  moduleServer(id,
    function(input, output, session) {
      
      # For R CMD check
      year <- NULL
      currentYear <- as.numeric(format(Sys.Date(), "%Y")) - 1
      
      ns <- session$ns
      
      results <- reactiveValues()
      
      noData <- reactive(translate(uiText(), "noData"))
      tmpTranslation <- reactive(translate(uiText(), 
          if (!facet)
            "management-mapOccurrence" else
            "management-mapInvasion"))
      
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
                if (!facet)
                  paste0(" (", translate(uiText(), "map")$title, ")")),
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
          
          choices <- c("absolute", "difference", "cpue")
          names(choices) <- translate(uiText(), choices)$title
          
          selectInput(inputId = ns("unit"), label = translate(uiText(), "unit")$title, 
            choices = choices)
          
        })
      
      output$bronMap <- renderUI({
          
          req(sourceChoices)
          
          selectInput(inputId = ns("bronMap"),
            label = translate(uiText(), "source")$title,
            choices = sourceChoices, selected = sourceChoices,
            multiple = TRUE)
          
        })
            
      subShape <- reactive({
          
          req(gewest())
          # Subset for GEWEST
          lapply(shapeData, function(iData) {
              if ("GEWEST" %in% colnames(iData)){
                iData$GEWEST <- dplyr::recode(iData$GEWEST, "Brussels" = "brussels", "Vlaams"="flanders", "Waals" =  "wallonia")
                iData[iData$GEWEST %in% gewest(), ]
              }else{
                iData[apply(sf::st_drop_geometry(iData[, paste0("is", simpleCap(gewest())), drop = FALSE]), 1, sum) > 0, ]
              }
            })
          
        })
      
      output$regionLevel <- renderUI({
          
          choices <- c("communes", "provinces")
          names(choices) <- translate(uiText(), choices)$title
          
          selectInput(inputId = ns("regionLevel"), label = translate(uiText(), "regionLevel")$title,
            choices = choices)
          
        })
      
      output$region <- renderUI({
          
          choices <- sort(unique(subShape()[[req(input$regionLevel)]]$NAAM))
          # expected to be missing for some region levels
          names(choices) <- suppressWarnings(translate(uiText(), choices)$title)
          
          selectInput(inputId = ns("region"), label = translate(uiText(), "regions")$title,
            choices = choices, multiple = TRUE)
          
        })
      
      # Map attributes
      output$legend <- renderUI({
          
          legendChoices <- if (facet)
              c("bottom", "top", "right", "left", "none") else
              c("topright", "bottomright", "topleft", "bottomleft", "none")
          names(legendChoices) <- sapply(legendChoices, function(x) translate(uiText(), x)$title)
          
          selectInput(inputId = ns("legend"), 
            label = translate(uiText(), "legend")$title,
            choices = legendChoices)
          
        })
      
      
      # Subset on filters
      subData <- reactive({
          
          req(inherits(df(), "data.frame"))
          
          subData <- df()
          
          if (!is.null(sourceChoices)) {
            req(input$bronMap)
            subData <- subData[subData$type %in% input$bronMap, ]
          }
          
          subData
          
        })
      
      
      summaryData <- reactive({
          
          req(nrow(subData()) > 0)
          req(input$year)
          
          createSummaryRegions(data = subData(), 
            shapeData = shapeData,
            regionLevel = req(input$regionLevel),
            year = if (facet)
              list(
                c(input$year-8, input$year-5), 
                c(input$year-4, input$year-1),
                input$year) else
                input$year, 
            unit = input$unit,
            groupingVariable = if (!is.null(sourceChoices)) c("nest_type", "isBeheerd")
          )
          
        })
      
      
      # Filter Occurrence data
      subOccurrence <- reactive({
          
          # Filter on taxonKey and year
          occurrenceData <- occurrenceData[occurrenceData$scientificName == species() & occurrenceData$year == req(input$year), ]
          
        })
      
      
      # Send map to the UI
      output$regionsPlot <- renderLeaflet({
          
          req(!facet)
          validate(need(nrow(req(summaryData())) > 0, noData()))
          
          mapRegions(
            managementData = summaryData(),
            occurrenceData = subOccurrence(),
            shapeData = subShape(), 
            uiText = uiText(), 
            regionLevel = input$regionLevel,
            baseMap = addBaseMap(regions = gewest()),
            addGlobe = isolate(input$globe %% 2 == 1),
            palette = if (!is.null(input$unit) && input$unit == "difference") "RdYlGn" else "YlOrBr"
          )
          
        })
      
      output$regionsPlotFacet <- renderPlot({
          
          req(facet)
          validate(need(nrow(req(summaryData())) > 0, noData()))
          
          mapRegionsFacet(
            managementData = summaryData(),
            shapeData = subShape(), 
            uiText = uiText(), 
            regionLevel = input$regionLevel,
            legend = input$legend,
            addGlobe = input$globe,
            palette = if (!is.null(input$unit) && input$unit == "difference") "RdYlGn" else "YlOrBr"
          )
          
        })
      
      # Shape data for regionLevel
      spatialData <- reactive({
          
          shapeData[[req(input$regionLevel)]]
          
        })
      
      # Define text to be shown in the pop-ups
      textPopup <- reactive({
          
          validate(need(nrow(req(summaryData())) > 0, noData()))
          
          mapPopup(summaryData = summaryData(), uiText = uiText(), year = input$year,
            unit = input$unit, bronMap = input$bronMap)
                              
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
          req(!facet)
          req(summaryData())
          
          proxy <- leafletProxy("regionsPlot")
          proxy %>% 
            removeControl(layerId = "legend") %>% 
            removeControl(layerId = "legend2")
          
          if (input$legend != "none") {
            
            palette <- if (!is.null(input$unit) && input$unit == "difference") "RdYlGn" else "YlOrBr"
            paletteFunction <- colorFactor(palette = palette, levels = levels(summaryData()$group), 
              na.color = "transparent", reverse = (palette != "YlOrBr"))
            valuesPalette <- summaryData()$group[match(spatialData()$NAAM, summaryData()$region)]
            
            
            proxy %>% addLegend(
              position = input$legend,
              pal = paletteFunction, 
              values = valuesPalette,
              opacity = 0.8,
              title = translate(uiText(), "legend")$title,
              layerId = "legend"
            )
            
            if (!is.null(occurrenceData))
              proxy %>% addLegend(
                position = input$legend,
                colors = "blue",
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
          
          req(summaryData())
          
          if (facet) {
            
            mapRegionsFacet(
              managementData = summaryData(),
              shapeData = subShape(),
              uiText = uiText(), 
              regionLevel = input$regionLevel,
              legend = input$legend,
              addGlobe = input$globe,
              palette = if (!is.null(input$unit) && input$unit == "difference") "RdYlGn" else "YlOrBr"
            )
            
          } else {
            
          newMap <- mapRegions(
            managementData = summaryData(), 
            occurrenceData = subOccurrence(), 
            shapeData = subShape(), 
            baseMap = addBaseMap(regions = gewest()),
            uiText = uiText(), regionLevel = input$regionLevel,
            legend = input$legend, addGlobe = input$globe %% 2 == 1,
            palette = if (input$unit == "difference") "RdYlGn" else "YlOrBr")
          
          
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
          
        }
        
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
          
          if (facet) {
            
            ggplot2::ggsave(file, plot = finalMap())
            
          } else {
            
            # convert temp .html file into .png for download
            webshot::webshot(url = finalMap(), file = file,
              vwidth = 1000, vheight = 500, cliprect = "viewport")
            
          }
        
        }
      )
      
      output$downloadData <- downloadHandler(
        filename = function()
          nameFile(species = species(),
            period = input$year, 
            content = if (!facet) "management_data" else "invasion_data", 
            fileExt = "csv"),
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
            data = subData(), 
            shapeData = shapeData,
            regionLevel = "gewest",
            year = input$period[1]:input$period[2],
            unit = input$unit
          )
          
        })
      
      plotModuleServer(id = "timePlotFlanders",
        plotFunction = "trendYearRegion", 
        data = reactive({
            timeDataFlanders()[timeDataFlanders()$region %in% req(gewest()), ]
          }),
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
            data = subData(),
            shapeData = shapeData,
            regionLevel = req(input$regionLevel),
            year = input$period[1]:input$period[2],
            unit = input$unit
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
#' @param showUnit boolean, whether to show the option to choose unit;
#' default is TRUE
#' @inheritParams mapRegionsServer
#' @return UI object
#' 
#' @author mvarewyck
#' @import shiny
#' @importFrom leaflet leafletOutput
#' @export
mapRegionsUI <- function(id, plotDetails = NULL, showUnit = TRUE, facet = FALSE) {
  
  ns <- NS(id)
  
  # Occurrence Map
  
  tags$div(class = "container",
    
    uiOutput(ns("titleMapRegions")),
    
    uiOutput(ns("descriptionMapRegions")),
    
    wellPanel(
      fixedRow(
        column(6, uiOutput(ns("regionLevel"))),
        if (!facet)
          column(6, uiOutput(ns("region"))),
        column(6, uiOutput(ns("year"))),
        if (!facet)
          column(6, uiOutput(ns("period")))
      ),
      fixedRow(
        column(6, uiOutput(ns("legend"))),
        if (showUnit)
          column(6, uiOutput(ns("unit"))),
        column(6, uiOutput(ns("bronMap")))
      ),
      if ("region" %in% plotDetails)
        checkboxInput(inputId = ns("combine"), 
          label = "Combine all selected regions"),
      actionLink(inputId = ns("globe"), label = "Show globe", icon = icon("globe"))
    ),
    
    if (!facet)
        withSpinner(leafletOutput(ns("regionsPlot"))) else
        withSpinner(plotOutput(ns("regionsPlotFacet"))),
  
    
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
