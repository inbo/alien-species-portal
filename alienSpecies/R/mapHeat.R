#' Combine data for observations on Actieve haarden data
#' 
#' Used on Management page for Vespa Velutina
#' 
#' @param activeData sf, points data with actieve haarden
#' @param untreatedData sf, points data with onbehandelde nesten
#' @param managedData sf, points data with beheerde nesten; default is NULL
#' @return sf data.frame, combining all data sources
#' 
#' @author mvarewyck
#' @importFrom dplyr select filter mutate group_by summarise
#' @export
combineActiveData <- function(activeData, untreatedData, managedData = NULL) {
  
  activeData$type <- "individual"
  untreatedData$type <- "untreated nest"
  
  # for intermediate data (no radius yet)
  if (is.null(activeData$radius))
    activeData$radius <- NA
  if (is.null(untreatedData$radius))
    untreatedData$radius <- NA
  
  if (!is.null(managedData)) {
    managedData$type <- "managed nest"
    if (is.null(managedData$radius))
      managedData$radius <- NA
  }
  

  toReturn <- rbind(
    activeData[, c("type", "popup", "radius")],
    if (!is.null(managedData))
      managedData[, c("type", "popup", "radius")],
    untreatedData[, c("type", "popup", "radius")]
  )
  
  toReturn$filter <- as.factor(toReturn$type)
  toReturn$type <- as.factor(toReturn$type)
  
  toReturn
  
}

#' Combine data for observations on Individual and Nest data
#' 
#' Used on Management page for Vespa Velutina
#' 
#' @param pointsData sf data.frame, points observations for individuals
#' @param nestenData sf data.frame, points observations for nests
#' @param currentYear integer, current year for selecting nest data
#' @inheritParams mapHeat
#' @return sf data.frame, combining both data sources
#' 
#' @author mvarewyck
#' @importFrom dplyr select filter mutate group_by summarise rename case_when
#' @importFrom data.table year
#' @export
combineNestenData <- function(pointsData, nestenData, 
  currentYear = data.table::year(Sys.Date()),
  uiText = NULL) {
  
  # For R CMD check
  type <- eventDate <- popup <- institutionCode <- id <- observation_time <- NULL
  geometry <- nest_type <- result <- NULL
  
  points_redux <- pointsData %>% 
    dplyr::filter(year == currentYear) %>%
    # Only retain individual data, see https://github.com/inbo/alien-species-portal/issues/63#issuecomment-1918810526
    dplyr::filter(dplyr::case_when(eventDate >= as.Date("31/12/2028", format = "%d/%m/%Y") ~ type == "Individu", TRUE ~ TRUE)) %>%
    dplyr::select(type, eventDate, popup, institutionCode, year) %>%
    mutate(type = ifelse(type == "Individu", "individual", "nest"))
  
  # punten laag van gemelde nesten
  nesten <- nestenData %>% 
    dplyr::filter(year == currentYear) 
  
  if (nrow(nesten) == 0)
    return(NULL)
  
  nesten <- nesten %>% 
    mutate(type = "nest",
      popup = paste(
        translate(uiText, "nest")$title, ":", translate(uiText, nest_type)$title,
        "</br>", translate(uiText, "management")$title, ":", translate(uiText, result)$title,
        "</br>Vespawatch", translate(uiText, "row")$title, id), 
      institutionCode = "Vespawatch")
  
  nesten_redux <- nesten %>% 
    dplyr::filter(year == currentYear) %>% 
    dplyr::select(type, eventDate = observation_time, popup, institutionCode, year) 
  
  # Recombine points
  points_nesten <- rbind(points_redux, nesten_redux) %>% 
    mutate(eventDate = format(eventDate, "%Y-%m-%d")) %>% 
    group_by(geometry, type, eventDate) %>% 
    summarise(institutionCode = paste(unique(institutionCode), collapse = ","),
      popup = paste0("<b>", type, "</b></br>",
        paste(popup, ":", eventDate, collapse = ",<br>"))) %>%
    rename("filter" = "institutionCode") %>%
    mutate(filter = as.factor(filter),
      type = as.factor(type))
  
  
  points_nesten
  
} 


#' Create leaflet heatmap for the occurrence of Vespa Velutina (management data)
#' 
#' @param selected character vector, defines which layers should be shown on the map
#' @inheritParams mapCube
#' @param combinedData, data.frame as returned by \code{\link{combineNestenData}}
#' @param colors named character vector, defines colors and labels in the map
#' @param blur character, value of `type` for which to show blur
#' @return leaflet map
#' 
#' @author mvarewyck
#' @importFrom leaflet addTiles `%>%` leaflet addLegend addScaleBar addCircleMarkers 
#' @importFrom leaflet.extras addHeatmap
#' @export

mapHeat <- function(combinedData, baseMap = addBaseMap(), colors, blur = NULL, selected,
  legend = "topright", addGlobe = FALSE, uiText = NULL) {
  
  
  # Base map
  ah_map <- baseMap
  
  if (addGlobe)
    ah_map <- addTiles(ah_map)
  
  
  if (legend != "none")
    ah_map <- addLegend(
      map = ah_map,
      position = legend,
      colors = colors,
      labels = sapply(names(colors), function(x) translate(uiText, x)$title),
      opacity = 0.8,
      title = translate(uiText, "legend")$title,
      layerId = "legend"
    )
  
  
  # Filter data
  plotData <- combinedData[combinedData$filter %in% selected, ]
 
  if (is.null(plotData) || nrow(plotData) == 0)
    return(ah_map)
  
  
  pal_ah <- colorFactor(palette = colors, levels = names(colors))
  plotData$color <- pal_ah(plotData$type)
  
  # Add blur
  if (!is.null(blur))
    ah_map <- addHeatmap(ah_map,
      data = plotData[plotData$type == blur, ],
      blur = 25, 
      max = 1, 
      radius = 20
    ) 
  
  for (iFilter in levels(plotData$filter))
    ah_map <- addCircleMarkers(
      ah_map,
      data = plotData[plotData$filter == iFilter, ],
      color = ~color,
      opacity = 0.5,
      radius = 2,
      popup = ~popup,
      group = iFilter)
 
  
  ah_map
  
}



#' Shiny module for creating the plot \code{\link{mapCube}} - server side
#' 
#' @inheritParams welcomeSectionServer
#' @inheritParams mapHeat
#' @inheritParams mapCubeServer
#' @param filter reactive list with filters to be shown in the app;
#' names should match a plotFunction in \code{uiText}; 
#' values define the choices in \code{selectInput}
#' @param maxDate reactive date, last observation date in the dataset
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import leaflet
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot webshot
#' @importFrom sf st_drop_geometry
#' @export
mapHeatServer <- function(id, uiText, species, gewest, combinedData, filter, colors, 
  blur = NULL, maxDate, dashReport = NULL
) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      
      noData <- reactive(translate(uiText(), "noData")$title)
      tmpTranslation <- reactive(translate(uiText(), ns("mapHeat")))
      
      tmpFile <- tempfile(fileext = ".html")
      
      description <- reactive({
          
          decodeText(
            text = tmpTranslation()$description,
            params = list(
              maxDate = format(maxDate(), "%d/%m/%Y"),
              maxYear = format(Sys.Date(), "%Y")
            )
          )
          
        })
      
      output$descriptionMapHeat <- renderUI(HTML(description()))
      
      output$titleMapHeat <- renderUI(h3(HTML(tmpTranslation()$title)))
      
      # Hide output if no data
      observe({
          
          # Wait until output is created
          req(input$legend)
          
          shinyjs::toggle(id = "mapHeatUI", 
            condition = (!is.null(combinedData()) && nrow(combinedData()) > 0))
          
        })
      
      output$filters <- renderUI({
          
          req(nrow(combinedData()))
          
          if (!is.null(filter()))
            lapply(names(filter()), function(filterName) {
                
              if(is.factor(filter()[[filterName]])){
              
                choices <- filter()[[filterName]]
                names(choices) <- translate(uiText(), choices)$title
                
                column(4, 
                  selectInput(inputId = ns(filterName), 
                    label = translate(uiText(), filterName)$title,
                    choices = choices,
                    multiple = TRUE, selected = filter()[[filterName]])
                )
                }else if(is.numeric(filter()[[filterName]])){
                  
                  
                choices <- c(translate(uiText(), "none")$title, filter()[[filterName]])
          
                column(4, 
                       selectInput(inputId = ns(filterName), 
                                   label = translate(uiText(), filterName)$title,
                                   choices = choices,
                                   multiple = FALSE, selected =  min(as.numeric(filter()[[filterName]])))
                )
                }
              })
          
        })
      
      
      
      
      output$legend <- renderUI({
          
          legendChoices <- c("topright", "bottomright", "topleft", "bottomleft", "none")
          names(legendChoices) <- sapply(legendChoices, function(x) translate(uiText(), x)$title)
          
          selectInput(inputId = ns("legend"), 
            label = translate(uiText(), "legend")$title,
            choices = legendChoices)
          
        })
      
      # filter conbinedData if there are more that 1 filter
      # assume the filter name is the column name in data generated by combineActiveData
      
      combinedDataPostFilter <- reactive({
          #trigger re-evaluation
          # otherwise, only change in 1st filter will not take action
          input[[ names(filter())[1] ]]
          input[["legend"]]
          input[["global"]]


          tmpData <- combinedData()

          for (iFilter in names(filter())[-1]){

            if (!is.null(input[[iFilter]]) && input[[iFilter]] != "<none>"){
              index <- !is.na(tmpData[[iFilter]]) & (tmpData[[iFilter]] == input[[iFilter]])
              tmpData <- tmpData[index,]
            }
          }
          
          tmpData
          
        })
      
      # Send map to the UI
      output$spacePlot <- renderLeaflet({
          
          validate(need(nrow(combinedDataPostFilter()) > 0, noData()))
          
          myMap <- mapHeat(
            combinedData =  combinedDataPostFilter(),
            baseMap = addBaseMap(regions = gewest()),
            colors = colors(),
            selected = unique(combinedDataPostFilter()$filter),
            addGlobe = isolate(input$globe %% 2 == 1),
            blur = blur,
            uiText = uiText()
          )
          
          myMap
          
        })
      


      
      # Add/remove map layers
      observe({
       
        
         input[[ names(filter())[2] ]]
        
          proxy <- leafletProxy("spacePlot")
          
          if (!is.null(proxy)) {
            
            for (iLayer in filter()[[1]])
              if (iLayer %in% input[[names(filter())[1]]])
                proxy %>% showGroup(iLayer) else
                proxy %>% hideGroup(iLayer)
            
          }
          
        })
      
      
      # Add world map
      observe({
          
        input[[ names(filter())[2] ]]
        
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
          input[[ names(filter())[2] ]]
        
          req(input$legend)
          
          proxy <- leafletProxy("spacePlot")
          proxy %>% removeControl(layerId = "legend")
          
          if (input$legend != "none") {
            
            excludedLayers <- setdiff(input[[names(filter())[1]]], filter()[[1]])
            currentColors <- colors()[!names(colors()) %in% excludedLayers]
            names(currentColors) <- sapply(names(currentColors), function(x) translate(uiText(), x)$title)
            
            proxy %>% addLegend(
              position = input$legend,
              colors = currentColors,
              labels = names(currentColors),
              opacity = 0.8,
              title = translate(uiText(), "legend")$title,
              layerId = "legend"
            )                      
            
          }
          
        })
      
      
      # Create final map (for download)
      finalMap <- reactive({
          
          req(nrow(combinedDataPostFilter()) > 0)
          req(input[[names(filter())[1]]])
     
          newMap <- mapHeat(
            combinedData = combinedDataPostFilter(),
            baseMap = addBaseMap(regions = gewest()),
            colors = colors(),
            selected = input[[names(filter())[1]]],
            blur = blur, 
            legend = if (is.null(input$legend)) "topright" else input$legend,
            addGlobe = if (is.null(input$globe)) TRUE else input$globe %% 2 == 1,
            uiText = uiText()
          )
          
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
            label = translate(uiText(), "downloadMap")$title, 
            class = "downloadButton")
        })
      
      output$download <- downloadHandler(
        filename = function()
          nameFile(species = species(),
            content = id, fileExt = "png"),
        content = function(file) {
          
          # convert temp .html file into .png for download
          webshot::webshot(url = finalMap(), file = file,
            vwidth = 1200, vheight = 600, cliprect = "viewport")
          
        }
      )
      
#      output$downloadData <- downloadHandler(
#        filename = function()
#          nameFile(species = species(),
#            content = paste0(id, "_data"), fileExt = "csv"),
#        content = function(file) {
#          myData <- do.call(rbind, cubeShape())
#          myData$source <- attr(cubeShape(), "splitFactor")
#          myData$geometry <- NULL          
#          ## write data to exported file
#          write.table(x = myData, file = file, quote = FALSE, row.names = FALSE,
#            sep = ";", dec = ",")
#          
#        })
      
      ## Report Objects ##
      ## -------------- ##
      
      observe({
          
          req(dashReport)
          
          # Update when any of these change
          finalMap()
          maxDate()
          input
          
          # Return the static values
          dashReport[[ns("mapHeat")]] <- c(
            list(
              plot = isolate(finalMap()),
              title = isolate(tmpTranslation()$title),
              description = isolate(description()) 
            ),
            isolate(reactiveValuesToList(input))
          )
          
        })
      
      
      return(dashReport)
    
      
    })  
} 



#' Shiny module for creating the plot \code{\link{mapCube}} - UI side
#' @inheritParams welcomeSectionServer
#' @inheritParams mapCubeUI
#' 
#' @return UI object
#' 
#' @author mvarewyck
#' @import shiny
#' @importFrom leaflet leafletOutput
#' @export
mapHeatUI <- function(id, showLegend = TRUE, showGlobe = TRUE) {
  
  ns <- NS(id)
  
  # Occurrence Map
  
  tags$div(class = "container",
    
    uiOutput(ns("titleMapHeat")),
    uiOutput(ns("descriptionMapHeat")),
    
    tags$div(id = ns("mapHeatUI"),
      wellPanel(
        fixedRow(uiOutput(ns("filters")),
          if (showLegend)
            column(4, 
              uiOutput(ns("legend"))
            ),
          if (showGlobe)
            column(6, 
              actionLink(inputId = ns("globe"), label = "Show globe",
                icon = icon("globe"))
            )
        )
      ),
      withSpinner(leafletOutput(ns("spacePlot"), height = "600px")),
      
      tags$br(),
      
      tags$div(uiOutput(ns("downloadMapButton")), style = "display:inline-block;"),
#    downloadButton(ns("downloadData"), label = "Download data", class = "downloadButton"),
    ),
    
    tags$hr()
  
  )
  
}
