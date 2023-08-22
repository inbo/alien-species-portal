
#' Create leaflet heatmap for the occurrence of Vespa Velutina (management data)
#' 
#' @param activeData sf, points data with actieve haarden
#' @param managedData sf, points data with beheerde nesten
#' @param untreatedData sf, points data with onbehandelde nesten
#' @param selected character vector, defines which layers should be shown on the map
#' @inheritParams mapCube
#' @return leaflet map
#' 
#' @author mvarewyck
#' @importFrom leaflet addTiles `%>%` leaflet addLegend addScaleBar addCircleMarkers 
#' @importFrom leaflet.extras addHeatmap
#' @export
mapHeat <- function(activeData, managedData, untreatedData,
  selected = c("managed nest", "untreated nest"), legend = "bottomright", addGlobe = FALSE
) {
  
  
  groups <- c("blue", "black", "red")
  names(groups) <- c("individual", "managed nest", "untreated nest")
  
  
  if (!"managed nest" %in% selected)
    groups <- groups[!names(groups) %in% "managed nest"]
  
  if (!"untreated nest" %in% selected)
    groups <- groups[!names(groups) %in% "untreated nest"]
  
  pal_ah <- colorFactor(palette = groups, levels = names(groups))
  
  ah_map <- leaflet() %>%
    
    addHeatmap(
      data = activeData,
      blur = 25, 
      max = 1, 
      radius = 20
    ) %>%
    addScaleBar(position = "bottomleft") %>%
    addCircleMarkers(data = activeData,
      color = groups[names(groups) == "individual"],
      opacity = 0.5,
      radius = 2,
      popup = ~popup,
      group = "active") 
  
  
  if (addGlobe)
    ah_map <- addTiles(ah_map)
  
  
  if (legend != "none")
    ah_map <- addLegend(ah_map,
      pal = pal_ah,
      values = names(groups),
      opacity = 0.7,
      position = legend,
      layerId = "legend"
    )
  
  
  if ("managed nest" %in% selected) {
    
    ah_map <- ah_map %>% 
      addCircleMarkers(data = managedData,
        color = groups[names(groups) == "managed nest"],
        opacity = 0.5,
        radius = 1,
        popup = ~popup,
        group = "managed nest")
    
  }
  
  if ("untreated nest" %in% selected) {
    
    ah_map <- ah_map %>%
      addCircleMarkers(data = untreatedData,
        color = groups[names(groups) == "untreated nest"],
        opacity = 0.5,
        radius = 2,
        popup = ~popup,
        group = "untreated nest")
    
  }
  
  ah_map
  
}



#' Shiny module for creating the plot \code{\link{mapCube}} - server side
#' 
#' @inheritParams welcomeSectionServer
#' @inheritParams mapHeat
#' @param species reactive character, readable name of the selected species
#' @param filter reactive list with filters to be shown in the app;
#' names should match a plotFunction in \code{uiText}; 
#' values define the choices in \code{selectInput}
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import leaflet
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot webshot
#' @importFrom sf st_drop_geometry
#' @export
mapHeatServer <- function(id, uiText, species, activeData, managedData, untreatedData, 
  filter
) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      rv <- reactiveValues(nest = c("managed nest", "untreated nest"))
      
      groups <- c("blue", "black", "red")
      names(groups) <- c("individual", "managed nest", "untreated nest")
      
      noData <- reactive(translate(uiText(), "noData")$title)
      tmpTranslation <- reactive(translate(uiText(), ns("mapHeat")))
      
      output$descriptionMapHeat <- renderUI({
          
          HTML(gsub("\\{\\{maxDate\\}\\}", format(max(activeData()$eventDate), "%d/%m/%Y"), 
              tmpTranslation()$description))
          
        })
      
      output$titleMapHeat <- renderUI(h3(HTML(tmpTranslation()$title)))
      
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
      
      
      output$legend <- renderUI({
          
          legendChoices <- c("topright", "bottomright", "topleft", "bottomleft", "none")
          names(legendChoices) <- sapply(legendChoices, function(x) translate(uiText(), x)$title)
          
          selectInput(inputId = ns("legend"), 
            label = translate(uiText(), "legend")$title,
            choices = legendChoices)
          
        })
      
      
      
      # Send map to the UI
      output$spacePlot <- renderLeaflet({
          
          mapHeat(activeData = activeData(), 
            managedData = managedData(),
            untreatedData = untreatedData(),
            selected = isolate(input$nest)
          )
          
        })
      
      
      # Add/remove nest layers
      observe({
          
          proxy <- leafletProxy("spacePlot")
          
          if (!is.null(proxy)) {
            
            if (is.null(input$nest)) {
              
              proxy %>% hideGroup(group = c("managed nest", "untreated nest")) 
              
            } else if ("managed nest" %in% input$nest & !"managed nest" %in% rv$nest) {
              
              proxy %>%
                addCircleMarkers(data = managedData(),
                  color = groups[names(groups) == "managed nest"],
                  opacity = 0.5,
                  radius = 1,
                  popup = ~popup,
                  group = "managed nest")
              
            } else if ("untreated nest" %in% input$nest & !"untreated nest" %in% rv$nest) {
              
              proxy %>%
                addCircleMarkers(data = untreatedData(),
                  color = groups[names(groups) == "untreated nest"],
                  opacity = 0.5,
                  radius = 2,
                  popup = ~popup,
                  group = "untreated nest")
            }
            
            rv$nest <- input$nest
            
          }
          
        })
      
      
      # Add world map
      observe({
          
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
          
          req(input$legend)
          
          proxy <- leafletProxy("spacePlot")
          proxy %>% removeControl(layerId = "legend")
          
          if (input$legend != "none") {
            
            groups <- c("blue", "black", "red")
            names(groups) <- c("individual", "managed nest", "untreated nest")
            
            
            if (!"managed nest" %in% input$nest)
              groups <- groups[!names(groups) %in% "managed nest"]
            
            if (!"untreated nest" %in% input$nest)
              groups <- groups[!names(groups) %in% "untreated nest"]
            
            names(groups) <- sapply(names(groups), function(x) translate(uiText(), x)$title)
            palette <- colorFactor(palette = groups, levels = names(groups))
            
            proxy %>% addLegend(
              position = input$legend,
              pal = palette, 
              values = names(groups),
              opacity = 0.8,
              title = translate(uiText(), "legend")$title,
              layerId = "legend"
            )                      
            
          }
          
        })
      
      
      # Create final map (for download)
      finalMap <- reactive({
          
          newMap <- mapHeat(
            activeData = activeData(),
            managedData = managedData(),
            untreatedData = untreatedData(),
            selected = input$nest,
            legend = input$legend,
            addGlobe = input$globe %% 2 == 1
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
      
      
    })  
} 



#' Shiny module for creating the plot \code{\link{mapCube}} - UI side
#' @inheritParams welcomeSectionServer
#' @inheritParams mapCubeUI
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
    withSpinner(leafletOutput(ns("spacePlot"), height = "600px")),
    
    tags$br(),
    
    tags$div(uiOutput(ns("downloadMapButton")), style = "display:inline-block;"),
#    downloadButton(ns("downloadData"), label = "Download data", class = "downloadButton"),
    
    tags$hr()
  
  )
  
}
