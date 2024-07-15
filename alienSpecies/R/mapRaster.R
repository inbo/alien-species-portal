# Functions for raster maps
#
# Used for
# - climate risk maps
# 
# Author: mvarewyck
###############################################################################


#' Create leaflet raster map for the climate risk maps
#' 
#' @param rasterInput SpatRaster object, as returned by \code{terra::rast}
#' @param legendScale character, scale to be mentioned in the legend
#' @inheritParams mapHeat
#' 
#' @return leaflet map
#' 
#' @author mvarewyck
#' @importFrom leaflet addScaleBar addTiles addLegend colorNumeric addRasterImage
#' @importFrom terra values
#' @export

mapRaster <- function(rasterInput, baseMap = addBaseMap(), colors = "Spectral", 
  legend = "topright", legendScale = "risk", addGlobe = FALSE, uiText = NULL) {
  
  
  # Base map
  rasterMap <- baseMap %>%
    addScaleBar(position = "bottomleft")
  
  if (addGlobe)
    rasterMap <- addTiles(rasterMap)
  
  
  if (is.null(rasterInput))
    return(rasterMap)
  
  rasterPal <- colorNumeric(palette = colors, domain = c(0, 1), 
    na.color = "transparent", reverse = TRUE)
  
  
  if (legend != "none")
    rasterMap <- addLegend(
      map = rasterMap,
      position = legend,
      values = values(rasterInput),
      opacity = 0.8,
      colors = rasterPal(seq(0, 1, by = 0.2)),
      labels = c(
        paste("0 -", translate(uiText, paste0(legendScale, "Low"))$title), 
        rep("", 4), 
        paste("1 -", translate(uiText, paste0(legendScale, "High"))$title)),
      title = translate(uiText, "legend")$title,
      layerId = "legend"
    )
  
  rasterMap <- addRasterImage(rasterMap,
      rasterInput, colors = rasterPal,
      opacity = 0.8)
  
  
  rasterMap
  
}





#' Shiny module for creating the plot \code{\link{mapCube}} - server side
#' 
#' @inheritParams welcomeSectionServer
#' @inheritParams mapHeat
#' @inheritParams mapCubeServer
#' @param taxonKey reactive numeric, taxonkey of the species to select the correct tiff file
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import leaflet
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot webshot
#' @importFrom terra values rast
#' @importFrom httr http_status GET
#' @importFrom utils download.file
#' @export
mapRasterServer <- function(id, uiText, species, gewest, taxonKey) {
  
  colors <- "Spectral"
    
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      
      noData <- reactive(translate(uiText(), "noData")$title)
      tmpTranslation <- reactive(translate(uiText(), ns("mapRaster")))
      
      output$titleMapRaster <- renderUI(h3(HTML(tmpTranslation()$title)))
      output$descriptionMapRaster <- renderUI(HTML(tmpTranslation()$description))
      
      
      output$filters <- renderUI({
          
          # Filter choices
          modelScenarios <- c("hist", "rcp26", "rcp45", "rcp85")
          names(modelScenarios) <- translate(uiText(), modelScenarios)$title
          
          modelTypes <- c("riskMap", "confMap", "diffMap")
          names(modelTypes) <- translate(uiText(), modelTypes)$title
          
          
          filters <- list(
            modelScenario = modelScenarios,
            modelType = modelTypes
          )
          
          lapply(names(filters), function(iName) {
              
              column(4, 
                selectInput(inputId = ns(iName), 
                  label = translate(uiText(), iName)$title,
                  choices = filters[[iName]],
                  multiple = FALSE))
              
            })
          
        })
      
      
      rasterFile <- reactive({
          
          req(input$modelScenario)
          req(!is.null(input$modelType))
          
          tiffPath <- "https://raw.githubusercontent.com/trias-project/risk-maps/main/public/geotiffs"
          tiffFile <- paste0(
            paste("be", taxonKey(), input$modelScenario, sep = "_"),
            if (input$modelType != "riskMap") 
              paste0("_", gsub("Map", "", input$modelType)), 
            ".4326.tif")
          
          toReturn <- file.path(tiffPath, tiffFile)
          
          if (httr::http_status(httr::GET(toReturn))$category == "Client error")
            NULL else
            toReturn
          
        })
      
      output$warningFile <- renderUI({
          
          if (is.null(rasterFile()))
            tags$div(class = "alert alert-warning", noData())
          
        })
      
      
      rasterInput <- reactive({
          
          if (is.null(rasterFile()))
            return(NULL)   
          
          tempFile <- file.path(tempdir(), basename(rasterFile()))
          download.file(rasterFile(), destfile = tempFile, method = "curl")
          
          terra::rast(x = tempFile)
                    
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
          
          mapRaster(
            rasterInput = rasterInput(),
            baseMap = addBaseMap(regions = gewest()),
            colors = colors,
            legendScale = isolate(gsub("Map", "", input$modelType)),
            addGlobe = isolate(input$globe %% 2 == 1),
            uiText = uiText()
          )
          
        })
      
      
      # Add world map
      observe({
          
          proxy <- leafletProxy("spacePlot")
          
          if (!is.null(input$globe) & !is.null(proxy)){
            
            if (input$globe %% 2 == 1){
              
              updateActionLink(session, inputId = "globe", 
                label = translate(uiText(), "hideGlobe")$title)
              
              proxy %>% addTiles(options = tileOptions(zIndex = -10))
              
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
            
            req(rasterInput())
            
            rasterPal <- colorNumeric(palette = colors, domain = c(0, 1), 
              na.color = "transparent", reverse = TRUE)
            
            legendScale <- gsub("Map", "", input$modelType)
            
            proxy %>% addLegend(
              position = input$legend,
              values = terra::values(rasterInput()),
              opacity = 0.8,
              colors = rasterPal(seq(0, 1, by = 0.2)),
              labels = c(
                paste("0 -", translate(uiText(), paste0(legendScale, "Low"))$title), 
                rep("", 4), 
                paste("1 -", translate(uiText(), paste0(legendScale, "High"))$title)),
              title = translate(uiText(), "legend")$title,
              layerId = "legend"
            )                      
            
          }
          
        })
      
      
      # Create final map (for download)
      finalMap <- reactive({
          
          newMap <- mapRaster(
            rasterInput = rasterInput(),
            baseMap = addBaseMap(regions = gewest()),
            colors = colors,
            legend = input$legend,
            legendScale = gsub("Map", "", input$modelType),
            addGlobe = input$globe %% 2 == 1,
            uiText = uiText()
          )
          
          # save the zoom level and centering to the map object
          newMap <- newMap %>% setView(
            lng = input$spacePlot_center$lng,
            lat = input$spacePlot_center$lat,
            zoom = input$spacePlot_zoom
          )
          
          tmpFile <- tempfile(fileext = ".html")
          
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
mapRasterUI <- function(id, uiText) {
  
  ns <- NS(id)
  
  
  # Raster Map
  tags$div(class = "container", style = "margin-top: 10px;",
    
    uiOutput(ns("titleMapRaster")),
    uiOutput(ns("descriptionMapRaster")),
    
    wellPanel(
      fixedRow(
        uiOutput(ns("filters")),
          column(4, 
            uiOutput(ns("legend"))
          ),
          column(6, 
            actionLink(inputId = ns("globe"), label = "Show globe",
              icon = icon("globe"))
          )
      )
    ),
    uiOutput(ns("warningFile")),
    withSpinner(leafletOutput(ns("spacePlot"), height = "600px")),
    
    tags$br(),
    
    tags$div(uiOutput(ns("downloadMapButton")), style = "display:inline-block;"),
    
    tags$hr()
  
  )
  
}
