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
#' @importFrom leaflet addScaleBar addProviderTiles addLegend colorNumeric addRasterImage
#' @importFrom terra values
#' @export

mapRaster <- function(rasterInput, baseMap = addBaseMap(), colors = "Spectral", 
  legend = "topright", legendScale = "risk", addGlobe = FALSE) {
  
  
  # Base map
  rasterMap <- baseMap %>%
    addScaleBar(position = "bottomleft")
  
  if (addGlobe)
    rasterMap <- addProviderTiles(rasterMap, providers$CartoDB.Positron)
  
  
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
        paste("0 -", translate(paste0(legendScale, "Low"))$title), 
        rep("", 4), 
        paste("1 -", translate(paste0(legendScale, "High"))$title)),
      title = translate("legend")$title,
      layerId = "legend"
    )
  
  rasterMap <- addRasterImage(rasterMap,
      rasterInput, colors = rasterPal,
      opacity = 0.8)
  
  
  rasterMap
  
}





#' Build the expected risk map file name (WISDM naming convention, issue #207)
#'
#' @param taxonKey character/numeric, GBIF taxon key
#' @param period character, one of "current", "2041-2070", "2071-2100"
#' @param scenario character, one of "baseline", "ssp126", "ssp370", "ssp585";
#' ignored when \code{period == "current"} (that period only ever combines
#' with the baseline scenario, and has no scenario token in the file name)
#' @param suffix character, "" for a prediction file, "_diff" for a difference
#' file, "_SD" for a confidence file
#' @param source character, "Combined" (default) or "Climate" (fallback for
#' species without a Combined model)
#' @return character, file name (without path)
#'
#' @export
riskMapFileName <- function(taxonKey, period, scenario, suffix = "", source = "Combined") {

  periodScenario <- if (period == "current")
    "current" else
    paste(period, scenario, sep = "_")

  paste0(taxonKey, "_", source, "_", periodScenario, "_ensemble", suffix, ".tif")

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
#' @importFrom webshot2 webshot
#' @importFrom terra values rast
#' @importFrom httr http_status GET content
#' @importFrom utils download.file
#' @export
mapRasterServer <- function(id, species, gewest, taxonKey) {
  
  colors <- "Spectral"
    
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns

      periodIds <- c(current = "Current", "2041-2070" = "2041-2070", "2071-2100" = "2071-2100")
      scenarioIds <- c(baseline = "Baseline", ssp126 = "SSP1-2.6", ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")

      noData <- reactive(translate("noData")$title)
      tmpTranslation <- reactive(translate(ns("mapRaster")))
      
      output$titleMapRaster <- renderUI(h3(HTML(tmpTranslation()$title)))
      output$descriptionMapRaster <- renderUI(HTML(tmpTranslation()$description))
      
      
      output$filters <- renderUI({

          # Filter choices
          periodChoices <- c("current", "2041-2070", "2071-2100")
          names(periodChoices) <- translate(periodIds[periodChoices])$title

          scenarioChoices <- c("baseline", "ssp126", "ssp370", "ssp585")
          names(scenarioChoices) <- translate(scenarioIds[scenarioChoices])$title

          modelTypes <- c("riskMap", "confMap", "diffMap")
          names(modelTypes) <- translate(modelTypes)$title


          filters <- list(
            period = periodChoices,
            scenario = scenarioChoices,
            modelType = modelTypes
          )

          lapply(names(filters), function(iName) {

              column(3,
                selectInput(inputId = ns(iName),
                  label = translate(iName)$title,
                  choices = filters[[iName]],
                  multiple = FALSE))

            })

        })


      # Period "current" only ever combines with scenario "baseline";
      # the other periods combine with the other (non-baseline) scenarios
      observeEvent(input$period, {

          req(input$period)

          scenarioChoices <- if (input$period == "current")
            "baseline" else
            c("ssp126", "ssp370", "ssp585")
          names(scenarioChoices) <- translate(scenarioIds[scenarioChoices])$title

          updateSelectInput(session, inputId = "scenario",
            choices = scenarioChoices, selected = scenarioChoices[1])

          # Period "current" (-> scenario "baseline") never has a Difference
          # map (nothing to diff against itself)
          modelTypes <- if (input$period == "current")
            c("riskMap", "confMap") else
            c("riskMap", "confMap", "diffMap")
          names(modelTypes) <- translate(modelTypes)$title

          selectedType <- if (isolate(input$modelType) %in% modelTypes)
            isolate(input$modelType) else
            modelTypes[1]

          updateSelectInput(session, inputId = "modelType",
            choices = modelTypes, selected = selectedType)

        })


      # Mirror image of the above: Difference maps are never valid for
      # period "current", so selecting Difference removes "current" from
      # the period choices too
      observeEvent(input$modelType, {

          req(input$modelType)

          periodChoices <- c("current", "2041-2070", "2071-2100")
          if (input$modelType == "diffMap")
            periodChoices <- setdiff(periodChoices, "current")
          names(periodChoices) <- translate(periodIds[periodChoices])$title

          selectedPeriod <- if (isolate(input$period) %in% periodChoices)
            isolate(input$period) else
            periodChoices[1]

          updateSelectInput(session, inputId = "period",
            choices = periodChoices, selected = selectedPeriod)

        })


      # Type dropdown determines both the file suffix and the storage subfolder
      typeFolder <- reactive(switch(input$modelType,
          riskMap = "Predictions", confMap = "Confidence", diffMap = "Difference"))
      typeSuffix <- reactive(switch(input$modelType,
          riskMap = "", confMap = "_SD", diffMap = "_diff"))

      rasterFile <- reactive({

          req(input$period)
          req(input$scenario)
          req(!is.null(input$modelType))

          # List once, so the Combined -> Climate fallback (species with no
          # Combined model) is a lookup in an already-fetched listing, not an
          # extra live request
          listing <- httr::GET(paste0(
              "https://api.github.com/repos/inbo/wisdm-maps-iasportal/contents/data/",
              taxonKey(), "/", typeFolder(), "?ref=uat"))

          if (httr::http_status(listing)$category == "Client error")
            return(NULL)

          availableFiles <- sapply(httr::content(listing), function(x) x$name)

          wantedFile <- riskMapFileName(taxonKey(), input$period, input$scenario,
            suffix = typeSuffix(), source = "Combined")

          if (!wantedFile %in% availableFiles)
            wantedFile <- riskMapFileName(taxonKey(), input$period, input$scenario,
              suffix = typeSuffix(), source = "Climate")

          if (!wantedFile %in% availableFiles)
            return(NULL)

          file.path("https://raw.githubusercontent.com/inbo/wisdm-maps-iasportal/uat/data",
            taxonKey(), typeFolder(), wantedFile)

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
          names(legendChoices) <- sapply(legendChoices, function(x) translate(x)$title)
          
          selectInput(inputId = ns("legend"), 
            label = translate("legend")$title,
            choices = legendChoices)
          
        })
      
      
      # Send map to the UI
      output$spacePlot <- renderLeaflet({
          
          mapRaster(
            rasterInput = rasterInput(),
            baseMap = addBaseMap(regions = gewest()),
            colors = colors,
            legendScale = isolate(gsub("Map", "", input$modelType)),
            addGlobe = isolate(input$globe %% 2 == 1)
          ) %>%
          leaflet.extras::addFullscreenControl() %>% 
          leaflet.extras2::addEasyprint(    # use leaflets personal functionality to download maps
            options = leaflet.extras2::easyprintOptions(
              exportOnly = TRUE,
              hideControlContainer = FALSE,  # Keep controls visible
              hideClasses = c("leaflet-control-zoom", "leaflet-control-fullscreen", "leaflet-control-easyPrint")
            )
          )
          
        })
      
      
      # Add world map
      observe({
          
          proxy <- leafletProxy("spacePlot")
          
          if (!is.null(input$globe) & !is.null(proxy)){
            
            if (input$globe %% 2 == 1){
              
              updateActionLink(session, inputId = "globe", 
                label = translate("hideGlobe")$title)
              
              proxy %>% addProviderTiles(providers$CartoDB.Positron,
                options = providerTileOptions(zIndex = -10))
              
            } else {
              
              updateActionLink(session, inputId = "globe", 
                label = translate("showGlobe")$title)
              
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
                paste("0 -", translate(paste0(legendScale, "Low"))$title), 
                rep("", 4), 
                paste("1 -", translate(paste0(legendScale, "High"))$title)),
              title = translate("legend")$title,
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
          req(newMap)
          htmlwidgets::saveWidget(newMap, file = tmpFile, selfcontained = FALSE)
          
          # output is path to temp .html file containing map
          tmpFile
          
        }) 
      
      
      # Download the map
      output$downloadMapButton <- renderUI({
#          downloadButton(ns("download"), 
#            label = translate(uiText(), "downloadMap")$title, 
#            class = "downloadButton")
          
          actionButton(ns("download"), 
            label = translate("downloadMap")$title, 
            icon = icon("download"),
            class = "btn-default shiny-download-link downloadButton", type = "button")
        })
      
      observeEvent(input$download, {
          
          leafletProxy("spacePlot") %>% leaflet.extras2::easyprintMap(
            sizeModes = "CurrentSize",
            filename = nameFile(species = species(),
              content = id, fileExt = "png")
          )
          
        })
    
#      output$download <- downloadHandler(
#        filename = function()
#          nameFile(species = species(),
#            content = id, fileExt = "png"),
#        content = function(file) {
#          
#          # convert temp .html file into .png for download
#          webshot2::webshot(url = finalMap(), file = file,
#            vwidth = 1200, vheight = 600, cliprect = "viewport")
#          
#        }
#      )
      
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
mapRasterUI <- function(id) {
  
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
