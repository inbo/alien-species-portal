
#' Create data.frame with occupancy for t0 and t1 data
#' @param dataDir path to folder where to find the raw t0 and t1 files
#' @param packageDir path to folder where to save the created data.frame
#' @return TRUE if creation succeeded
#' 
#' @author mvarewyck
#' @importFrom rgdal readOGR
#' @importFrom sp spTransform CRS
#' @importFrom sf st_read
#' @importFrom reshape2 dcast
createOccupancyData <- function(dataDir = "~/git/alien-species-portal/data",
  packageDir = system.file("extdata", package = "alienSpecies")) {
  
  
  ## DATA T0 ##
  ## ------- ##
  
  # geojson: https://zenodo.org/record/3835756#.Yoc5oLxBw5m
  data_t0 <- lapply(seq(2016, 2020, by = 2), function(iYear) {
      dataYear <- rgdal::readOGR(dsn = file.path(dataDir, paste0("ias_belgium_t0_", iYear, ".geojson")), verbose = TRUE)
      dataYear <- sp::spTransform(dataYear, sp::CRS("+proj=longlat +datum=WGS84"))
      # inconsistent colnames over the years
      colnames(dataYear@data) <- c("cellcode", "reference", "data_partn", "accepted", "species", "notes") 
      dataYear
    })
  # sp::plot(data_t0[[1]])
  
  data_t0_all <- do.call(rbind, data_t0)@data
  data_t0_all$source <- "t0"
  # table(data_t0_all)
  
  # Keep accepted Y or New
  data_t0_all <- data_t0_all[data_t0_all$accepted %in% c("Y", "New"), ]
  
  
  ## DATA T1 ##
  ## ------- ##
  
  # shp: https://zenodo.org/record/3060173
  data_t1 <- sf::st_read(file.path(dataDir, "T1_Belgium_Union_List_Species.shp"))
  # sp::plot(data_t1)
  
  # Keep first two words only for matching with t0 data
  data_t1$species <- sapply(strsplit(data_t1$species, split = " |\\,"), function(x) paste(x[1:2], collapse = " "))
  
  data_t1$source <- "t1"
  data_t1 <- as.data.frame(data_t1)  # drop spatial info
  data_t1$cellcode <- data_t1$CellCode
  
  
  ## COMBINE DATA ##
  ## ------------ ##
  
  
  allData <- rbind(data_t0_all[, c("species", "source", "cellcode")], data_t1[, c("species", "source", "cellcode")])
  allData <- allData[!duplicated(allData), ]
  allData$cellcode <- NULL
  occupancy <- reshape2::dcast(data = as.data.frame(table(allData)), species ~ source, value.var = "Freq")
  occupancy$total <- occupancy$t0 + occupancy$t1
  
  occupancy <- occupancy[order(occupancy$total), ]
  occupancy$species <- factor(occupancy$species, levels = unique(occupancy$species)) # sort by freq in barchart
  
  save(occupancy, file = file.path(packageDir, "occupancy.RData"))
  
  return(TRUE)
  
}


#' Create occupancy bar chart
#' @param df data.frame as created by \code{\link{createOccupancyData}}
#' @param nSquares integer, total number of squares for calculating percentages
#' @return list with plotly object and data.frame
#' 
#' @author mvarewyck
#' @import plotly
#' @export
countOccupancy <- function(df, nSquares = 370) {
  
  p <- plot_ly(data = df, x = ~t0/nSquares*100, y = ~species, name = "baseline",
      type = "bar", orientation = "h") %>%
    add_trace(x = ~t1/nSquares*100, name = "rapportage") %>%
    layout(xaxis = list(title = 'Percentage bezette hokken (10km2) in Vlaanderen'),
      yaxis = list(title = ""), barmode = 'group')
  
  
  return(list(plot = p, data = df))
  
}


#' Shiny module for creating the plot \code{\link{countOccupancy}} - server side
#' @inheritParams welcomeSectionServer
#' @param data reactive object, data for \code{\link{countOccupancy}}
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countOccupancyServer <- function(id, uiText, data) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$titleOccupancy <- renderUI({
          
          h3(HTML(uiText[uiText$plotFunction == "countOccupancy", ]$title))
          
        })
      
      plotModuleServer(id = "occupancy",
        plotFunction = "countOccupancy", 
        data = data
      )
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{countOccupancy}} - UI side
#' @template moduleUI
#' 
#' @author mvarewyck
#' @export
countOccupancyUI <- function(id) {
  
  ns <- NS(id)
  
  
  tagList(
    
    actionLink(inputId = ns("linkOccupancy"), 
      label = uiOutput(ns("titleOccupancy"))),
    conditionalPanel("input.linkOccupancy % 2 == 1", ns = ns,
      
      helpText("NOTE: Plot independent of the selected filters above"),
      
      plotModuleUI(id = ns("occupancy"), height = "800px"),
      optionsModuleUI(id = ns("plotTrias"), doWellPanel = FALSE),
      tags$hr()
    
    )
  )
  
}