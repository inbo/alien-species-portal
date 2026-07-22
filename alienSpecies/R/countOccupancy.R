


#' Create occupancy bar chart
#' @param df data.frame as created by \code{\link{loadOccupancyData}}
#' @param nSquares integer, total number of squares for calculating percentages
#' @return list with plotly object and data.frame
#' 
#' @author mvarewyck
#' @import plotly
#' @importFrom INBOtheme inbo_palette
#' @importFrom data.table melt
#' @export
countOccupancy <- function(df, nSquares = 370) {
  
  periods <- list()
  periodCols <- colnames(df)[endsWith(colnames(df), "_period")]
  for (col in periodCols) {
    cycle <- gsub("_period", "", col)
    if (cycle == "t0") {
      periods[[cycle]] <- df %>% select(c("species", col))
    } else {
      periods[[cycle]] <- na.omit(unique(df %>% select(c(col))))[[1]]
    }
  }
  
  
  plotData <- data.table::melt(df %>% select(-c("total", periodCols)), id.vars = "species")
  plotData[, "species"] <- droplevels(plotData[, "species"])
  
  old_levels <- sort( levels(plotData$variable) )
  new_labels <- sapply(old_levels, function(x) {
      if (x == "t0") {
        translate("baseline")$title
      } else {
        paste0(translate("reporting")$title, " (Cycle ", substring(x, 2), ": ", periods[[x]], ")")
      }
    })
    
  plotData$variable <- factor(plotData$variable,
    levels = old_levels,
    labels = new_labels)
  
  plotData <- as.data.frame(merge(plotData, as.data.frame(periods$t0)))
  plotData$text <- paste0(
    "(", round(plotData$value/nSquares*100, 5), ", ", plotData$species, ")<br>",
    plotData$variable, ifelse(plotData$variable == translate("baseline")$title, paste0(" (", plotData$t0_period, ")"), "")
  )
  
  colors <- inbo_palette(length(levels(plotData$variable)) + 1)[-1]
  names(colors) <- levels(plotData$variable)
  
  p <- plot_ly(data = plotData, x = ~value/nSquares*100, y = ~species, text = ~text, textposition = "none",
      color = ~variable, colors = colors, type = "bar", orientation = "h", hoverinfo = "text") %>%
    layout(xaxis = list(title = translate('percentCages')$title),
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
countOccupancyServer <- function(id, data) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      tmpTranslation <- reactive(translate("countOccupancy"))
      
      output$descriptionOccupancy <- renderUI(HTML(tmpTranslation()$description))
      
      output$titleOccupancy <- renderUI(h3(HTML(tmpTranslation()$title)))
          
      
      plotModuleServer(id = "occupancy",
        plotFunction = "countOccupancy", 
        data = data
      )
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{countOccupancy}} - UI side
#' @inheritParams plotModuleUI
#' 
#' @author mvarewyck
#' @export
countOccupancyUI <- function(id) {
  
  ns <- NS(id)
  
  
  tags$div(class = "container",
    
    actionLink(inputId = ns("linkOccupancy"), 
      label = uiOutput(ns("titleOccupancy"))),
    conditionalPanel("input.linkOccupancy % 2 == 1", ns = ns,
      
      uiOutput(ns("descriptionOccupancy")),
      
      plotModuleUI(id = ns("occupancy"), height = "800px"),
      optionsModuleUI(id = ns("occupancy"), doWellPanel = FALSE),
      tags$hr()
    
    )
  )
  
}