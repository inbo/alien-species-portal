#' Plot number of new introductions per year
#' 
#' @param data data.frame with raw data for plotting
#' @param cumulative boolean, whether to display cumulative instead of absolute counts 
#' @param region NULL currently not implemented
#' @importFrom trias indicator_introduction_year indicator_total_year
#' @return list with plot and data
#' data that was used for the plot (i.e. without missing values for the )
#' 
#' @export
countIntroductionYear <- function(data, cumulative = FALSE, region = NULL){
  
  ## apply region filter
#  filteredData <- data[data$locality == region,]
#  filteredData = data
  
#  ## filter out missing valuse for first_observed
#  toExclude <- is.na(filteredData$first_observed)
#  plotData <- filteredData[!toExclude,]
  
  ## generate plot
  plotArgs <- list(
    df = data, 
    start_year_plot = min(data$first_observed, na.rm = TRUE) - 1,
    x_lab = "Jaar",
    y_lab = "Aantal ge\u00EFntroduceerde uitheemse soorten"
  )
  
  myPlot <- do.call(if (cumulative) "indicator_total_year" else "indicator_introduction_year",
    plotArgs)
    
  
  ## convert to plotly object
  p <- ggplotly(myPlot)
  
  ## add info on proportion of used data
#  attr(p, "proportionInfo") <- paste0("Info beschikbaar en weergegeven voor ", round((sum(!toExclude)/nrow(filteredData))*100, 1),
#                            " % van de totale gegevens (", sum(!toExclude), "/", nrow(filteredData), ")")
  
  return(list(plot = p, data = data))
  
  
}





#' Shiny module for creating the plot \code{\link{countIntroductionYear}} - server side
#' @inheritParams welcomeSectionServer
#' @inheritParams countIntroductionYear
#' @param data reactive object, data for \code{\link{countIntroductionYear}}
#' @param region reactive object, selected regions
#' @param time reactive object, selected years 
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countIntroductionYearServer <- function(id, uiText, data, cumulative = FALSE, 
  region, time) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns

      subText <- reactive({
          uiText[uiText$plotFunction == 
              paste0("countIntroductionYear-", if (cumulative) "cum" else "count"), ]
        })

      output$titleIntroductionYear <- renderUI({
          h3(HTML(paste(subText()$title, 
                vectorToTitleString(region()),
                yearToTitleString(req(time()))
              )))
          
        })
      
      plotModuleServer(id = "introductionYear",
        plotFunction = "countIntroductionYear", 
        data = data,
        cumulative = cumulative
      )
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{countIntroductionYear}} - UI side
#' @template moduleUI
#' 
#' @author mvarewyck
#' @export
countIntroductionYearUI <- function(id) {
  
  ns <- NS(id)
  
  
  tagList(
    
    actionLink(inputId = ns("linkIntroductionYear"), 
      label = uiOutput(ns("titleIntroductionYear"))),
    conditionalPanel("input.linkIntroductionYear % 2 == 1", ns = ns,
      
      plotModuleUI(id = ns("introductionYear")),
      tags$hr()
    
    )
  )
  
}