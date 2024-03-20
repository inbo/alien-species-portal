

#' Create plot data for \code{\link{trendYearRegion}}
#' @inheritParams createSummaryRegions
#' @param period numeric vector of length 2, time range selected for plot
#' @param typeNesten character vector, type of nests to filter data
#' @return data.frame with minimal data for the plot
#' 
#' @author mvarewyck
#' @importFrom dplyr rename group_by summarise n ungroup
#' @importFrom sf st_drop_geometry
#' @export
createSummaryNesten <- function(data, 
  regionLevel = c("communes", "provinces", "gewest"),
  period = NULL,
  typeNesten = NULL) {
  
  # For R CMD check
  region <- nest_type <- NULL
  
  regionVariable <- switch(regionLevel,
    communes = "NAAM",
    provinces = "provincie",
    gewest = "GEWEST"
  )
  
  if (!regionVariable %in% colnames(data))
    return(NULL) else
    colnames(data)[colnames(data) == regionVariable] <- "region"
  
  if (is.null(period))
    period <- range(data$year, na.rm = TRUE)
  
  if (is.null(typeNesten))
    typeNesten <- unique(data$nest_type)
    
  
  plotData <- data %>% 
    st_drop_geometry() %>% 
    filter(year %in% period[1]:period[2], nest_type %in% typeNesten) %>%
    group_by(region, year) %>% 
    summarise(outcome = n()) %>% 
    ungroup() 
  
  attr(plotData, "unit") <- "absolute"
  
  
  return(plotData)  
  
}



#' Shiny module for creating the plot countNesten - server side
#' @inheritParams plotTriasServer
#' @param data reactive object, data for \code{\link{trendYearRegion}}
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countNestenServer <- function(id, uiText, maxDate = reactive(NULL), data,
  dashReport = NULL) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      results <- reactiveValues()
      
      tmpTranslation <- reactive(translate(uiText(), "countNesten"))
      
      output$titleCountNesten <- renderUI(h3(HTML(tmpTranslation()$title)))
      
      description <- reactive({
          
          decodeText(text = tmpTranslation()$description,
            params = list(maxDate = format(maxDate(), "%d/%m/%Y")))
          
        })
      
      output$descriptionCountNesten <- renderUI(HTML(description()))
      
      ## Periode (grafiek)
      output$period <- renderUI({
          
          req(nrow(data()) > 0)
          
          # initialize
          if (is.null(results$period_value))
            results$period_value <- range(data()$year, na.rm = TRUE)
          
          sliderInput(inputId = ns("period"), 
            label = translate(uiText(), "period")$title,
            value = results$period_value,
            min = min(data()$year, na.rm = TRUE),
            max = max(data()$year, na.rm = TRUE),
            step = 1,
            sep = "")
          
        })
      
      # freeze value - when input$regionLevel changes
      observeEvent(input$regionLevel, {
          
          if (is.null(input$period)) {
            results$period_value <- range(data()$year, na.rm = TRUE)
          } else {
            results$period_value <- input$period
          }
          
        })
      
      # Region filter
      output$regionLevel <- renderUI({
          
          choices <- c("communes", "provinces", "gewest")
          names(choices) <- translate(uiText(), choices)$title
          
          selectInput(inputId = ns("regionLevel"), label = translate(uiText(), "regionLevel")$title,
            choices = choices)
          
        })
      
      output$region <- renderUI({
          
          req(input$regionLevel)
          
          regionVariable <- switch(input$regionLevel,
            communes = "NAAM",
            provinces = "provincie",
            gewest = "GEWEST"
          )
          choices <- sort(unique(data()[[regionVariable]]))
          # expected to be missing for some region levels
          names(choices) <- suppressWarnings(translate(uiText(), choices)$title)
          
          selectInput(inputId = ns("region"), 
            label = translate(uiText(), "regions")$title,
            choices = choices, multiple = TRUE)
          
        })
      
      
      output$typeNesten <- renderUI({
          
          nestChoices <- sort(unique(data()$nest_type))
          # Temporary fix
          # https://github.com/inbo/alien-species-portal/issues/27#issuecomment-1801937223
          nestChoices <- nestChoices[!nestChoices %in% c('NA', 'NULL')]
          names(nestChoices) <- sapply(nestChoices, function(x) 
              translate(uiText(), x)$title)
          
          selectInput(inputId = ns("typeNesten"), 
            label = translate(uiText(), "nest")$title,
            choices = nestChoices, multiple = TRUE)
          
        })
      
      plotData <- reactive({
          
          createSummaryNesten(data = data(), regionLevel = req(input$regionLevel),
            period = req(input$period), typeNesten = req(input$typeNesten))
          
        })
      
      plotResult <- plotModuleServer(id = "countNesten",
        plotFunction = "trendYearRegion",
        data = reactive(plotData()[plotData()$region %in% req(input$region), ]),
        uiText = uiText,
        combine = reactive(input$combine)
      )
      
      ## Report Objects ##
      ## -------------- ##
      
      observe({
          
          # Update when any of these change
          plotResult()
          input
          
          # Return the static values
          dashReport[["countNesten"]] <- c(
            list(
              plot = isolate(plotResult()),
              title = isolate(tmpTranslation()$title),
              description = isolate(description())
            ),
            isolate(reactiveValuesToList(input))
          )
          
        })
      
      
      return(dashReport)
      
    })
  
} 



#' Shiny module for creating the plot countNesten - UI side
#' @inheritParams plotModuleUI
#' @author mvarewyck
#' @import shiny
#' @export
countNestenUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    actionLink(inputId = ns("linkCountNesten"), 
      label = uiOutput(ns("titleCountNesten"))),
    conditionalPanel("input.linkCountNesten % 2 == 1", ns = ns,
      
      uiOutput(ns("descriptionCountNesten")),
      wellPanel(
        fixedRow(
          column(4, uiOutput(ns("regionLevel"))),
          column(8, uiOutput(ns("region")))
        ),
        fixedRow(
          column(4, uiOutput(ns("typeNesten"))),
          column(8, uiOutput(ns("period")))
        ),
        checkboxInput(inputId = ns("combine"), 
          label = "Combine all selected regions")
      ),
      
      plotModuleUI(id = ns("countNesten")),
      optionsModuleUI(id = ns("countNesten"), doWellPanel = FALSE),
      tags$hr()
    
    )
  )

}

#' Table with total number of nests per province
#' 
#' Used on Management page for Vespa Velutina
#' 
#' @inheritParams barplotLenteNesten
#' @return data.frame
#' 
#' @author mvarewyck
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr group_by summarise n mutate ungroup rename all_of
#' @importFrom tidyr pivot_wider
#' @export
tableNesten <- function(df, uiText = NULL) {
  
  # For R CMD check
  NAAM <- provincie <- NULL
  
  
  total_per_year <- df %>% 
    st_drop_geometry() %>% 
    group_by(year) %>% 
    summarise(n = n()) %>% 
    mutate(provincie = translate(uiText, id = "total")$title)
  
  prov_per_year <- df %>% 
    st_drop_geometry()  %>% 
    group_by(provincie, year) %>% 
    summarise(n = n()) %>% 
    ungroup()
  
  newName <- "provincie"
  names(newName) <- translate(uiText, id = "provinces")$title
  
  dt_prov_nesten <- rbind(prov_per_year, total_per_year) %>% 
    tidyr::pivot_wider(id_cols = provincie,
      names_from = year,
      values_from = n) %>%
    rename(all_of(newName))
  
  dt_prov_nesten
  
}




#' Barplot for number of spring nests
#' 
#' Used on Management page for Vespa Velutina
#' 
#' @param df data.frame input data for plotting
#' @inheritParams mapHeatServer
#' @return plotly object
#' 
#' @author mvarewyck
#' @import ggplot2
#' @export
barplotLenteNesten <- function(df, uiText = NULL) {
  
  # For R CMD check
  observation_jaar <- aantal_gemelde_nesten <- prov <- NULL
  
  ggplot(data = df, aes(x = observation_jaar, 
        y = aantal_gemelde_nesten,
        fill = prov)) +
    labs(
      x = translate(uiText, id = "year")$title,
      y = translate(uiText, id = "lenteNesten")$title
    ) +
    scale_fill_discrete(
      name = translate(uiText, id = "provinces")$title
    ) +
    geom_bar(position = "stack", stat = "identity")
  
}

