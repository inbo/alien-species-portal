# Functions for the Target 6 indicator graph (BCubed issue #208)
#
# Author: mvarewyck
###############################################################################


#' Create Target 6 indicator plot
#'
#' Rate of newly established invasive alien species per year in Belgium,
#' with a 95% confidence interval ribbon
#' @param df data.frame with columns \code{Year}, \code{Annual_Rate},
#' \code{Lower_CI_95}, \code{Upper_CI_95}
#' @param xLab character, x-axis label
#' @param yLab character, y-axis label
#' @return list with plotly object and data.frame
#'
#' @author mvarewyck
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @export
target6Indicator <- function(df, xLab = translate("year")$title,
  yLab = translate("target6_yLab")$title) {

  p <- ggplot(df, aes(x = Year, y = Annual_Rate)) +
    geom_ribbon(
      aes(ymin = Lower_CI_95, ymax = Upper_CI_95),
      fill = "grey70",
      alpha = 0.5
    ) +
    geom_line(
      color = "black",
      linewidth = 1
    ) +
    labs(x = xLab, y = yLab)

  list(plot = ggplotly(p), data = df)

}


#' Shiny module for creating the plot \code{\link{target6Indicator}} - server side
#' @inheritParams welcomeSectionServer
#' @param data reactive object, data for \code{\link{target6Indicator}}
#' @return no return value
#'
#' @author mvarewyck
#' @import shiny
#' @export
target6Server <- function(id, data) {

  # Hardcoded fallback while translate("target6") isn't available yet on the
  # bucket-hosted translations (BCubed issue #208) - once target6_title/
  # target6_description exist there, they take over automatically
  defaultTitle <- "Target 6 indicator: rate of invasive alien species establishment"
  defaultDescription <- paste(
    "Number of newly established invasive species per year in Belgium since 1970,",
    "calculated using the b3alien package to correct for detection lags.",
    "This indicator is based on the Belgian checklist of the Global Register of",
    "Introduced and Invasive Species (GRIIS) (Desmet et al., 2025) and includes",
    "only species classified as established, invasive, or widespread invasive."
  )

  moduleServer(id,
    function(input, output, session) {

      ns <- session$ns

      tmpTranslation <- reactive(translate("target6"))

      output$titleTarget6 <- renderUI({
          title <- tmpTranslation()$title
          h3(HTML(if (title != "target6") title else defaultTitle))
        })

      output$descriptionTarget6 <- renderUI({
          description <- tmpTranslation()$description
          HTML(if (nzchar(description)) description else defaultDescription)
        })

      plotModuleServer(id = "target6",
        plotFunction = "target6Indicator",
        data = data
      )

    })

}


#' Shiny module for creating the plot \code{\link{target6Indicator}} - UI side
#' @inheritParams plotModuleUI
#'
#' @author mvarewyck
#' @export
target6UI <- function(id) {

  ns <- NS(id)

  tags$div(class = "container",

    actionLink(inputId = ns("linkTarget6"),
      label = uiOutput(ns("titleTarget6"))),
    conditionalPanel("input.linkTarget6 % 2 == 1", ns = ns,

      uiOutput(ns("descriptionTarget6")),

      plotModuleUI(id = ns("target6"), height = "600px"),
      optionsModuleUI(id = ns("target6"), doWellPanel = FALSE),
      tags$hr()

    )
  )

}
