#' Plot number of new introductions per pathway at level 2
#' 
#' Split up by pathways at level 1
#' 
#' @param df data.frame with raw data for plotting
#' @importFrom trias visualize_pathways_level2
#' @importFrom plotly ggplotly subplot
#' @return list with plot and data that was used for the plot
#' 
#' @export
countIntroductionPathway <- function(df){
  
  ## generate plot
  
  plots <- lapply(unique(df$pathway_level1), function(x) {
      p <- visualize_pathways_level2(
        df = df, 
        chosen_pathway_level1 = x,
        x_lab = "Aantal ge\u00EFntroduceerde uitheemse soorten"
      ) 
      
      ggplotly(p) %>%
        add_annotations(text = x,
          x = 0.5,
          y = 1.1,
          yref = "paper",
          xref = "paper",
          xanchor = "middle",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 15))
    })
  
  p <- subplot(plots, 
    nrows = ceiling(length(plots) / 2),
    margin = c(0.06, 0.06, 0.1, 0.06)) #l-r-t-d #%>%
#      layout(xaxis = list(
#              title = "x Axis"), 
#            yaxis = list(title = "y"))
  
  
  
  return(list(plot = p, data = df))
  
  
}




#' Shiny module for creating the plot \code{\link{countIntroductionPathway}} - server side
#' @inheritParams welcomeSectionServer
#' @param data reactive object, data for \code{\link{countIntroductionPathway}}
#' @param time numeric vector, selected time span
#' @param region NULL currently not implemented
#' @param pathway character vector, selected pathways 
#' @inheritParams countIntroductionYear
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countIntroductionPathwayServer <- function(id, uiText, data, region, pathway, time) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      subText <- reactive({
          uiText[uiText$plotFunction == "countIntroductionPathway", ]
        })
      
      output$titleIntroductionPathway <- renderUI({
          
          h3(HTML(paste(subText()$title, 
                vectorToTitleString(region()),
                if (!is.null(pathway())) 
                  paste0("(", vectorToTitleString(pathway()), ")"),
                yearToTitleString(req(time()))
              )))
          
        })
      
      plotModuleServer(id = "introductionPathway",
        plotFunction = "countIntroductionPathway", 
        data = data
      )
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{countIntroductionPathway}} - UI side
#' @template moduleUI
#' 
#' @author mvarewyck
#' @export
countIntroductionPathwayUI <- function(id) {
  
  ns <- NS(id)
  
  
  tagList(
    
    actionLink(inputId = ns("linkIntroductionPathway"), 
      label = uiOutput(ns("titleIntroductionPathway"))),
    conditionalPanel("input.linkIntroductionPathway % 2 == 1", ns = ns,
      
      plotModuleUI(id = ns("introductionPathway"), height = "800px"),
      tags$hr()
    
    )
  )
  
}