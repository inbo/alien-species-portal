
#' Table with number of new introductions per pathway at level 1 and level 2
#' 
#' @param df data.frame with raw data for plotting
#' @importFrom reshape2 melt
#' @importFrom stats xtabs
#' @importFrom utils head
#' @return data.frame
#' 
#' @export
tableIntroductionPathway <- function(df) {
  
  summaryData <- as.data.frame(xtabs(~ pathway_level1 + pathway_level2, data = df))
  summaryData <- summaryData[summaryData$Freq > 0, ]
  
  examples <- as.data.frame(tapply(df$species, list(df$pathway_level1, df$pathway_level2), function(x)
        paste(head(x, n = 5), collapse = ", ")))
  examples$pathway_level1 <- rownames(examples)
  newExamples <- melt(examples, id.vars = "pathway_level1", variable.name = "pathway_level2", value.name = "examples")
 
  fullTable <- merge(summaryData, newExamples, all.x = TRUE)
  colnames(fullTable) <- c("Level 1", "Level 2", "n", "examples")
  
  
  return(
    list(data = fullTable)
  )
  
}



#' Shiny module for creating the plot \code{\link{tableIntroductionPathway}} - server side
#' @inheritParams welcomeSectionServer
#' @param data reactive object, data for \code{\link{countIntroductionYear}}
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
tableIntroductionServer <- function(id, data, uiText) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      subText <- reactive({
          uiText[uiText$plotFunction == "tableIntroductionPathway", ]
        })
      
      output$titleTableIntroduction <- renderUI({
          
          h3(HTML(subText()$title))
          
        })
      
      plotModuleServer(id = "tableIntroduction",
        plotFunction = "tableIntroductionPathway", 
        data = data
      )
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{tableIntroductionPathway}} - UI side
#' @template moduleUI
#' 
#' @author mvarewyck
#' @export
tableIntroductionUI <- function(id) {
  
  ns <- NS(id)

  
  tagList(
    
    actionLink(inputId = ns("linkTableIntroduction"), 
      label = uiOutput(ns("titleTableIntroduction"))),
    conditionalPanel("input.linkTableIntroduction % 2 == 1", ns = ns,
      
      tableModuleUI(id = ns("tableIntroduction")),
      optionsModuleUI(id = ns("tableIntroduction"), doWellPanel = FALSE),
      tags$hr()
      
    )
  )
  
  
}