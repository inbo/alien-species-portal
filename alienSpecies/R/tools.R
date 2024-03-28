# Tools for the application
# 
# Author: mvarewyck
###############################################################################



#' Name file given content information
#' @param species character vector, selected species
#' @param period numeric vector, selected year(s)
#' @param content character, more information on the file
#' @param fileExt character, extension of the file
#' @return character, suggested file name pasting together \code{species},
#' \code{year}, \code{content}, \code{fileExt}
#' @author mvarewyck
#' @export
nameFile <- function(species = NULL, period = NULL, content, fileExt) {
  
  paste0(
    if (!is.null(species))
      paste0(paste(species, collapse = "-"), "_"),
    if (!is.null(period))
      paste0(paste(period, collapse = "-"), "_"),
    
    content, ".", fileExt
  )

}


#' Convert original variable names to display names
#' @param text character vector, names to be 'translated'
#' @param translations data.frame, contains translations for \code{text}
#' @return named character vector, names are the new display names and values
#' are the original values from \code{text}
#' 
#' @author mvarewyck
#' @export
displayName <- function(text, translations = NULL) {
  
  
  if (is.null(translations)) {
    names(text) <- text
    return(text)
  } 
  
  newNames <- sapply(text, function(x) {
      
      toReturn <- translate(translations, x)$title
      if (is.na(toReturn))
        x else
        toReturn
      
    })
  
  switchNames <- names(newNames)
  names(switchNames) <- newNames
  
  return(switchNames)
  
}


#' Transform vector ready to be in title
#' 
#' @param vector character vector with elements to be collapsed together into one string
#' @return string Elements of the vector are separated by comma in the string, 
#' ultimate and penultimate element are separated by 'and'. 
#' 
#' @author Eva Adriaensen
#' @export
vectorToTitleString <- function(vector) {
  
  vector <- sort(unique(vector))
  
  if (length(vector) > 3) {
    paste0(paste(vector[1:3], collapse = ", "), ", ...")
  } else if (length(vector) > 1) { 
    paste(vector[1:length(vector)], collapse = ", ")
  } else {
    vector
  }

}


#' Transform numeric vector with years ready to be in title
#' 
#' @param year numeric, vector with year values for which to print the range
#' @param brackets logical, should the output string with years be wrapped in brackets? Defaults to TRUE 
#' @return string ready to be printed in the title 
#' 
#' @author Eva Adriaensen
#' @export
yearToTitleString <- function(year, brackets = TRUE) {
  
  year <- sort(unique(year))
  
  toReturn <- if (year[1] != year[length(year)])
      paste(year[1], "-", year[length(year)]) else
      year[1]
  
  if (brackets)
    paste0("(", toReturn, ")") else 
    toReturn

}



#' Helper function to draw bullet in specific color
#' 
#' @param color color in hexadecimal notation
#' @importFrom htmltools div
#' @export
drawBullet <- function(color) {
  
  div(style = paste0("background-color: ", color, 
      "; display: inline-block; vertical-align:top; width: 20px; height: 20px; border-radius: 10px"))
  
}



#' Replicate inbo colors if more than 9 needed
#' @param nColors integer, number of colors needed
#' @return list with
#' colors = character vector with (repeated) inbo colors
#' warning = character, not NULL if colors are repeated
#' 
#' @author mvarewyck
#' @importFrom INBOtheme inbo_palette
#' @export
replicateColors <- function(nColors) {
  
  rest <- nColors%%9
  times <- floor(nColors/9)
  
  colors <- c()
  
  if (times > 0)
    colors <- rep(inbo_palette(n = 9), times)
  if(rest > 0)
    colors <- c(colors, inbo_palette(n = rest))
  
  # warning if noLocaties exceeds 9 colours
  warningText <- NULL
  if(nColors > 9) {
    warningText <- "Door de ruime selectie werden de kleuren van deze grafiek hergebruikt. 
      Hierdoor is verwarring mogelijk. Verklein de selectie om dit te voorkomen."
  }
  
  return(
    list(
      colors = colors, 
      warning = warningText
    )
  )
  
  
}


#' Capitalize first letter
#' @param names character vector, names to be capitalized (e.g. countries)
#' @param keepNames boolean, whether to keep the names for the returned vector;
#' default is TRUE
#' @return character vector, capitalized version of \code{names}
#' @author mvarewyck
#' @export
simpleCap <- function(names, keepNames = TRUE) {
  
  sapply(names, function(x) {
      
      if (is.na(x))
        return(x)
      
      s <- tolower(as.character(x))
      paste0(toupper(substring(s, 1, 1)), substring(s, 2))
      
    }, USE.NAMES = keepNames)
  
}


#' get path of INBO logo file
#' 
#' @return character, path of logo file
#' @param type character, defines which logo is returned; should be one of 
#' \code{c("inbo", "trias")}
#' @author mvarewyck
#' @export
getPathLogo <- function(type = c("inbo", "trias", "combined")) {
  
  type <- match.arg(type)
  
  system.file("app/www", switch(type,
      inbo = "logo.png",
      trias = "logoTrias.png",
      combined = "logoCombined.png"
    ), package = "alienSpecies")

}



#' Style plotly object for Rmd report
#' 
#' @param myPlot plotly object
#' @return plotly object
#' 
#' @author mvarewyck
#' @import plotly
#' @export
plotlyReport <- function(myPlot) {
  
  myPlot <- myPlot %>% config(displayModeBar = FALSE)
  
  # remove gridlines
  if (is.null(myPlot$x$layoutAttrs[[1]]$xaxis))
    myPlot$x$layoutAttrs[[1]]$xaxis <- list(showgrid = FALSE) else
    myPlot$x$layoutAttrs[[1]]$xaxis$showgrid <- FALSE
#  myPlot$x$layoutAttrs[[1]]$xaxis$ticks <- "outside"
  
  if (is.null(myPlot$x$layoutAttrs[[1]]$yaxis))
    myPlot$x$layoutAttrs[[1]]$yaxis <- list(showgrid = FALSE) else
    myPlot$x$layoutAttrs[[1]]$yaxis$showgrid <- FALSE
#  myPlot$x$layoutAttrs[[1]]$yaxis$ticks <- "outside"
  
  myPlot %>% layout(autosize = FALSE, width = 1000, height = 400)
  
}

