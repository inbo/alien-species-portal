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
      
      toReturn <- translate(translations, x)
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
#' Separates elements of vectors with a comma; the final element of the vector is seperated with 'en'
#' 
#' @param vector character vector with elements to be collapsed together into one string
#' @return string Elements of the vector are separated by comma in the string, 
#' ultimate and penultimate element are separated by 'en'. 
#' 
#' @author Eva Adriaensen
#' @export
vectorToTitleString <- function(vector) {
  
  sub(",\\s+([^,]+)$", " en \\1", toString(vector))
  
}


#' Transform numeric vector with years ready to be in title
#' 
#' @param year numeric, vector of length 1 or 2
#' @param brackets logical, should the output string with years be wrapped in brackets? Defaults to TRUE 
#' @return string Numbers of the vector are separated by 'tot' in the string, 
#' brackets are put around (\code{brackets = TRUE}). If length of vector is only one, or if both elements
#' of the vector are the same, a string with the year between brackets is returned. 
#' 
#' @author Eva Adriaensen
#' @export
yearToTitleString <- function(year, brackets = TRUE) {
  
  if (!is.numeric(year) | any(is.na(year))) {
    stop("Argument is niet aanvaard.")
  }
  
  if (length(year) == 2) {
    ifelse(year[1] != year[2],
      if (brackets) paste0("(", year[1], " - ", year[2], ")") else paste0(year[1], " - ", year[2]),
      if (brackets) paste0("(", year[1], ")") else paste0(year[1])
    )
  } else if (length(year) == 1) {
    if (brackets) paste0("(", year[1], ")") else paste0(year[1]) 
  } else {
    stop("Kan periode niet formatteren.")
  }
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