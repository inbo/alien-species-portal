# Tools for the application
# 
# Author: mvarewyck
###############################################################################


#' Get translation of specific label
#' @param translations data.frame with 2 columns:
#' \itemize{
#' \item{label}{character, label which points to the correct translation}
#' \item{dutch, french or english}{character, translated text}
#' }
#' @param label character, label to be translated
#' @return character, translated label
#' 
#' @author mvarewyck
#' @export
getTranslation <- function(translations, label) {
  
  translations[match(translations$label, label), 2]
  
} 
