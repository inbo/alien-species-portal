

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


#' Line plot for total number of nests per province
#' 
#' Used on Management page for Vespa Velutina
#' 
#' @inheritParams barplotLenteNesten  
#' @return plotly object
#'
#' @import ggplot2
#' @importFrom dplyr rename group_by summarise n ungroup
#' @importFrom sf st_drop_geometry
#' @author mvarewyck
#' @export
countNesten <- function(df, uiText = NULL) {
  
  # For R CMD check
  NAAM <- provincie <- NULL
  
  plotData <- df %>% 
    st_drop_geometry() %>% 
    rename(provincie = NAAM) %>% 
    group_by(provincie, year) %>% 
    summarise(n = n()) %>% 
    ungroup() 
  
  ggplot(data = plotData, aes(x = year, y = n, color = provincie)) + 
    geom_line() +
    labs(
      x = translate(uiText, id = "year")$title,
      y = translate(uiText, id = "number")$title,
      color = translate(uiText, id = "provinces")$title
    ) 
  
}



#' Table with total number of nests per province
#' 
#' Used on Management page for Vespa Velutina
#' 
#' @inheritParams countNesten
#' @return data.frame
#' 
#' @author mvarewyck
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr group_by summarise n mutate ungroup rename all_of
#' @importFrom tidyr pivot_wider
#' @export
tableNesten <- function(df, uiText = NULL) {
  
  # For R CMD check
  NAAM <- NULL
  
  
  total_per_year <- df %>% 
    st_drop_geometry() %>% 
    group_by(year) %>% 
    summarise(n = n()) %>% 
    mutate(NAAM = translate(uiText, id = "total")$title)
  
  prov_per_year <- df %>% 
    st_drop_geometry()  %>% 
    group_by(NAAM, year) %>% 
    summarise(n = n()) %>% 
    ungroup()
  
  newName <- "NAAM"
  names(newName) <- translate(uiText, id = "provinces")$title
  
  dt_prov_nesten <- rbind(prov_per_year, total_per_year) %>% 
    tidyr::pivot_wider(id_cols = NAAM,
      names_from = year,
      values_from = n) %>%
    rename(all_of(newName))
  
  dt_prov_nesten
  
}


