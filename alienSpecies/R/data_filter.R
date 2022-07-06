
#' Create taxa choices based on available exotenData
#' @param exotenData data.frame, as read from \code{\link{loadTabularData}}
#' @return nested list with all choices to be shown
#' 
#' @author mvarewyck
#' @export
createTaxaChoices <- function(exotenData) {
  
  # For R CMD check
  kingdom <- NULL
  phylum <- NULL
  family <- NULL
  species <- NULL
  
  subData <- exotenData[, .(kingdom, phylum, class, order, family, species)]
  subData <- subData[!duplicated(subData), ]
  
  lapply(unname(split(subData, subData$kingdom, drop = TRUE)), function(kingdom)
      list(id = kingdom[1, kingdom], title = kingdom[1, kingdom], 
        subs = lapply(unname(split(kingdom, kingdom$phylum, drop = TRUE)), function(phylum)
            list(id = phylum[1, phylum], title = paste(phylum[1, .(kingdom, phylum)], collapse = " > "),
          subs = lapply(unname(split(phylum, phylum$class, drop = TRUE)), function(class)
              list(id = class[1, class], title = paste(class[1, .(kingdom, phylum, class)], collapse = " > "), 
              subs = lapply(unname(split(class, class$order, drop = TRUE)), function(order)
                  list(id = order[1, order], title = paste(order[1, .(kingdom, phylum, class, order)], collapse = " > "), 
                  subs = lapply(unname(split(order, order$family, drop = TRUE)), function(family)
                      list(id = family[1, family], title = paste(family[1, .(kingdom, phylum, class, order, family)], collapse = " > "),
                      subs = lapply(unname(split(family, family$species, drop = TRUE)), function(species) 
                          list(id = species[1, species], title = paste(species, collapse = " > ")))))
              ))))))))

}



#' Create pathway choices based on available exotenData
#' @inheritParams createTaxaChoices
#' @param columns character vector, column names in \code{exotenData} for which 
#' to create choices list
#' @return nested list with all choices to be shown
#' 
#' @author mvarewyck
#' @importFrom data.table setkeyv
#' @export
createDoubleChoices <- function(exotenData,
  columns = c("pathway_level1", "pathway_level2")) {
  
  if (length(columns) != 2)
    stop("Exactly 2 columns should be defined")
  
  subData <- exotenData[ , ..columns]
  subData <- subData[!duplicated(subData), ]
  setkeyv(subData, columns)
  
  lapply(unname(split(subData, subData[, columns[1], with = FALSE], drop = TRUE)), function(subData1)
        list(id = subData1[[columns[1]]][1], title = subData1[[columns[1]]][1],
          subs = lapply(unname(split(subData1, subData1[, columns[2], with = FALSE], drop = TRUE)), function(subData2)
              list(id = subData2[[columns[2]]][1], title = paste(subData2[1, ], collapse = " > ")))
        ))
  
}



#' Filter the exotenData based on the combo filter selection in the application
#' @inheritParams createTaxaChoices
#' @param inputValue character vector, input value of \code{\link{comboTreeInput}}
#' @param inputLevels character vector, levels on which the user can select in 
#' the combo tree (column names in \code{exotenData})
#' @return data.table, subset of \code{exotenData}
#' 
#' @author mvarewyck
#' @export
filterCombo <- function(exotenData, inputValue, inputLevels) {
 
  if (is.null(inputValue))
    stop("Please select value")
  
  mySelection <- do.call(rbind, lapply(inputValue, function(x) {
        splitSelection <- strsplit(x, " > ")[[1]]
        data.frame( 
          level = inputLevels[length(splitSelection)],
          value = tail(splitSelection, n = 1)
        )
      }))
  toInclude <- rep(FALSE, nrow(exotenData))
  
  for (i in seq_len(nrow(mySelection)))
    toInclude <- toInclude | exotenData[[mySelection$level[i]]] %in% mySelection$value[i]
  
  exotenData[toInclude, ]
 
}
