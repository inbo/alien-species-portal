
#' Create taxa choices based on available exotenData
#' @param exotenData data.frame, as read from \code{\link{loadTabularData}}
#' @return nested list with all choices to be shown - for comboTreeInput()
#' 
#' @author mvarewyck
#' @export
createTaxaChoices <- function(exotenData) {
  
  # For R CMD check
  kingdom <- kingdomKey <- NULL
  phylum <- phylumKey <- NULL
  classKey <- NULL
  orderKey <- NULL
  family <- familyKey <- NULL
  species <- key <- NULL
  
  subData <- exotenData[, .(kingdom, phylum, class, order, family, species,
      kingdomKey, phylumKey, classKey, orderKey, familyKey, key)]
  subData <- subData[!duplicated(subData), ]
  
  lapply(unname(split(subData, subData$kingdom, drop = TRUE)), function(kingdom)
      list(id = kingdom[1, kingdomKey], title = kingdom[1, kingdom], 
        subs = lapply(unname(split(kingdom, kingdom$phylum, drop = TRUE)), function(phylum)
            list(id = phylum[1, phylumKey], title = paste(phylum[1, .(kingdom, phylum)], collapse = " > "),
          subs = lapply(unname(split(phylum, phylum$class, drop = TRUE)), function(class)
              list(id = class[1, classKey], title = paste(class[1, .(kingdom, phylum, class)], collapse = " > "), 
              subs = lapply(unname(split(class, class$order, drop = TRUE)), function(order)
                  list(id = order[1, orderKey], title = paste(order[1, .(kingdom, phylum, class, order)], collapse = " > "), 
                  subs = lapply(unname(split(order, order$family, drop = TRUE)), function(family)
                      list(id = family[1, familyKey], title = paste(family[1, .(kingdom, phylum, class, order, family)], collapse = " > "),
                      subs = lapply(unname(split(family, family$species, drop = TRUE)), function(species) 
                          list(id = species[1, key], title = paste(species[1, .(kingdom, phylum, class, order, family, species)], collapse = " > ")))))
              ))))))))

}



#' Create taxa choices based on available exotenData
#' @param exotenData data.frame, as read from \code{\link{loadTabularData}}
#' @return data.frame all choices to be shown - for selectizeInput()
#' 
#' @author mvarewyck
#' @export
createTaxaChoices2 <- function(exotenData) {
  
  # For R CMD check
  kingdom <- kingdomKey <- NULL
  phylum <- phylumKey <- NULL
  classKey <- NULL
  orderKey <- NULL
  family <- familyKey <- NULL
  species <- key <- NULL
  
  subData <- exotenData[, .(kingdom, phylum, class, order, family, species,
      kingdomKey, phylumKey, classKey, orderKey, familyKey, key)]
  subData <- subData[!duplicated(subData), ]
  subData$speciesKey <- subData$key
  
  speciesLevels <- c("kingdom", "phylum", "class", "order", "family", "species")
  
  choices <- do.call(rbind, lapply(seq_along(speciesLevels), function(i) {
        
        iLevel <- speciesLevels[i]
        keyVar <- paste0(iLevel, "Key")
        do.call(rbind, lapply(split(subData, subData[[keyVar]]), function(iData) {
              iData <- iData[!duplicated(iData[[keyVar]]), ]
              longName <- paste(iData[, speciesLevels[1:i], with = FALSE], collapse = " > ")
              data.frame(
                value = iData[[keyVar]], 
                label = iData[[iLevel]],
                long = longName,
                html = paste0("<b>", iData[[iLevel]], "</b>", if (i != 1) paste0("</br>", longName))
              ) 
            }))
        
      }))
  
  choices <- choices[order(choices$label), ]
  
  choices
  
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
  
  columnsTranslate <- c(columns, paste0(columns, "_translate"))
  
  subData <- exotenData[ , columnsTranslate, with = FALSE]
  subData <- subData[!duplicated(subData), ]
  setkeyv(subData, columns)
  
  lapply(unname(split(subData, subData[, columns[1], with = FALSE], drop = TRUE)), function(subData1)
        list(id = subData1[[columns[1]]][1], title = subData1[[columnsTranslate[3]]][1],
          subs = lapply(unname(split(subData1, subData1[, columns[2], with = FALSE], drop = TRUE)), function(subData2)
              list(id = paste(subData2[1, columns, with = FALSE], collapse = ">"), title = paste(subData2[1, columnsTranslate[3:4], with = FALSE], collapse = " > ")))
        ))
  
}


#' Match the selected titles with IDs for search query
#' @param selected character vector, selected titles
#' @param longChoices character vector, all choices with first ID then title
#' @return character, that can be directly pasted in the search query
#' 
#' @author mvarewyck
#' @export
matchCombo <- function(selected, longChoices) {
  
  matchPosition <- sapply(selected, function(iChoice)
      match(iChoice, longChoices[c(FALSE, TRUE)])[1])  # id and title alternate
  matchIds <- longChoices[c(TRUE, FALSE)][matchPosition]
  
  paste(matchIds, collapse = ",")
  
}



#' Filter the exotenData based on the combo filter selection in the application
#' @inheritParams createTaxaChoices
#' @param inputValue character vector, input value of \code{\link{comboTreeInput}}
#' @param inputLevels character vector, levels on which the user can select in 
#' the combo tree (column names in \code{exotenData})
#' @return data.table, subset of \code{exotenData}
#' 
#' @author mvarewyck
#' @importFrom utils tail
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
