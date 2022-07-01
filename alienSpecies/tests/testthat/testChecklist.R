# Test plots and summaries for Exoten
# 
# Author: mvarewyck, eadriaensen
###############################################################################

##
## Load exoten data
##

exotenData <- loadTabularData(type = "indicators")
expect_is(exotenData, "data.frame")
unionlistData <- loadTabularData(type = "unionlist")
expect_is(unionlistData, "data.frame")
occurrenceData <- loadTabularData(type = "occurrence")
expect_is(occurrenceData, "data.frame")
translations <- loadMetaData(type = "ui")

##
## Combine duplicated keys
##
simpleData <- tableIndicators(exotenData = exotenData, 
  unionlistData = unionlistData,
  occurrenceData = occurrenceData)

##
## Explore data
##

colnames(exotenData)
sapply(exotenData, class)        
sapply(exotenData, function(x) if (length(unique(x)) < 20) table(x, exclude = NULL) else summary(x))
sapply(exotenData, function(x) sum(is.na(x)))
#                     key          first_observed           last_observed 
#                       0                    8166                    8166 
#                 kingdom                  phylum                   class 
#                       0                       2                      11 
#                   order                  family                locality 
#                       4                       4                       0 
#              locationId            native_range degree_of_establishment 
#                       0                     603                   12486 
#          pathway_level1          pathway_level2                  marine 
#                       0                       0                      83 
#              freshwater             terrestrial                  source 
#                      83                      83                      83 
#        native_continent 
#                     603 


##
## Define user choices
##
exoten_time <- min(exotenData$first_observed, na.rm = TRUE):max(exotenData$first_observed, na.rm = TRUE)

exoten_regionChoices <- unique(exotenData$locality)
exoten_region <- exoten_regionChoices[1]

exoten_doeChoices <- unique(exotenData$degree_of_establishment[!is.na(exotenData$degree_of_establishment)])
exoten_doe <- exoten_doeChoices[1:length(exoten_doeChoices)]

exoten_unionlistChoices <- c("all", "euConcern")
exoten_unionlistChoice <- exoten_unionlistChoices[1] 

exoten_unionlist <- if (exoten_unionlistChoice %in% "all") {
    # i.e. include all species
    c(NA, unique(sort(exotenData$species)))
    
  } else if (exoten_unionlistChoice %in% c("euConcern") ) {
    
    # filter only species that are present in the unionlist 
    ind <-  which(exotenData$species %in% unionlistData$scientificName)
    unique(sort(exotenData$species[ind]))
    
  }



## these are currently manually defined
exoten_habitatChoices <- colnames(exotenData)[colnames(exotenData) %in% c("marine", "freshwater", "terrestrial")]
exoten_habitat <- exoten_habitatChoices[1:length(exoten_habitatChoices)] 

## taxonomy
## kingdom -> phylum -> class -> order -> family

#kingdom
exoten_kingdomChoices <- unique(exotenData$kingdom[!is.na(exotenData$kingdom)])
exoten_kingdom <- exoten_kingdomChoices[3] #[1:length(exoten_kingdomChoices)]

#phylum
exoten_phylumChoices <- na.omit(unique(exotenData$phylum[exotenData$kingdom %in% exoten_kingdom]))
exoten_phylum <- exoten_phylumChoices[1:length(exoten_phylumChoices)] 

#class
exoten_classChoices <-  na.omit(unique(exotenData$class[exotenData$kingdom %in% exoten_kingdom & 
        exotenData$phylum %in% exoten_phylum]))
exoten_class <- exoten_classChoices[1:length(exoten_classChoices)]

#order
exoten_orderChoices <- na.omit(unique(exotenData$order[exotenData$kingdom %in% exoten_kingdom & 
        exotenData$phylum %in% exoten_phylum &
        exotenData$class %in% exoten_class]))
exoten_order <- exoten_orderChoices[1:length(exoten_orderChoices)]

#family
exoten_familyChoices <- na.omit(unique(exotenData$family[exotenData$kingdom %in% exoten_kingdom & 
        exotenData$phylum %in% exoten_phylum &
        exotenData$class %in% exoten_class &
        exotenData$order %in% exoten_order]))
exoten_family <- exoten_familyChoices[1:length(exoten_familyChoices)]

## pathways
## pathway_level1 > pathway_level2

# pathway_level1
exoten_pwLevelOneChoices <- unique(exotenData$pathway_level1[!is.na(exotenData$pathway_level1)])
exoten_pwLevelOne <- exoten_pwLevelOneChoices[3:7] #[1:length(exoten_pwLevelOneChoices)]

# pathway_level2
exoten_pwLevelTwoChoices <- na.omit(unique(exotenData$pathway_level2[exotenData$pathway_level1 %in% exoten_pwLevelOne]))
exoten_pwLevelTwo <- exoten_pwLevelTwoChoices[3:7] #[1:length(exoten_pwLevelTwoChoices)]

##
## Create data upon user choices
##
toRetain <- exotenData$first_observed %in% exoten_time &
  exotenData$locality %in% exoten_region &
  exotenData$degree_of_establishment %in% exoten_doe &
  ##habitat
  apply(exotenData[, .SD, .SDcols = which(colnames(exotenData) %in% exoten_habitat)], 1, function(x) any(x, na.rm = TRUE))    


## additional filters
## kingdom
extraFilter <- exotenData$kingdom %in% exoten_kingdom &
  # kingdom - dependent
  exotenData$phylum %in% exoten_phylum &
  exotenData$class %in% exoten_class &
  exotenData$order %in% exoten_order &
  exotenData$family %in% exoten_family &
  ## pathway
  exotenData$pathway_level1 %in% exoten_pwLevelOne & #c("corridor", "escape", "contaminant") 
  # pathway - dependent
  exotenData$pathway_level2 %in% exoten_pwLevelTwo &
  # Uniion list
  exotenData$species %in% exoten_unionlist


exoten_data <- exotenData[toRetain,]
dim(exoten_data)

## PLOT 1
test_that("Grafiek: Aantal geïntroduceerde uitheemse soorten per jaar", {
    
    tmpResult <- plotTrias(triasFunction = "indicator_introduction_year",
      df = exoten_data,
      triasArgs = list(start_year_plot = min(exoten_data$first_observed)))
    expect_is(tmpResult, "list")
    
    expect_is(tmpResult$plot, "plotly")
    expect_is(tmpResult$data, "data.frame")
    
  })



## PLOT 2
test_that("Grafiek: Cumulatief aantal geïntroduceerde uitheemse soorten per jaar", {
    
    tmpResult <- plotTrias(triasFunction = "indicator_total_year",
      df = exoten_data)
    expect_is(tmpResult, "list")
    
    expect_is(tmpResult$plot, "plotly")
    expect_is(tmpResult$data, "data.frame")
    
  })

test_that("Grafiek: Mate van verspreiding van de Unielijstsoorten", {
    
    load(file = file.path(dataDir, "dfCube.RData"))
    occupancy <- createOccupancyData(dfCube = dfCube)
    
    tmpResult <- countOccupancy(df = occupancy)
    
    expect_is(tmpResult, "list")
    
    expect_is(tmpResult$plot, "plotly")
    expect_is(tmpResult$data, "data.frame")
    
  })


## PLOT 3
test_that("Grafiek: Aantal geïntroduceerde uitheemse soorten per pathway", {
    
    tmpResult <- countIntroductionPathway(data = exoten_data)
    expect_is(tmpResult, "list")
    
    expect_is(tmpResult$plot, "plotly")
    expect_is(tmpResult$data, "data.frame")
    
  })


## PLOT 4
test_that("Grafiek: Aantal geïntroduceerde uitheemse soorten per jaar per regio van oorsprong", {
    
    tmpResult <- countYearNativerange(data = exoten_data, type = "native_continent", jaartallen = 2014)
    expect_is(tmpResult, "list")
    
    expect_is(tmpResult$plot, "plotly")
    expect_is(tmpResult$data, "data.frame")
    
    trias::indicator_native_range_year(data = exoten_data)
    
  })


# TABLE: tableIntroductionPathway
test_that("Introduction pathways per category", {
    
    tmpResult <- plotTrias(triasFunction = "get_table_pathways", df = exotenData,
      triasArgs = list(species_names = "species"),
      outputType = "table", uiText = NULL)
    expect_is(tmpResult, "list")
    
    expect_is(tmpResult$data, "data.frame")
    
  })


# PLOT: CBD Level 1/2 introduction pathways
test_that("CBD Level 1/2 introduction pathways", {
    
    tmpResult <- plotTrias(triasFunction = "visualize_pathways_level1", df = exoten_data)
    expect_is(tmpResult, "list")
    
    expect_is(tmpResult$plot, "plotly")
    expect_is(tmpResult$data, "data.frame")
    
    visualize_pathways_year_level1(df = exotenData)
    tmpResult <- plotTrias(triasFunction = "visualize_pathways_year_level1", df = exotenData)
    expect_is(tmpResult, "list")
    
    expect_is(tmpResult$plot, "plotly")
    expect_is(tmpResult$data, "data.frame")
    
    
    tmpResult <- plotTrias(triasFunction = "visualize_pathways_level2", df = exoten_data,
      triasArgs = list("chosen_pathway_level1" = exoten_pwLevelOne[1]))
    expect_is(tmpResult, "list")
    
    expect_is(tmpResult$plot, "plotly")
    expect_is(tmpResult$data, "data.frame")
    
  })