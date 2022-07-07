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


test_that("Define user choices", {
    
    taxaChoices <- createTaxaChoices(exotenData = exotenData)
    habitatChoices <- attr(exotenData, "habitats")
    pwChoices <- createDoubleChoices(exotenData = exotenData, 
      columns = c("pathway_level1", "pathway_level2"))
    nativeChoices <- createDoubleChoices(exotenData = exotenData,
      columns = c("native_continent", "native_range"))
    
  })

test_that("Filter data", {
    
    # taxa
    taxaLevels <- c("kingdom", "phylum", "class", "order", "family", "species")
    subData <- filterCombo(exotenData = exotenData, inputValue = "Mammalia", 
      inputLevels = taxaLevels)
    
    # habitat
    subData <- exotenData[habitat %in% habitatChoices[1], ]
    
    
    # pathways
    subData <- filterCombo(exotenData = exotenData, inputValue = pwChoices[[1]]$title, 
        inputLevels = c("pathway_level1", "pathway_level2"))
    
    # degree of establishment
    subData <- exoten[degree_of_establishment %in% "captive", ]
    
    # native
    subData <- filterCombo(exotenData = exotenData, inputValue = nativeChoices[[1]]$title, 
        inputLevels = c("native_continent", "native_range"))
    
    # time
    subData <- exotenData[first_observed %in% c(NA, 2000:2020), ]
    
    # unionlist - always save
    subData <- subData[species %in% unionlistData$scientificName, ]
    
    # region
    subData <- subData[locality %in% "Vlaanderen", ]
    
    # source
    subData <- subData[source %in% "WRiMS", ]
    
  })


## PLOT 1
test_that("Grafiek: Aantal geïntroduceerde uitheemse soorten per jaar", {
    
    tmpResult <- plotTrias(triasFunction = "indicator_introduction_year",
      df = exotenData,
      triasArgs = list(start_year_plot = min(exotenData$first_observed)))
    expect_is(tmpResult, "list")
    
    expect_is(tmpResult$plot, "plotly")
    expect_is(tmpResult$data, "data.frame")
    
  })



## PLOT 2
test_that("Grafiek: Cumulatief aantal geïntroduceerde uitheemse soorten per jaar", {
    
    tmpResult <- plotTrias(triasFunction = "indicator_total_year",
      df = exotenData)
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
    
    tmpResult <- countIntroductionPathway(data = exotenData)
    expect_is(tmpResult, "list")
    
    expect_is(tmpResult$plot, "plotly")
    expect_is(tmpResult$data, "data.frame")
    
  })


## PLOT 4
test_that("Grafiek: Aantal geïntroduceerde uitheemse soorten per jaar per regio van oorsprong", {
    
    tmpResult <- countYearNativerange(df = exotenData, type = "native_continent", jaartallen = 2014)
    expect_is(tmpResult, "list")
    
    expect_is(tmpResult$plot, "plotly")
    expect_is(tmpResult$data, "data.frame")
    
    trias::indicator_native_range_year(data = exotenData)
    
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
    
    tmpResult <- plotTrias(triasFunction = "visualize_pathways_level1", df = exotenData)
    expect_is(tmpResult, "list")
    
    expect_is(tmpResult$plot, "plotly")
    expect_is(tmpResult$data, "data.frame")
    
    visualize_pathways_year_level1(df = exotenData)
    tmpResult <- plotTrias(triasFunction = "visualize_pathways_year_level1", df = exotenData)
    expect_is(tmpResult, "list")
    
    expect_is(tmpResult$plot, "plotly")
    expect_is(tmpResult$data, "data.frame")
    
    
    tmpResult <- plotTrias(triasFunction = "visualize_pathways_level2", df = exotenData,
      triasArgs = list("chosen_pathway_level1" = exoten_pwLevelOne[1]))
    expect_is(tmpResult, "list")
    
    expect_is(tmpResult$plot, "plotly")
    expect_is(tmpResult$data, "data.frame")
    
  })
