# Test plots and summaries for union checklist - global indicators
# 
# Author: mvarewyck, eadriaensen
###############################################################################

##
## Load exoten data
##

exotenData <- loadTabularData(type = "indicators")
unionlistData <- loadTabularData(type = "unionlist")
occurrenceData <- loadTabularData(type = "occurrence")
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

test_that("Trigger errors data filtering" , {
    
    expect_error(createDoubleChoices(exotenData = exotenData,
        columns = letters[1:3]))

    expect_error(filterCombo(exotenData = exotenData, inputValue = NULL, 
        inputLevels = NULL))
  })

test_that("Translate exoten data", {
    
#    exotenData <- loadTabularData(type = "indicators")
    translations <- loadMetaData(language = "nl")
    
    time1 <- Sys.time()
    exotenData[, pathway_level2_translate := translate(translations, do.call(paste, c(.SD, sep = "_")))$title,
      .SDcols = c("pathway_level1", "pathway_level2")]
    print(Sys.time() - time1)
#    exotenData$habitat_translate <- sapply(exotenData$habitat, function(x) 
#        paste(translate(translations, strsplit(x, split = "\\|")[[1]])$title, collapse = "|"))
#    print(Sys.time() - time1)
    exotenData[, ':=' (
        pathway_level1_translate = translate(translations, pathway_level1)$title,
        native_continent_translate = translate(translations, native_continent)$title,
        native_range_translate = translate(translations, native_range)$title,
        degree_of_establishment_translate = translate(translations, degree_of_establishment)$title,
        habitat_translate = translate(translations, habitat)$title
      )]    
    print(Sys.time() - time1)
    
    expect_is(exotenData, "data.table")
    
  })

test_that("Define user choices and filter data", {
    
    # choices
    taxaChoices <- createTaxaChoices(exotenData = exotenData)
    tmpChoices <- unlist(taxaChoices)
    matchTaxum <- match("185465684", tmpChoices)[1]
    tmpChoices[matchTaxum + 1]
    
    habitatChoices <- attr(exotenData, "habitats")
    exotenData[, ':=' (
        pathway_level1_translate = translate(translations, pathway_level1)$title,
        pathway_level2_translate = translate(translations, pathway_level2)$title)
    ]
    pwChoices <- createDoubleChoices(exotenData = exotenData, 
      columns = c("pathway_level1", "pathway_level2"))
    expect_type(pwChoices, "list")
    
    exotenData[, ':=' (
        native_continent_translate = translate(translations, native_continent)$title,
        native_range_translate = translate(translations, native_range)$title)
    ]
    nativeChoices <- createDoubleChoices(exotenData = exotenData,
      columns = c("native_continent", "native_range"))
    expect_type(nativeChoices, "list")
    
    
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
    subData <- exotenData[degree_of_establishment %in% "captive", ]
    
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
    
    expect_s3_class(subData, "data.table")
    
  })


## PLOT 1
test_that("Grafiek: Aantal geïntroduceerde uitheemse soorten per jaar", {
    
    tmpResult <- plotTrias(triasFunction = "indicator_introduction_year",
      df = exotenData,
      triasArgs = list(start_year_plot = min(exotenData$first_observed, na.rm = TRUE)))
    expect_type(tmpResult, "list")
    
    expect_s3_class(tmpResult$plot, "plotly")
    expect_s3_class(tmpResult$data, "data.frame")
    
  })



## PLOT 2
test_that("Grafiek: Cumulatief aantal geïntroduceerde uitheemse soorten per jaar", {
    
    tmpResult <- plotTrias(triasFunction = "indicator_total_year",
      df = exotenData)
    expect_type(tmpResult, "list")
    
    expect_s3_class(tmpResult$plot, "plotly")
    expect_s3_class(tmpResult$data, "data.frame")
    
  })

test_that("Grafiek: Mate van verspreiding van de Unielijstsoorten", {
    
    #readS3(file = "dfCube.RData")
    occupancy <- loadOccupancyData()
    
    tmpResult <- countOccupancy(df = occupancy)
    
    expect_type(tmpResult, "list")
    
    expect_s3_class(tmpResult$plot, "plotly")
    expect_s3_class(tmpResult$data, "data.frame")
    
  })


## PLOT 4
test_that("Grafiek: Aantal geïntroduceerde uitheemse soorten per jaar per regio van oorsprong", {
    
    tmpResult <- trias::indicator_native_range_year(df = exotenData)
    expect_type(tmpResult, "list")
    
    expect_s3_class(tmpResult$interactive_plot, "plotly")
    expect_s3_class(tmpResult$data, "data.frame")
    
    
  })


# TABLE: tableIntroductionPathway
test_that("Introduction pathways per category", {
    
    uiText <- loadMetaData(language = "nl")
    exotenData$pathway_level1 <- translate(uiText, exotenData$pathway_level1)$title
    exotenData$pathway_level2 <- translate(uiText, exotenData$pathway_level2)$title
    
    tmpResult <- plotTrias(triasFunction = "get_table_pathways", df = exotenData,
      triasArgs = list(species_names = "species"),
      outputType = "table", uiText = NULL)
    expect_type(tmpResult, "list")
    
    expect_s3_class(tmpResult$data, "data.frame")
    
  })


# PLOT: CBD Level 1/2 introduction pathways
test_that("CBD Level 1/2 introduction pathways", {
    
    # Level 1
    tmpResult <- plotTrias(triasFunction = "visualize_pathways_level1", df = exotenData,
      triasArgs = list(cbd_standard = FALSE))
    expect_type(tmpResult, "list")
    
    expect_s3_class(tmpResult$plot, "plotly")
    expect_s3_class(tmpResult$data, "data.frame")
    
    visualize_pathways_year_level1(df = exotenData)
    tmpResult <- plotTrias(triasFunction = "visualize_pathways_year_level1", df = exotenData)
    expect_type(tmpResult, "list")
    
    expect_s3_class(tmpResult$plot, "plotly")
    expect_s3_class(tmpResult$data, "data.frame")
    
    # Level 2
    exotenData[, ':=' (
        pathway_level1_translate = translate(translations, pathway_level1)$title,
        pathway_level2_translate = translate(translations, pathway_level2)$title)
    ]
    levelOneChoice <- createDoubleChoices(exotenData = exotenData, 
      columns = c("pathway_level1", "pathway_level2"))[[1]]$id
    
    tmpResult <- plotTrias(triasFunction = "visualize_pathways_level2", df = exotenData,
      triasArgs = list("chosen_pathway_level1" = levelOneChoice))
    expect_type(tmpResult, "list")
    
    expect_s3_class(tmpResult$plot, "plotly")
    expect_s3_class(tmpResult$data, "data.frame")
    
    tmpResult <- plotTrias(triasFunction = "visualize_pathways_year_level2", df = exotenData,
      triasArgs = list("chosen_pathway_level1" = levelOneChoice))
    expect_type(tmpResult, "list")
    
  })
