
In this folder, the required data is collected to create the Rshiny application.

# Tabular Data Sources

## Checklist data

The file `data_input_checklist_indicators.tsv` contains all alien species in Belgium.
Data source + description (GBIF): https://www.gbif.org/dataset/6d9e952f-948c-4483-9807-575348147c7e

## Union list data

The file `eu_concern_species.tsv` contains all alien species of interest from the EU.
Data source + description: https://ec.europa.eu/environment/nature/invasivealien/list/index_en.htm

## t0 and t1 data

The raw data files are processed in `createOccupancyCube()` which creates the file `dfCube.RData`

### t0 data

The files located outside the R package `~/git/alien-species-portal/data/trendOccupancy/ias_belgium_t0_xxx.geojson` 
contain the baseline distribution of the alien species of EU concern in Belgium.
Data source + description (Zenodo): https://zenodo.org/record/3835756#.Yp8nr7wzZhG

### t1 data

The files located outside the R package `~/git/alien-species-portal/data/trendOccupancy/T1_Belgium_Union_List_Species.*`
contain the **reported** distribution of the alien species of EU concern in Belgium (shapefile)
Data source + description (Zenodo): https://zenodo.org/record/3060173#.Yp8nnrwzZhG

## Occurrence Cube

The file `be_alientaxa_cube.csv` contains occurrence cube of alien taxa from the Belgian checklist
The file `be_alientaxa_info.csv` contains taxonomic information for taxa in `be_alientaxa_cube.csv`
Data source + description (Zenodo): https://zenodo.org/record/4299976#.Yp8q57wzZhF

The file `be_alientaxa_info.csv` is enriched with gbifKey information.

The file `be_classes_cube.csv` contains occurrence data grouped at class level, which will be used for correcting the research bias effort. 
Data source + description (Zenodo): https://zenodo.org/record/3635510#.YrBnE7xBxhE

## Protected Areas

The file `intersect_EEA_ref_grid_protected_areas.tsv` contains grid cells with Belgian protected areas.
Data source + description (TrIAS indicators): https://trias-project.github.io/indicators/05_occurrence_indicators_preprocessing.html#1_setup


# Spatial Data

The files `grid/be_1km.*` contain 1x1 km grid data (shapefile)
The files `grid/be_10km.*` contain 10x10 km grid data (shapefile)
Data source + description (EEA): https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2


# Meta Data

## Translations

The file `translations.csv` contains translated labels and descriptions to be displayed in the application.

| Column                       | Type          | Description                                          |
| ---------------------------- | ------------- | ---------------------------------------------------- |
| plotFunction                 | character     | unique ID to link with specific R function or object |
| title_nl                     | character     | Dutch title                                          |
| description_nl               | character     | Dutch description                                    |
| title_fr                     | character     | French title                                         |
| description_fr               | character     | French description                                   |
| title_en                     | character     | English title                                        |
| description_en               | character     | English description                                  |


## Keys

We construct a file using the function `createKeyData()` which combines multiple keys to match data from different sources.

### taxonkey

The file `be_alientaxa_info.csv` contains taxonomic information for taxa in `be_alientaxa_cube.csv`
Data source + description (Zenodo): https://zenodo.org/record/4299976#.Yp8q57wzZhF

### gbif

TODO Linking the gbifKey, taxonKey, Latin name, KB/DAISIEkey 
