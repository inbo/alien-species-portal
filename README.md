# alien-species-portal
Portal for alien and invasive species indicators

Enkele relevante URLS:
- [MOCKUP](https://docs.google.com/presentation/d/1-ejJsMfjYXXkaR_6YBeylYouPuT0a6vsJAVW4nY9z1U/edit?usp=sharing)
- [TRIAS-Indicators](https://trias-project.github.io/indicators/) => Tutorial met data voorbereiding en gebruik van Trias - package
- [TRIAS-Package](https://github.com/trias-project/trias) => R-package met code voor het maken van grafieken obv checklist en de occurrence - cube
- [TRIAS-Riskmaps](https://trias-project.github.io/risk-maps/) => Web app om risicokaarten te bekijken 
- [Occurrence-Cube](https://raw.githubusercontent.com/trias-project/occ-cube-alien/master/data/processed/be_alientaxa_cube.csv) => dataframe met occurrence gegevens voor de soorten op de unified checklist
- [GRIIS - checlist](https://www.gbif.org/dataset/6d9e952f-948c-4483-9807-575348147c7e) => Unified checklist
- [Grofwildjacht-exotenbranch](https://github.com/inbo/reporting-rshiny-grofwildjacht/tree/exoten) => Code voor het toevoegen van een exoten tab aan de grofwildjacht pagina
- [Species_Dashboard](https://github.com/inbo/IAS_Species_Dashboard) => Code voor het generen van kaartjes (reporting) en gams (indicatoren) voor unielijst soorten & de bijhorende shiny app
- [VespaR](https://github.com/inbo/vespaR) => Code voor het maken van kaartjes en grafieken ivm vespa velutina beheer & de bijhorende shiny app
- [T1](https://zenodo.org/record/3060173#.YaEEmdBKiUm) => shapes, zips & geojsons van de verspreiding van soorten van union concern gedurende de eerste rapportage cyclus.
- [T0](https://zenodo.org/record/3835756#.YaEE4NBKiUm) => shapes, zips & geojsons van de verspreiding van soorten van union concern voor de baseline (van 01-01-2000 tot datum van opname op de lijst).


# Build/Run docker image

The dockerfile needs to be updated only when some of the **dependencies** for `alienSpecies` changed.
This file is generated automatically by packamon: please do not edit by hand. 
The following commands are run to update this dockerfile.

```
if (!require(packamon))
  install.packages("packamon", repos = c(rdepot = "https://repos.openanalytics.eu/repo/public", getOption("repos")))
library(packamon)
writeDockerfile(sourceDir = ".", installSource = TRUE,
overwrite = TRUE, shinyFunction = "alienSpecies::runShiny()")
```

Then, to build the docker image with the latest dockerfile, run in bash

```
cd git/alien-species-portal
docker build -t inbo/alienspecies .
```

Run the new docker image from bash. You need to point docker to the .aws folder on your local system to retrieve the credentials.

```
docker run -it -v ~/.aws:/root/.aws -p 3001:3838 inbo/alienspecies R -e "alienSpecies::setupS3(); alienSpecies::runShiny()" 
```


# Update the translations file

The latest translations file is available in the [aspbo project](https://github.com/inbo/aspbo/). Make sure you navigate to the correct branch.
It is located in the folder: `data/output/UAT_direct/translations.csv`

IMPORTANT: Make sure that you don't change the structure of the translations file (column names, delimiter), otherwise the application will fail to start!
The file will be updated on the S3 bucket automatically.
