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
TODO: Add link to packamon - currently using develop branch

```
library(packamon)
writeDockerfile(sourceDir = ".", installSource = TRUE,
overwrite = TRUE, shinyFunction = "alienSpecies::runShiny()")
```

Then, to build the docker image with the latest dockerfile, run in bash

```
cd git/alien-species-portal
docker build -t inbo/alienspecies .
```

Run the new docker image from bash

```
docker run -it -p 3000:3838 inbo/alienspecies
```
