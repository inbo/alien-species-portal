# Generated by packamon: do not edit by hand
# Instead of modifying this file, you can modify a template. See ?init for details.

FROM openanalytics/r-ver:4.2.2

# System libraries (incl. system requirements for R packages)
RUN apt-get update && apt-get install --no-install-recommends -y \
    libssl-dev \
    zlib1g-dev \
    pandoc \
    libgeos-dev \
    libgeos++-dev \
    libharfbuzz-dev \
    make \
    gdal-bin \
    libxml2-dev \
    libfribidi-dev \
    libproj-dev \
    libgdal-dev \
    libicu-dev \
    imagemagick \
    libudunits2-dev \
    libcurl4-openssl-dev \
    libtiff-dev \
    libpng-dev \
    && rm -rf /var/lib/apt/lists/*

# Missing for tidyverse install

RUN apt-get update && apt-get install --no-install-recommends -y libfontconfig1-dev

RUN R -e "cat(\"local(options(repos = c(CRAN = 'https://cloud.r-project.org')))\n\", file = R.home('etc/Rprofile.site'), append = TRUE)"

# install dependencies
RUN R -q -e "install.packages('remotes')" && \
    R -q -e "remotes::install_version('backports', version = '1.4.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('base64enc', version = '0.1-3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('bit', version = '4.0.5', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('brew', version = '1.0-8', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('brio', version = '1.1.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('cli', version = '3.6.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('clipr', version = '0.8.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('colorspace', version = '2.1-0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('commonmark', version = '1.9.0', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('cpp11', version = '0.4.6', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('crayon', version = '1.5.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('curl', version = '5.1.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('data.table', version = '1.14.8', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('DBI', version = '1.1.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('digest', version = '0.6.33', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('evaluate', version = '0.22', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('fansi', version = '1.0.5', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('farver', version = '2.1.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('fastmap', version = '1.1.1', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('fs', version = '1.6.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('generics', version = '0.1.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('glue', version = '1.6.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('httpcode', version = '0.3.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('isoband', version = '0.2.7', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('jsonlite', version = '1.8.7', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('KernSmooth', version = '2.23-22', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('labeling', version = '0.4.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('lattice', version = '0.21-9', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('lazyeval', version = '0.2.2', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('magrittr', version = '2.0.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('MASS', version = '7.3-60', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('mime', version = '0.12', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('pkgconfig', version = '2.0.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('png', version = '0.1-8', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('praise', version = '1.0.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('prettyunits', version = '1.2.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('proxy', version = '0.4-27', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('ps', version = '1.7.5', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('R6', version = '2.5.1', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('rappdirs', version = '0.3.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('RColorBrewer', version = '1.1-3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('Rcpp', version = '1.0.11', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rematch', version = '2.0.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rlang', version = '1.1.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rprojroot', version = '2.0.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rstudioapi', version = '0.15.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('sourcetools', version = '0.1.7-1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('stringi', version = '1.7.12', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('sys', version = '3.4.2', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('utf8', version = '1.2.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('uuid', version = '1.1-1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('viridisLite', version = '0.4.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('whisker', version = '0.4.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('withr', version = '2.5.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('wk', version = '0.8.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('xfun', version = '0.40', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('xml2', version = '1.3.5', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('xtable', version = '1.8-4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('yaml', version = '2.3.7', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('askpass', version = '1.2.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('aws.ec2metadata', version = '0.2.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('aws.signature', version = '0.6.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('bit64', version = '4.0.5', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('cachem', version = '1.0.8', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('class', version = '7.3-22', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('config', version = '0.3.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('desc', version = '1.4.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('diffobj', version = '0.3.5', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('ellipsis', version = '0.3.2', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('highr', version = '0.10', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('later', version = '1.3.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('lifecycle', version = '1.0.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('Matrix', version = '1.6-1.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('munsell', version = '0.5.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('nlme', version = '3.1-163', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('plyr', version = '1.8.9', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('processx', version = '3.8.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rex', version = '1.2.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('s2', version = '1.1.4', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('sp', version = '2.1-1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('systemfonts', version = '1.0.5', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('terra', version = '1.7-55', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('timechange', version = '0.2.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('tinytex', version = '0.48', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('triebeard', version = '0.4.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('tzdb', version = '0.4.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('units', version = '0.8-4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('callr', version = '3.7.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('e1071', version = '1.7-13', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('gtable', version = '0.3.4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('htmltools', version = '0.5.6.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('knitr', version = '1.44', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('lubridate', version = '1.9.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('memoise', version = '2.0.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('mgcv', version = '1.9-0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('openssl', version = '2.1.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('promises', version = '1.2.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('raster', version = '3.6-26', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('scales', version = '1.2.1', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('textshaping', version = '0.3.7', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('urltools', version = '1.7.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('vctrs', version = '0.6.4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('blob', version = '1.2.4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('classInt', version = '0.4-10', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('conflicted', version = '1.2.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('crosstalk', version = '1.2.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('crul', version = '1.4.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('fontawesome', version = '0.5.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('gridExtra', version = '2.3', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('hms', version = '1.1.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('httpuv', version = '1.6.11', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('httr', version = '1.4.7', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('ids', version = '1.0.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('jquerylib', version = '0.1.4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('leaflet.providers', version = '2.0.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('pillar', version = '1.9.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('pkgbuild', version = '1.4.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('purrr', version = '1.0.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('ragg', version = '1.2.6', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('sass', version = '0.4.7', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('stringr', version = '1.5.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('tidyselect', version = '1.2.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('webshot', version = '0.5.5', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('aws.s3', version = '0.3.21', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('bslib', version = '0.5.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('covr', version = '3.6.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('gargle', version = '1.5.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('pkgload', version = '1.3.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('progress', version = '1.2.2', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('reshape2', version = '1.4.4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('selectr', version = '0.4-2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('sf', version = '1.0-14', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('tibble', version = '3.2.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('cellranger', version = '1.1.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('dplyr', version = '1.1.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('forcats', version = '1.0.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('ggplot2', version = '3.4.4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('googledrive', version = '2.1.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('oai', version = '0.4.0', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('rematch2', version = '2.1.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rmarkdown', version = '2.25', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('roxygen2', version = '7.2.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rvest', version = '1.0.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('shiny', version = '1.7.5.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('vroom', version = '1.6.4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('dtplyr', version = '1.3.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('googlesheets4', version = '1.1.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('htmlwidgets', version = '1.6.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('readr', version = '2.1.4', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('readxl', version = '1.4.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('reprex', version = '2.0.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('rgbif', version = '3.7.8', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('shinycssloaders', version = '1.0.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('shinyjs', version = '2.1.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('tidyr', version = '1.3.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('viridis', version = '0.6.4', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('waldo', version = '0.5.1', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('broom', version = '1.0.5', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('dbplyr', version = '2.3.4', upgrade = FALSE)"
RUN R -q -e "remotes::install_version('DT', version = '0.30', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('haven', version = '2.5.3', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('leaflet', version = '2.2.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('plotly', version = '4.10.2', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('testthat', version = '3.2.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('leaflet.extras', version = '1.0.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('modelr', version = '0.1.11', upgrade = FALSE)" && \
    R -q -e "remotes::install_version('tidyverse', version = '2.0.0', upgrade = FALSE)" && \
    R -q -e "remotes::install_github('trias-project/trias')"

# Specific version INBOtheme
RUN R -q -e "remotes::install_github('inbo/INBOtheme@v0.5.9')"

# To prevent errors 
## when opening ggplot via linux docker
## when installing phantomjs via webshot
RUN apt-get update && apt-get install --no-install-recommends -y \
    libxt-dev \
    wget \ 
    bzip2 
    
# To download leaflet maps from within the app
## Attention: do not install phantomjs directly, will not work then!
RUN R -e "webshot::install_phantomjs()"

# install local package(s)
RUN mkdir -p /tmp
WORKDIR /tmp
COPY alienSpecies /tmp/alienSpecies
#RUN R -q -e "roxygen2::roxygenize('alienSpecies'); #install.packages(pkgbuild::build('alienSpecies'), repos = NULL, dependencies = FALSE)"
RUN R -e "remotes::install_local('/tmp/alienSpecies')"

#packamon.run-shiny-start
RUN R -e "cat(\"local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))\n\", file = R.home('etc/Rprofile.site'), append = TRUE)"
EXPOSE 3838
CMD ["R", "-e", "alienSpecies::runShiny()"]

