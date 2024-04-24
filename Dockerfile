FROM rocker/r-ver:4.3.2

MAINTAINER Machteld Varewyck machteld.varewyck@openanalytics.eu

RUN apt-get update && apt-get install --no-install-recommends -y \
    libgdal-dev \
    libudunits2-dev \ 
    libharfbuzz-dev \
    libfribidi-dev \
    libproj22 \
    libgeos3.10.2 libgeos-c1v5  \
    libcurl4-openssl-dev \
    curl \
    imagemagick \
    lbzip2 \
    pandoc \
    libmagick++-dev \
    libssl-dev \
    texlive-latex-extra \
    lmodern \
    && rm -rf /var/lib/apt/lists/*

# Use the remotes package instead of devtools as it is much lighter
RUN R -q -e "install.packages('remotes')"

RUN R -q -e "options(warn = 2); remotes::install_cran(c('shiny', 'data.table', 'dplyr', 'DT', 'ggplot2', 'ggspatial', 'htmlwidgets', 'httr', 'jsonlite', 'leaflet', 'leaflet.extras', 'plotly', 'reshape2', 'rgbif', 'sf', 'shinyjs', 'terra', 'testthat', 'tidyr', 'tidyverse', 'webshot', 'xtable'))"

# Specific data format + access to S3 on UAT
RUN R -q -e "options(warn = 2); Sys.setenv(LIBARROW_MINIMAL=FALSE); remotes::install_cran(c('arrow', 'config', 'aws.ec2metadata', 'aws.s3', 'aws.signature'))"

# INBO packages & dependencies
RUN R -q -e "options(warn = 2); remotes::install_cran('tibble')"
RUN R -q -e "options(warn = 2); remotes::install_github(c('inbo/INBOtheme@v0.5.9', 'gavinsimpson/gratia@v0.9.0', 'trias-project/trias', 'daattali/shinycssloaders'))"

# For the rmarkdown pdf report
RUN R -q -e "install.packages('oaStyle', repos = c(rdepot = 'https://repos.openanalytics.eu/repo/public', getOption('repos')))"
#RUN R -e "tinytex::install_tinytex()" 
#ENV PATH="/root/bin:${PATH}" 
#RUN R -e "tinytex::tlmgr_install(pkgs = c('fancyhdr', 'sectsty', 'titling', 'grffile'))" 

# For downloading the maps
# Attention: do not install phantomjs directly, will not work then!
RUN R -q -e "options(warn = 2); remotes::install_cran('webshot'); webshot::install_phantomjs()"

# Git sha
ARG GIT_SHA
ENV GIT_SHA=$GIT_SHA


# Install the package without the source files ending up in the Docker image
COPY alienSpecies /tmp/package
RUN R -q -e "options(warn = 2); remotes::install_local('/tmp/package', dependencies=FALSE)"

# set host
COPY Rprofile.site /usr/local/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e alienSpecies::runShiny()"]