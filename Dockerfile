FROM rocker/r-ver:4.3.2

LABEL maintainer="Machteld Varewyck machteld.varewyck@openanalytics.eu"

RUN apt-get update && apt-get install --no-install-recommends -y \
    libgdal-dev \
    libudunits2-dev \ 
    libharfbuzz-dev \
    libfribidi-dev \
    libproj22 \
    libgeos3.10.2 libgeos-c1v5  \
    libcurl4-openssl-dev \
		libpoppler-cpp-dev \
    curl \
    imagemagick \
    lbzip2 \
    pandoc \
    libmagick++-dev \
    libssl-dev \
		libxml2-dev \
		fonts-crosextra-carlito \
    lmodern \
		gnupg \
    wget \
		libgit2-dev && \
    wget https://downloads.vivaldi.com/stable/vivaldi-stable_6.8.3381.55-1_$(dpkg --print-architecture).deb && \
    apt-get install --no-install-recommends -y ./vivaldi-stable*.deb && \
    rm -rf /var/lib/apt/lists/*
    
    
# Use the remotes package instead of devtools as it is much lighter
RUN R -q -e "install.packages('remotes')"

RUN R -q -e "options(warn = 2); remotes::install_cran(c('fastmap', 'later', 'DT', 'magrittr', 'R6', 'Rcpp', 'rlang'))"
RUN R -q -e "options(warn = 2); install.packages('https://cran.r-project.org/src/contrib/Archive/promises/promises_1.3.2.tar.gz', repos = NULL, type = 'source');"
RUN R -q -e "options(warn = 2); remotes::install_cran(c('data.table', 'dplyr', 'DT', 'ggplot2', 'ggspatial', 'htmlwidgets', 'httr', 'jsonlite', 'leaflet', 'leaflet.extras', 'leaflet.extras2', 'leaflegend', 'plotly', 'reshape2', 'rgbif', 'sf', 'shinyscreenshot', 'shinyjs', 'shiny.i18n', 'chromote', 'terra', 'testthat', 'tidyr',  'webshot', 'xtable', 'xml2'))"
RUN R -q -e "options(warn = 2); install.packages('https://cran.r-project.org/src/contrib/Archive/shiny/shiny_1.10.0.tar.gz', repos = NULL, type = 'source');"

# Specific data format + access to S3 on UAT
RUN R -q -e "options(warn = 2); Sys.setenv(LIBARROW_MINIMAL=FALSE); remotes::install_cran(c('arrow', 'config', 'aws.ec2metadata', 'aws.signature'), Ncpus=1)"
RUN R -q -e "options(warn = 2); Sys.setenv(LIBARROW_MINIMAL=FALSE); remotes::install_version('aws.s3', version = '0.3.22', repos = 'https://rforge.net')"

# INBO packages & dependencies
RUN R -q -e "options(warn = 2); remotes::install_cran(c('tibble'))"
RUN R -q -e "options(warn = 2); remotes::install_github(c('inbo/INBOtheme@v0.5.9', 'gavinsimpson/gratia@v0.9.0', 'trias-project/trias', 'daattali/shinycssloaders'))"

RUN apt-get update && apt-get install -y wget && \
    wget https://github.com/jgm/pandoc/releases/download/3.2/pandoc-3.2-1-arm64.deb && \
    dpkg -i pandoc-3.2-1-arm64.deb && \
    rm pandoc-3.2-1-arm64.deb && \
    rm -rf /var/lib/apt/lists/*
RUN R -q -e "webshot::install_phantomjs();"

# For the rmarkdown pdf report
RUN R -e "options(warn = 2); install.packages('lintr', repos='https://cloud.r-project.org')"
RUN R -q -e "options(warn = 2); remotes::install_cran(c('bookdown', 'gert', 'pdftools', 'devtools', 'qrcode', 'gh', 'hunspell', 'pkgdown', 'rcmdcheck', 'sessioninfo', 'pingr', 'codemetar'))"
RUN R -q -e "install.packages('tinytex')"
RUN R -e "options(warn = 0); tinytex::install_tinytex(force = TRUE, repository = 'https://ftp.gwdg.de/pub/ctan/systems/texlive/tlnet')"

# Add TinyTeX binaries to PATH
ENV PATH="${PATH}:/root/.TinyTeX/bin/x86_64-linux"
RUN apt-get update \
  && apt-get install -y  --no-install-recommends \
    ghostscript \
  && Rscript -e 'tinytex::tlmgr_install(c("babel-dutch", "babel-english", "babel-french", "beamer", "beamerswitch", "booktabs", "carlisle", "colortbl", "datetime", "dvips", "emptypage", "environ", "epstopdf", "eso-pic", "eurosym", "extsizes", "fancyhdr", "fancyvrb", "fmtcount", "float", "fontspec", "footmisc", "framed", "helvetic", "hyphen-dutch", "hyphen-french", "inconsolata", "lastpage", "lipsum", "makecell", "marginnote", "mdframed", "ms", "multirow", "parskip", "pdflscape", "pdfpages", "pdftexcmds", "placeins", "needspace", "tabu", "tex", "textpos", "threeparttable", "threeparttablex", "titlesec", "times", "tocloft", "translator", "trimspaces", "ulem", "upquote", "wrapfig", "xcolor", "xstring", "zref", "draftwatermark"))'

# Fonts for LaTeX (Calibri replacement + Inconsolata)
RUN apt-get update && apt-get install -y \
    fonts-crosextra-carlito \
    fonts-inconsolata \
    && rm -rf /var/lib/apt/lists/* \
    && fc-cache -fv

RUN mkdir -p ${HOME}/.fonts \
  && wget https://www.wfonts.com/download/data/2014/12/12/calibri/calibri.zip \
  && unzip calibri.zip -d ${HOME}/.fonts \
  && rm calibri.zip \
  \
  # REMOVE WOFF files so XeLaTeX cannot pick them
  && rm -f ${HOME}/.fonts/*.woff \
  \
  && wget -O ${HOME}/.fonts/Inconsolatazi4-Regular.otf \
       https://ftp.gwdg.de/pub/ctan/fonts/inconsolata/opentype/Inconsolatazi4-Regular.otf \
  && wget -O ${HOME}/.fonts/Inconsolatazi4-Bold.otf \
       https://ftp.gwdg.de/pub/ctan/fonts/inconsolata/opentype/Inconsolatazi4-Bold.otf \
  \
  && fc-cache -fv
	
RUN R -q -e "options(warn = 2); install.packages(c('checklist', 'INBOmd'), repos = 'https://inbo.r-universe.dev', dependencies = FALSE)"
RUN R -e "tinytex::tlmgr_conf(c('auxtrees', 'add', system.file('local_tex', package = 'INBOmd')))" 

		
# Configure browser for using webshot2
ENV CHROMOTE_CHROME=/usr/bin/vivaldi
ENV OPENSSL_CONF=/dev/null

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
