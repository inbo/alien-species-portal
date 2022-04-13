FROM openanalytics/r-shiny

MAINTAINER machteld.varewyck@openanalytics.eu

# system libraries of general use
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libgdal-dev

# install dependencies of alienSpecies that are not latest on CRAN
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/')"
RUN R -e "options(warn=2); remotes::install_github('trias-project/trias')"
RUN R -e "remotes::install_version('plotly', version = '4.9.2.1', repos = 'http://cran.us.r-project.org')"
RUN R -e "remotes::install_github('inbo/INBOtheme')"
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/DT/DT_0.12.tar.gz', repos = NULL, type = 'source')"

# install CRAN dependencies of alienSpecies
RUN R -e "install.packages(c('openxlsx', 'shinycssloaders', 'reshape2'), repos='https://cloud.r-project.org/')"


# For downloading maps
# Attention: do not install phantomjs directly, will not work then!
# RUN R -e "webshot::install_phantomjs()"


# copy the app to the image by installing package (need latest version!!)
ENV latestApp alienSpecies_0.0.1.tar.gz
COPY $latestApp /root/
RUN R CMD INSTALL /root/$latestApp
RUN rm /root/$latestApp


# set host
COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e alienSpecies::runShiny()"]
