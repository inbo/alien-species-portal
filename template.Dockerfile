#include packamon.disclaimer

#include packamon.from

#include packamon.system-dependencies

#include packamon.r-repos

#include packamon.r-dependencies

RUN R -e "remotes::install_github('trias-project/trias')"
RUN R -e "remotes::install_github('inbo/INBOtheme')"

#include packamon.local-r-dependencies

#include packamon.runtime-settings

ARG ARCHIVE
COPY $ARCHIVE /tmp/$ARCHIVE
RUN R -e "install.packages('/tmp/$ARCHIVE', repos = NULL, dependencies = FALSE)"
RUN rm /tmp/$ARCHIVE
