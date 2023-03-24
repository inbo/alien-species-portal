#include packamon.disclaimer

#include packamon.from

#include packamon.system-dependencies

# Missing for tidyverse install

RUN apt-get update && apt-get install --no-install-recommends -y libfontconfig1-dev

#include packamon.r-repos

#include packamon.r-dependencies


# To prevent errors 
## when opening ggplot via linux docker
## when installing phantomjs via webshot
RUN apt-get update && apt-get install --no-install-recommends -y \
    libxt-dev \
    wget
    
# To download leaflet maps from within the app
## Attention: do not install phantomjs directly, will not work then!
RUN R -e "webshot::install_phantomjs()"

#include packamon.local-r-dependencies

#include packamon.runtime-settings
