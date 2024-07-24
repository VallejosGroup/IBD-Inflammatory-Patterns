FROM rocker/rstudio:4.4.0

LABEL "org.opencontainers.image.source"="https://github.com/VallejosGroup/Lothian-IBDR" \
    "org.opencontainers.image.authors"="Nathan Constantine-Cooke <nathan.constantine-cooke@ed.ac.uk>" \
    "org.opencontainers.image.base.name"="rocker/rstudio:4.4.0" \
    "org.opencontainers.image.description"="Docker image for the LIBDR LCMM analysis" \
    "org.opencontainers.image.vendor"="University of Edinburgh"

RUN apt-get update && apt-get install -y --no-install-recommends \
    pandoc \
    pandoc-citeproc \
    curl \
    gdebi-core \
    xorg \
    openbox \
    libxml2-dev \
    libfontconfig1-dev \
    zlib1g-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    cmake \
    && rm -rf /var/lib/apt/lists/*
RUN install2.r -n -1 \
    tidyverse \
    rmarkdown \
    knitr \
    tibble \
    doParallel
RUN install2.r -n -1\
    foreach \
    viridis \
    plyr \
    lcmm \
    gameR
RUN install2.r -n -1 \
    ggalluvial \
    DT \
    lme4 \
    optimx \
    ggdist
RUN install2.r -n -1 \
    kml \
    mice \
    pander \
    patchwork \
    qqplotr
RUN install2.r -n -1 \
    datefixR \
    stringr \
    lubridate \
    colorspace \
    reshape2
RUN install2.r -n -1 \
    downlit \
    xml2

# Install dependencies for installing bioconductor packages
RUN install2.r --error \
    --ncpus -1 \
    --repos https://ropensci.r-universe.dev --repos getOption \
    --skipinstalled \
    BiocManager littler

# Install packages from Bioconductor (will compile from source)
RUN /usr/local/lib/R/site-library/littler/examples/installBioc.r ComplexHeatmap

COPY libdr libdr
RUN install2.r \"libdr\"


ARG TARGETARCH
RUN curl -LO https://quarto.org/download/latest/quarto-linux-$TARGETARCH.deb
RUN gdebi --non-interactive quarto-linux-$TARGETARCH.deb

RUN mkdir analysis
COPY . analysis
RUN cp analysis/docker/render analysis
RUN chmod u+x analysis/render
RUN mkdir -p analysis/docs
RUN mkdir analysis/data
RUN mkdir analysis/src
WORKDIR /analysis
RUN rm -rf /tmp/downloaded_packages
CMD ["./render"]
