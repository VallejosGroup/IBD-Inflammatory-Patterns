FROM eddelbuettel/r2u:22.04

LABEL "org.opencontainers.image.source"="https://git.ecdf.ed.ac.uk/s1961592/libdr" \
    "org.opencontainers.image.authors"="Nathan Constantine-Cooke <nathan.constantine-cooke@ed.ac.uk>" \
    "org.opencontainers.image.base.name"="eddelbuettel/r2u:22.04" \
    "org.opencontainers.image.description"="Docker image for the LIBDR repository" \
    "org.opencontainers.image.vendor"="University of Edinburgh"

RUN apt-get update && apt-get install -y --no-install-recommends \
    pandoc \
    pandoc-citeproc \
    curl \
    gdebi-core \
    xorg \
    openbox \
    && rm -rf /var/lib/apt/lists/*
RUN install2.r -n -1 \
    tidyverse \
    rmarkdown \
    knitr \
    tibble \
    ComplexHeatmap \
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

COPY libdr libdr
RUN install2.r \"libdr\"

RUN curl -LO https://quarto.org/download/latest/quarto-linux-amd64.deb
RUN gdebi --non-interactive quarto-linux-amd64.deb

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
