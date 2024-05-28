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
RUN install2.r \
    shiny \
    tidyverse \
    htmltools \
    remotes \
    renv \
    knitr \
    rmarkdown \
    quarto \
    pander \
    ComplexHeatmap \
    viridis \
    downlit \
    xml2 \
    plyr \
    lcmm \
    patchwork \
    ggalluvial \
    datefixR \
    lme4 \
    optimx \
    foreach \
    doParallel \
    gameR \
    qqplotr \
    reshape2 \
    mice
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
