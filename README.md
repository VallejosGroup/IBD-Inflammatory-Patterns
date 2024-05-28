# LIBDR

Analysis of the Lothian IBD Registry (LIBDR) Cohort. 

## Getting Started

The analysis requires access to the igmm/cvallejo-predicct datastore. One 
should  map `smb://cmvm.datastore.ed.ac.uk/igmm/` to `/Volumes/igmm/`. On MacOS,
this can be achieved from Finder by pressing `cmd+k` (or Go->Connect to Server
from the task bar) and entering the above address which will automatically map
to `/Volumes/igmm/`. 

The pipeline is currently set-up to use [Quarto](https://quarto.org) v1.4.554,
the "next generation" replacement for Rmarkdown. The installer for this specific
version can be downloaded via
[this link for macOS](https://github.com/quarto-dev/quarto-cli/releases/download/v1.4.554/quarto-1.4.554-macos.pkg).

This repository also comes with its own R package, found in [libdr](libdr),
which provides documented versions of custom functions used in the analysis.
Open this directory in Rstudio as a project and run `devtools::install("./libdr")` 
(or the Rstudio tooling) to install the `{libdr}` package required for the analyses. 

The scripts used to generate the reports can be found in [source](source).
As the interim processed data file are already available on datastore, the
reports can be ran in any order. However, the following order is intended:

> The reports in [data-cleaning](source/data-cleaning/) should be ran first as
  this will generate files in `igmm/cvallejo-predicct/libdr/processed/` which
  are then used  in [selection-both.qmd](source/selection/selection-both.qmd). `selection-both.qmd` prior to the model loading section generates the data
  files used in the Eddie scripts found in the [eddie directory](eddie) to fit
  the models. The model objects are then moved to
  `cvallejo-predicct/libdr/cache/` for use with the rest of
  `selection-both.qmd`.

Any rendered reports will be found in the `docs` directory. [scripts](scripts)
contains any miscellaneous scripts not used in the reports or passed to Eddie
(such as scripts which generate figures for posters)
