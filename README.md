# Lothian-IBDR

Clustering of faecal calprotectin and CRP profiles obtained from the Lothian IBD
Registry (LIBDR), a population cohort of patients with IBD receiving care from
the [NHS Lothian health board](https://www.nhslothian.scot/). This code was used
to conduct analyses described in _Analysis for Large-scale clustering of
longitudinal faecal calprotectin and C-reactive protein profiles in inflammatory
bowel disease_ by Constantine et al. If you re-use any this code for your own 
analyses, we would be very grateful if you cited our paper. 

## Getting Started

The analysis requires access to the igmm/cvallejo-predicct datastore. As the
data originated from unconsented patient data, we are currently unable to share
these datasets with external entities. 

This repository comes with its own R package, found in [libdr](libdr),
which provides documented versions of custom functions used in the analysis.
Please open this directory in Rstudio as a project and run
`devtools::install("./libdr")`  (or the Rstudio tooling) to install the
`{libdr}` package required for the analyses. 

The code expects `smb://cmvm.datastore.ed.ac.uk/igmm/` to be mounted to
`/Volumes/igmm/`. On MacOS, this can be achieved from Finder by pressing `cmd+k`
(or Go->Connect to Server from the task bar) and entering the above address
which will automatically map to `/Volumes/igmm/`. 

The pipeline is currently set-up to use [Quarto](https://quarto.org) v1.4.551,
the "next generation" replacement for Rmarkdown. The installer for this specific
version can be downloaded via
[this link for macOS](https://github.com/quarto-dev/quarto-cli/releases/download/v1.4.551/quarto-1.4.551-macos.pkg).

The scripts used to generate the reports can be found in [source](source).
As the interim processed data file are already available on datastore, the
reports can be ran in any order. However, the following order is intended:

> The reports in [pre](source/pre/) should be ran first as
  this will generate files in `igmm/cvallejo-predicct/libdr/processed/` which
  are then used in [selection](source/selection/) to compare model performance 
  and select the most appriopiate models. [post](source/post/) contains
  all post hoc analyses which explore associations between baseline
  variables/advanced therapy prescrbing and cluster assignment. Faecal
  calprotectin and CRP clustering is also considered for subjects meeting the
  criteria for both biomarkers. 
  
Any rendered reports will be found in the `docs` directory. [scripts](scripts)
contains any miscellaneous scripts not used in the reports or passed to Eddie
(such as scripts which have been used to generate poster figures)
