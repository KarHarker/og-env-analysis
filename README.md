# og-env-analysis

# Detailed Assessment - Biodiversity

This repo documents detailed assessment of various sites and the overlapping areas of biodiversity and socio-economic layers. 

## Data

The analysis uses data from several sources. Many files are accessible through the B.C. Data Catalogue and do not need to be downloaded. Other files will need to be accessed from the NAG-GAT folder and added to the /data folder prior to running the analysis. 


## Targets Workflow

This project leverages the `targets` package, a pipeline toolkit for
data science projects in R. You can install `targets` from CRAN:

``` r
#install.packages("targets")
```

#Usage Run `targets::tar_make()` to run project. This will run all of
the analysis - no individual scripts are required.

### Required R packages

The packages used in this analysis are catalogued in `packages.R`. The
packages will be loaded automatically with `tar_make()` but some may
need to be installed prior to initiating the workflow.

