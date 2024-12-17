# PRIO-GRID

PRIO-GRID version 3 is currently under development. Follow this space. 

Please see the legacy branch for the source code for PRIO-GRID v2, and https://grid.prio.org for data from version 2. The legacy3alpha contains code for a pilot to PRIO-GRID v3.

## Requirements

We rely on the simple-features and terra packages. Please see these packages for installation.

## Installation

```
install.packages("renv")
renv::install("prio-data/priogrid")

source("data_raw/pgsources.R")
source("data_raw/pgvariables.R")
source("data_raw/pgchecksum.R")
```

You might need to additionally follow install instructions on https://github.com/rspatial/terra and https://github.com/isciences/exactextractr (geos). If you are prompted with
a package that is missing, it will usually work just by renv::install("package_name"). If you in addition would file an issue on the package you needed to install, we would greatly
appreciate it.

## Useage
This is work in progress. However, here are some features we are working on.

```R
library(priogrid)

# downloading all data will require a lot of space (and increasing). set this to somewhere you have available space.
pgoptions$set_rawfolder("/path/to/where/you/want/files") 

# an ongoing collection of meta-data for data sources. Not all of these are (or will be) incorporated in PRIO-GRID.
pgsources

# How to download the raw-data.
files_to_download <- pg_rawfiles() |> head()
download_pg_rawdata(file_info = files_to_download) 

# How to read raw-data into R.
cshp <- read_cshp()

# How to calculate PRIO-GRID data
gwcode <- gen_cshapes_gwcode()

# Transform it to data.frame
gwcode |> rast_to_df(static = FALSE, varname = "gwcode")

# How to cite the data.
get_bibliography(keys = "schvitzMappingInternationalSystem2022")

# How to calculate all PRIO-GRID (takes a long time)
calculate_pgvariables() # Saves all rasters in your raw_folder/priogrid/version 
pg <- collate_pgdata()
pg$static |> head()
pg$non_static |> head()
```

We are planning on having a server automatically calculating and updating PRIO-GRID and serving the end-result on a web-server (for a few resolutions). However, anyone can re-calculate in their own
preferred resolution, etc. Having easy access to all stages in the data-process also makes it useful for researchers wanting to make their own accomodations.



