# PRIO-GRID

PRIO-GRID is a system for standardizing GIS data (i.e. a data aggregator) for use in the social sciences. Please read https://grid.prio.org/#/codebook for more information. In version 3, the version that is under development here (the main branch), in addition to serve data in a standardized form through https://grid.prio.org, we are building an R-package that will not only serve as replication code for the data, but also as useful templates for accomodating new data, and functions to simplify the work of standardizing data to the cShapes/Gleditsch & Ward (http://nils.weidmann.ws/projects/cshapes.html) and gridded systems. The PRIO-GRID R package relies heavily on the Simple Features (https://r-spatial.github.io/sf/) and raster (https://rspatial.org/raster/pkg/index.html) packages. We also try to be tidy (https://www.tidyverse.org/).

Please see the legacy branch for the source code for PRIO-GRID v2, and https://grid.prio.org for data from version 2.

## Requirements

More to come here. Importantly, we will not be able to provide a function to build the input_folder containing all the rawdata, as some of the raw-data, although openly available with permissive licencing, requires a sign-in or similar to obtain. However, we will work towards simplifying the process of creating the input_folder, and documentation for how to build this will come.

System requirements:
```
sudo apt install libgdal-dev
sudo apt install libnetcdf-dev 
```


## Installation

Please be aware that not all dependencies are resolved yet, and the dependency list is quite long. Error messages during the installation will generally provide you with the information you need for how to proceed. This will be resolved in the final version.

```
devtools::install_github("prio-data/priogrid")
```



