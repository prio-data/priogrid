Here we should settle on some principles and guidelines for how to write the code for PRIO-GRID v.3.0, and then outline the process we envisage.

The following guidelines is partially inspired by http://r-pkgs.had.co.nz/.

Code guidelines:
One file for each new variable:
    - data_raw/gen_[newvariable].R 
    - data_raw/[newvariable].rda
One file to add it all together:
    - R/combine_variables.R
    - data/priogrid_v3.rda
If larger datasets with multiple input variables are processed (e.g. transform a dataset from a from-to date from to a year, month form), this process should be in a separate file that results in a processed .rda file.
    - data_raw/tidy_[dataset].R
    - data_raw/cleaned_[dataset].rda
We use tidyverse, sf, and Velox in R.
We should note down versioning using sessionInfo() in R.
    - make comment at end of .R file after successful run.
Variable name conventions similar to earlier PRIO-GRID versions.
It is better to write code that documents itself, than to write comments.
    - Complex transformations should be put in a function with a proper function name.
    - Remember that most variables will be using the same transformation procedure. Important to recyle functions!
    - Functions that are used more than once should be placed in a separate files so it can be reused for multiple variables.
        + R/raster_functions.R
        + R/vector_functions.R
        + R/plot_maps.R
        + R/hdf5_functions.R
    - When writing functions, also write the documentation using Roxygen (see http://r-pkgs.had.co.nz/man.html). That way, we can automatically generate proper documentation.
Replication code and files should be placed in a Dropbox and possibly on GitHub after initial develpment phase is completed. It would be great if this could turn into an R package!

Process outline:
1. Generate PRIO-GRID template.
2. Load new data and make it fit into PRIO-GRID template.
3. Check data for missing, for correct matching, and other issues.
4. Check if data must be matched with other variables (e.g. gwcode).
5. Plot the new data and perform visual inspection.
6. Suggest cutpoints for a chloropleth map.
7. Write the file as rda with only pgid, (year, month), newvar.
8. Include the file in combine_variables.R (that converts the data to HDF5/NetCDF4).
9. Write documentation for newvar, including suggested citation, etc. Write this documentation using our markdown template (man/doc_[newvar].md).

New Variables:
- Annual NDVI (Check Fatih's product) - MODIS
- Annual Landcover (Fatih) - ESA
- Gridded population of the world (GPW) v.4
- Worldpop?
- Child Mortality (Africa only) + Vaccination?
- Gridded GDP and Human Development Index (Nature publication 2018)
- Nightlights combining old and new input data. "Toplights"
- Global human settlement layer
    - Soil moisture
- ViEWS outcomes?

What about projected data?

What about event data?
    - Facilitate merging.

Monthly products?