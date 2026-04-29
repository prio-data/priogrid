# Read and Process CRU TS Potential Evapotranspiration Data

This function reads CRU TS (Climate Research Unit Time Series) v4.09
potential evapotranspiration (PET) data files from local storage, unzips
them if necessary, loads them as raster stacks (`SpatRaster` objects),
and filters the data to match PRIO-GRID temporal coverage. The function
also corrects broken or missing time stamps by reconstructing a monthly
sequence starting from January 1901, ensuring consistent temporal
alignment with PRIO-GRID.

## Usage

``` r
read_cru_pet(config = pg_current_config())
```

## Value

A `SpatRaster` object (from the terra package) with the following
characteristics:

- **Spatial Resolution**: 0.5° x 0.5° global grid

- **Spatial Extent**: Global coverage (-180° to 180° longitude, -90° to
  90° latitude)

- **Coordinate System**: WGS84 geographic (lon/lat)

- **Temporal Coverage**: Filtered to match PRIO-GRID date intervals

- **Layer Names**: Set to corresponding timestamps

- **Units**: Millimeters of evapotranspiration

## References

Harris I, Osborn TJ, Jones P, Lister D (2020). “Version 4 of the CRU TS
Monthly High-Resolution Gridded Multivariate Climate Dataset.”
*Scientific Data*, **7**(1), 109. ISSN 2052-4463.
[doi:10.1038/s41597-020-0453-3](https://doi.org/10.1038/s41597-020-0453-3)
.

## See also

[`get_pgfile`](http://prio-data.github.io/priogrid/reference/get_pgfile.md)
for file retrieval functionality,
[`pg_date_intervals`](http://prio-data.github.io/priogrid/reference/pg_date_intervals.md)
for PRIO-GRID temporal boundaries

## Examples

``` r
if (FALSE) { # \dontrun{
# Load monthly potential evapotranspiration data
pet_data <- read_cru_pet()
} # }
```
