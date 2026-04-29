# Read and Process CRU TS Near Surface Temperature Data

This function reads CRU TS (Climate Research Unit Time Series) v4.09
near surface temperature climate data files from local storage, unzips
them if necessary, loads them as raster stacks, and filters the data to
match PRIO-GRID temporal coverage. The function also support reading
supporting statistics.

## Usage

``` r
read_cru_tmp(variable = "tmp", config = pg_current_config())
```

## Arguments

- variable:

  A character string specifying the climate variable to extract from the
  raster stack. Must match the prefix of variable names in the CRU
  dataset. Available options include "tmp" (temperature), "stn" (station
  count), "mae" (mean absolute error), and "maea". Default is "tmp".

## Value

A `SpatRaster` object (from the terra package) with the following
characteristics:

- **Spatial Resolution**: 0.5° x 0.5° global grid

- **Spatial Extent**: Global coverage (-180° to 180° longitude, -90° to
  90° latitude)

- **Coordinate System**: WGS84 geographic (lon/lat)

- **Temporal Coverage**: Filtered to match PRIO-GRID date intervals

- **Layer Names**: Set to corresponding timestamps

- **Units**: Variable-specific (e.g., degrees Celsius for temperature)

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
# Load temperature data (default)
temp_data <- read_cru()

# Load station count data
station_data <- read_cru(variable = "stn")

# Load mean absolute error data
error_data <- read_cru(variable = "mae")

} # }
```
