# Robustly transform any raster to PRIO-GRID format

This function performs a comprehensive transformation of input rasters
to match the PRIO-GRID specification. It handles rasters with different
projections, extents, and resolutions through an intelligent workflow
that includes reprojection, cropping, aggregation, disaggregation, and
final resampling as needed.

## Usage

``` r
robust_transformation(
  r,
  agg_fun,
  disagg_method = "near",
  config = pg_current_config(),
  ...
)
```

## Arguments

- r:

  SpatRaster object to transform. Can have any projection, extent, or
  resolution.

- agg_fun:

  Character string or function for aggregating high-resolution data.
  Common options include "mean", "sum", "max", "min", "median", "modal".
  See
  [`aggregate`](https://rspatial.github.io/terra/reference/aggregate.html)
  for all options.

- disagg_method:

  Character string specifying disaggregation method for low-resolution
  data. Options are "near" (nearest neighbor, default), "bilinear", or
  "cubic". See
  [`disagg`](https://rspatial.github.io/terra/reference/disaggregate.html)
  for details.

- config:

  A `pg_config` object. Defaults to
  [`pg_current_config()`](http://prio-data.github.io/priogrid/reference/pg_current_config.md).

- ...:

  Additional arguments passed to
  [`aggregate`](https://rspatial.github.io/terra/reference/aggregate.html).
  Useful for controlling aggregation behavior (e.g., na.rm = TRUE).

- cores:

  Integer specifying number of CPU cores to use for aggregation
  operations. Defaults to 1. Higher values can speed up processing of
  large datasets.

## Value

SpatRaster object conforming to PRIO-GRID specifications:

- CRS: As specified in `config` (default: EPSG:4326)

- Extent: As specified in `config` (default: global extent)

- Resolution: Calculated from `nrow`/`ncol` in `config`

- Grid alignment: Exactly matched to PRIO-GRID cell boundaries

## Details

The transformation workflow automatically detects and handles:

- **Projection differences**: Reprojects to PRIO-GRID CRS if needed

- **Extent mismatches**: Crops input if larger than PRIO-GRID extent

- **Resolution differences**:

  - Aggregates high-resolution data using specified aggregation function

  - Disaggregates low-resolution data using specified method

- **Final alignment**: Uses nearest-neighbor resampling for exact grid
  matching

All intermediate files are written to temporary storage to handle large
datasets efficiently and are automatically cleaned up after processing.

## Performance Notes

For large datasets, consider:

- Increasing `cores` parameter for faster aggregation

- Ensuring adequate disk space in the raw data folder for temporary
  files

- Pre-cropping input data to region of interest before transformation

## See also

[`prio_blank_grid`](http://prio-data.github.io/priogrid/reference/prio_blank_grid.md)
for creating empty PRIO-GRID templates,
[`aggregate`](https://rspatial.github.io/terra/reference/aggregate.html),
[`disagg`](https://rspatial.github.io/terra/reference/disaggregate.html),
[`resample`](https://rspatial.github.io/terra/reference/resample.html)

## Examples

``` r
if (FALSE) { # \dontrun{
r <- read_ghsl_population_grid()
res <- robust_transformation(r, agg_fun = "sum")
} # }
```
