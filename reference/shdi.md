# Generate PRIO-GRID Compatible SHDI Variables

This function processes GlobalDataLab Subnational Human Development
Index (SHDI) data and produces a PRIO-GRID–aligned raster for a selected
SHDI-related variable. It harmonizes national and subnational
observations, resolves missing or empty geometries using geoBoundaries
crosswalks, and rasterizes polygon-level values onto the PRIO-GRID using
area-weighted aggregation.

## Usage

``` r
shdi(variable = "shdi", config = pg_current_config())
```

## Arguments

- variable:

  A character string specifying which SHDI-related variable to generate.
  Supported values include:

  - `"shdi"` – Subnational Human Development Index

  - `"msch"` – Mean years of schooling

  - `"esch"` – Expected years of schooling

  - `"lifexp"` – Life expectancy at birth

  - `"gnic"` – Gross national income per capita

  Default is `"shdi"`.

## Value

A `SpatRaster` object (from the terra package) aligned to the PRIO-GRID,
containing the selected SHDI variable aggregated to grid cells using
area-weighted means.

## Details

The function may take some time to run due to spatial operations.

## References

GlobalDataLab (2019). “The Subnational Human Development Database.”
[2024-10-23](http://prio-data.github.io/priogrid/reference/2024-10-23).

## See also

[`read_shdi`](http://prio-data.github.io/priogrid/reference/read_shdi.md),
[`read_shdi_shapefile`](http://prio-data.github.io/priogrid/reference/read_shdi_shapefile.md),
[`read_geoboundaries`](http://prio-data.github.io/priogrid/reference/read_geoboundaries.md),
[`prio_blank_grid`](http://prio-data.github.io/priogrid/reference/prio_blank_grid.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate PRIO-GRID SHDI
shdi_pg <- shdi()

# Generate PRIO-GRID life expectancy
lifexp_pg <- shdi(variable = "lifexp")
} # }
```
