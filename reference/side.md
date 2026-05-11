# Generate PRIO-GRID Ethnic Settlement Shares by Political Status from SIDE

Aggregates annual SIDE ethnic settlement shares to PRIO-GRID cells for
one political status category at a time. Each cell contains the summed
share of the local population belonging to ethnic groups with the
selected status in that year.

## Usage

``` r
side(
  status = c("excluded", "included", "irrelevant"),
  config = pg_current_config()
)
```

## Arguments

- status:

  Character. Political status category to extract. One of `"excluded"`
  (groups excluded from central government power), `"included"` (groups
  represented in government), or `"irrelevant"` (groups not relevant to
  central power).

- config:

  A `pg_config` object. Defaults to
  [`pg_current_config`](http://prio-data.github.io/priogrid/reference/pg_current_config.md).

## Value

A `SpatRaster` object (terra package) conforming to the PRIO-GRID
specification defined in `config`. Each cell contains the summed
population share of ethnic groups with the requested political status.
The raster contains:

- Values: Shares ranging from 0 (none of the cell population has this
  status) to 1 (all of the cell population has this status); values can
  exceed 1 where group territories overlap

- Layers: One layer per date interval in `pg_date_intervals(config)`

- Layer names: Dates in YYYY-MM-DD format

## Details

SIDE (Spatially Interpolated Data on Ethnicity) provides raster surfaces
representing the settlement patterns of ethnic groups. This function
links those surfaces to political status via the bundled `leda_matches`
table, which connects SIDE groups to the EPR (Ethnic Power Relations)
typology.

The processing pipeline:

- Filters `leda_matches` to the requested status category

- Averages each group's surface across all available SIDE map rounds

- For each year in the config date range, sums the averaged surfaces of
  all groups with that status

- Reprojects and aggregates the native SIDE raster to PRIO-GRID
  resolution using mean aggregation

- Returns one layer per date in `pg_date_intervals(config)`

## References

Müller-Crepon C, Hunziker P (2018). “New Spatial Data on Ethnicity:
Introducing SIDE.” *Journal of Peace Research*, **55**(5), 687-698.
[doi:10.1177/0022343318764254](https://doi.org/10.1177/0022343318764254)
. <http://journals.sagepub.com/doi/full/10.1177/0022343318764254>.

## See also

[`read_side`](http://prio-data.github.io/priogrid/reference/read_side.md)
for reading the raw SIDE surfaces and match table,
[`gen_side_excluded`](http://prio-data.github.io/priogrid/reference/gen_side_excluded.md),
[`gen_side_included`](http://prio-data.github.io/priogrid/reference/gen_side_included.md),
[`gen_side_irrelevant`](http://prio-data.github.io/priogrid/reference/gen_side_irrelevant.md)
for convenience wrappers,
[`pg_date_intervals`](http://prio-data.github.io/priogrid/reference/pg_date_intervals.md)
for PRIO-GRID temporal coverage

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate excluded population shares at default PRIO-GRID resolution
excluded <- side("excluded")
print(excluded)

# Plot excluded share for a specific year
terra::plot(excluded[["2010-12-31"]], main = "Excluded ethnic population share 2010")

# Use a custom config with a restricted date range
cfg <- pg_config(start_date = as.Date("2000-12-31"),
                 end_date   = as.Date("2010-12-31"),
                 temporal_resolution = "1 year")
excluded_2000s <- side("excluded", config = cfg)
} # }
```
