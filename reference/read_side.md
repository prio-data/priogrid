# Read SIDE Ethnic Map Metadata and Local Files

Downloads or locates the raw SIDE ethnic raster files registered in
PRIO-GRID metadata, then joins those files to SIDE's map metadata and
the bundled annual `leda_matches` table.

## Usage

``` r
read_side()
```

## Value

A list with two elements:

- `meta`: a tibble of SIDE ethnic map metadata with local raster paths

- `matches`: the bundled annual SIDE-LEDA match table

## Details

The function uses the SIDE source entry in
[pgsources](http://prio-data.github.io/priogrid/reference/pgsources.md)
to retrieve the raw ASCII rasters from the ETH SIDE server when they are
missing locally. It returns the local file paths together with map
metadata from `sidedata::side_metadata()`, restricted to the ethnic
marker.

## References

Müller-Crepon C, Hunziker P (2018). “New Spatial Data on Ethnicity:
Introducing SIDE.” *Journal of Peace Research*, **55**(5), 687-698.
[doi:10.1177/0022343318764254](https://doi.org/10.1177/0022343318764254)
. <http://journals.sagepub.com/doi/full/10.1177/0022343318764254>.
