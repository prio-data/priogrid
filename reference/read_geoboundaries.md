# Read geoBoundaries Administrative Boundary Data

This function reads administrative boundary shapefiles from the
geoBoundaries global database (version 5.0.0). The data are unzipped to
a temporary directory and returned as an `sf` object.

## Usage

``` r
read_geoboundaries()
```

## Value

An `sf` object containing global administrative boundary geometries from
geoBoundaries.

## References

Runfola D, Anderson A, Baier H, Crittenden M, Dowker E, Fuhrig S,
Goodman S, Grimsley G, Layko R, Melville G, Mulder M, Oberman R,
Panganiban J, Peck A, Seitz L, Shea S, Slevin H, Youngerman R, Hobbs L
(2020). “geoBoundaries: A Global Database of Political Administrative
Boundaries.” *PLOS ONE*, **15**(4), e0231866. ISSN 1932-6203.
[doi:10.1371/journal.pone.0231866](https://doi.org/10.1371/journal.pone.0231866)
.

## See also

[`get_pgfile`](http://prio-data.github.io/priogrid/reference/get_pgfile.md)
for file retrieval,
[`st_read`](https://r-spatial.github.io/sf/reference/st_read.html) for
spatial data input

## Examples

``` r
if (FALSE) { # \dontrun{
gb <- read_geoboundaries()
} # }
```
