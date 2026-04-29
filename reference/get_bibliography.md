# Get the PRIO-GRID bibliography

The bibliography contains all citations and further references that are
included in
[pgsources](http://prio-data.github.io/priogrid/reference/pgsources.md).
You can also get a subset of references, and you can get the
[RefManageR::RefManageR-package](https://docs.ropensci.org/RefManageR/reference/RefManageR-package.html)
bibliography, or references as biblatex.

## Usage

``` r
get_bibliography(keys = NULL, as_biblatex = FALSE)
```

## Arguments

- keys:

  A vector of strings, bibkeys found in
  [pgsources](http://prio-data.github.io/priogrid/reference/pgsources.md).

- as_biblatex:

  Set to true if you want results in biblatex instead of an R object.

## Value

BibEntry or BibTex

## Examples

``` r
get_bibliography(keys = "schvitzMappingInternationalSystem2022")
#> [1] G. Schvitz, L. Girardin, S. Rüegger, et al. “Mapping the
#> International System, 1886-2019: The CShapes 2.0 Dataset”. In: _Journal
#> of Conflict Resolution_ 66.1 (Jan. 2022), pp. 144-161. ISSN: 0022-0027.
#> DOI: 10.1177/00220027211013563. (Visited on 11/22/2024).
```
