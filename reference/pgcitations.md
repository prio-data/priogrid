# Print citations for PRIO-GRID variables

Takes the variable names in a data.frame and links with citation using
[pgvariables](http://prio-data.github.io/priogrid/reference/pgvariables.md),
[pgsources](http://prio-data.github.io/priogrid/reference/pgsources.md),
and
[`get_bibliography()`](http://prio-data.github.io/priogrid/reference/get_bibliography.md).

## Usage

``` r
pgcitations(df, as_biblatex = FALSE)
```

## Arguments

- df:

  A data.frame or character vector containing variables listed in
  [pgvariables](http://prio-data.github.io/priogrid/reference/pgvariables.md)

- as_biblatex:

  Defaults to FALSE. Prints biblatex entry of citations.

## Value

Pure text or biblatex print of citation

## Examples

``` r
df <- data.frame("hilde_urban" = 1, "cru_tmp" = 1, "geoepr_reg_excluded" = 1)
pgcitations(df)
#> ## Package Citation
#> 
#> Tollefsen AF, Strand H, Buhaug H (2012). “PRIO-GRID: A Unified Spatial
#> Data Structure.” _Journal of Peace Research_, *49*(2), 363-374.
#> doi:10.1177/0022343311431287
#> <https://doi.org/10.1177/0022343311431287>.
#> 
#> Vestby J, Tollefsen AF, Helskog K, Benz G (2025). _priogrid: the
#> R-package_. R package version 3.0.1,
#> <http://prio-data.github.io/priogrid>.
#> 
#> 
#> Additionally, please cite these data providers:
#> (based on these variables: cru_tmp, geoepr_reg_excluded )
#> 
#> [1] I. Harris, T. J. Osborn, P. Jones, et al. “Version 4 of the CRU TS
#> Monthly High-Resolution Gridded Multivariate Climate Dataset”. In:
#> _Scientific Data_ 7.1 (Apr. 2020), p. 109. ISSN: 2052-4463. DOI:
#> 10.1038/s41597-020-0453-3.
#> 
#> [2] M. Vogt, N. Bormann, S. Rüegger, et al. “Integrating Data on
#> Ethnicity, Geography, and Conflict: The Ethnic Power Relations Data Set
#> Family.” In: _Journal of Conflict Resolution_ 59.7 (2015), pp. 1327-42.
#> (Visited on 10/21/2024).
#> 
#> [3] J. Wucherpfennig, N. B. Weidmann, L. Girardin, et al. “Politically
#> Relevant Ethnic Groups across Space and Time: Introducing the GeoEPR
#> Dataset”. In: _Conflict Management and Peace Science_ 28.5 (Nov. 2011),
#> pp. 423-437. ISSN: 0738-8942, 1549-9219. DOI: 10.1177/0738894210393217.
#> (Visited on 10/21/2024).
```
