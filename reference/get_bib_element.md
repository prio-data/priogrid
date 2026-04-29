# Extract bibliography element

Get bibliography information. Is used by
[`pgsearch()`](http://prio-data.github.io/priogrid/reference/pgsearch.md)
to search
[pgsources](http://prio-data.github.io/priogrid/reference/pgsources.md)
based on bibliography information.

## Usage

``` r
get_bib_element(key, element = "author", as_character = TRUE)
```

## Arguments

- key:

  A bibkey found in
  [pgsources](http://prio-data.github.io/priogrid/reference/pgsources.md)

- element:

  Supports author, journal, year, and title.

- as_character:

  Return the result as a string instead of a RefManageR class object.

## Value

BibEntry-element class or vector with character strings

## Examples

``` r
get_bib_element("schvitzMappingInternationalSystem2022", element = "author")
#> [1] "Guy Schvitz"               "Luc Girardin"             
#> [3] "Seraina Rüegger"           "Nils B. Weidmann"         
#> [5] "Lars-Erik Cederman"        "Kristian Skrede Gleditsch"
```
