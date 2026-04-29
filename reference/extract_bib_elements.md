# Helper function to parse bibliography elements based on a source

In
[pgsources](http://prio-data.github.io/priogrid/reference/pgsources.md),
citations are semi-colon separated. This function splits these into
individual keys and get the bibliography element from each key.

## Usage

``` r
extract_bib_elements(citation_liststr, bib_element = "author", ...)
```

## Arguments

- citation_liststr:

  A semi-colon separated list of bibliography keys from
  [pgsources](http://prio-data.github.io/priogrid/reference/pgsources.md)

- bib_element:

  Supports author, journal, year, and title.

- ...:

## Value

list with BibEntry-element classes or vectors with character strings

## Examples

``` r
extract_bib_elements(pgsources$citation_keys[1])
#> [[1]]
#> [1] "Christopher D. Elvidge" "Mikhail Zhizhin"        "Tilottama Ghosh"       
#> [4] "Feng-Chi Hsu"           "Jay Taneja"            
#> 
```
