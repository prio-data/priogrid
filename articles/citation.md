# Citations and Bibliography

## Why Attribution Matters

PRIOGRID aggregates data from many third-party providers. Most sources
are licensed under Creative Commons (CC-BY) or similar terms that
**require attribution**. Beyond legal compliance, citing data providers
gives credit where it is due and helps readers trace the provenance of
your results.

PRIOGRID makes this easy: citation metadata is embedded in the package
and linked to each variable.

## Citing Variables in Your Dataset

If your dataset uses PRIOGRID variables with their original names,
[`pgcitations()`](http://prio-data.github.io/priogrid/reference/pgcitations.md)
retrieves all required citations in one call:

``` r
variables <- c("hilda_urban", "cru_tmp")
pgcitations(variables)
#> ## Package Citation
#> 
#> Tollefsen AF, Strand H, Buhaug H (2012). "PRIO-GRID: A Unified Spatial
#> Data Structure." _Journal of Peace Research_, *49*(2), 363-374.
#> doi:10.1177/0022343311431287
#> <https://doi.org/10.1177/0022343311431287>.
#> 
#> Vestby J, Tollefsen AF, Helskog K, Benz G (2025). _priogrid: the
#> R-package_. R package version 3.0.1,
#> <http://prio-data.github.io/priogrid>.
#> 
#> 
#> Additionally, please cite these data providers:
#> (based on these variables: hilda_urban, cru_tmp )
#> [1] I. Harris, T. J. Osborn, P. Jones, et al. "Version 4 of the CRU TS
#> Monthly High-Resolution Gridded Multivariate Climate Dataset". In:
#> _Scientific Data_ 7.1 (Apr. 2020), p. 109. ISSN: 2052-4463. DOI:
#> 10.1038/s41597-020-0453-3.
#> 
#> [2] K. Winkler, R. Fuchs, M. D. A. Rounsevell, et al. _HILDA+ Global
#> Land Use Change between 1960 and 2019_. 2020. DOI:
#> 10.1594/PANGAEA.921846. (Visited on 05/26/2025).
```

You can also pass a data frame directly —
[`pgcitations()`](http://prio-data.github.io/priogrid/reference/pgcitations.md)
reads the column names:

``` r
pgcitations(pg_static)        # data.frame: uses column names
pgcitations(pg_timevarying)   # same
```

### BibLaTeX Export

For LaTeX documents, export as BibLaTeX entries:

``` r
pgcitations(c("hilda_urban", "cru_tmp"), as_biblatex = TRUE)
#> @Article{,
#>   title = {PRIO-GRID: A Unified Spatial Data Structure},
#>   author = {Andreas Forø Tollefsen and Håvard Strand and Halvard Buhaug},
#>   year = {2012},
#>   journal = {Journal of Peace Research},
#>   volume = {49},
#>   number = {2},
#>   pages = {363--374},
#>   doi = {10.1177/0022343311431287},
#> }
#> 
#> @Manual{,
#>   title = {priogrid: the R-package},
#>   author = {Jonas Vestby and Andreas Forø Tollefsen and Kristine Helskog and Garret Benz},
#>   year = {2025},
#>   note = {R package version 3.0.1},
#>   url = {http://prio-data.github.io/priogrid},
#> }
#> @Misc{winklerHILDAGlobalLand2020,
#>   title = {{{HILDA}}+ {{Global Land Use Change}} between 1960 and 2019},
#>   author = {Karina Winkler and Richard Fuchs and Mark D. A. Rounsevell and Martin Herold},
#>   year = {2020},
#>   publisher = {PANGAEA},
#>   doi = {10.1594/PANGAEA.921846},
#>   urldate = {2025-05-26},
#>   abstract = {Winkler, Karina; Fuchs, Richard; Rounsevell, Mark D A; Herold, Martin (2020): HILDA+ Global Land Use Change between 1960 and 2019 [dataset]. PANGAEA, https://doi.org/10.1594/PANGAEA.921846},
#>   copyright = {info:eu-repo/semantics/openAccess},
#>   langid = {english},
#> }
#> 
#> @Article{harrisVersion4CRU2020,
#>   title = {Version 4 of the {{CRU TS}} Monthly High-Resolution Gridded Multivariate Climate Dataset},
#>   author = {Ian Harris and Timothy J. Osborn and Phil Jones and David Lister},
#>   year = {2020},
#>   month = {apr},
#>   journal = {Scientific Data},
#>   volume = {7},
#>   number = {1},
#>   pages = {109},
#>   issn = {2052-4463},
#>   doi = {10.1038/s41597-020-0453-3},
#>   abstract = {CRU TS (Climatic Research Unit gridded Time Series) is a widely used climate dataset on a 0.5{$^\circ$} latitude by 0.5{$^\circ$} longitude grid over all land domains of the world except Antarctica. It is derived by the interpolation of monthly climate anomalies from extensive networks of weather station observations. Here we describe the construction of a major new version, CRU TS v4. It is updated to span 1901--2018 by the inclusion of additional station observations, and it will be updated annually. The interpolation process has been changed to use angular-distance weighting (ADW), and the production of secondary variables has been revised to better suit this approach. This implementation of ADW provides improved traceability between each gridded value and the input observations, and allows more informative diagnostics that dataset users can utilise to assess how dataset quality might vary geographically.},
#> }
```

Copy the output into your `.bib` file or paste it directly into an
Overleaf project.

## How Citation Lookup Works

The chain from variable to citation is:

    pgvariables$name → pgvariables$source_ids → pgsources$citation_keys → REFERENCES.bib

1.  `pgvariables` maps variable names to source UUIDs.
2.  `pgsources` maps source UUIDs to BibTeX keys (`citation_keys`
    column).
3.  `inst/REFERENCES.bib` holds the full bibliography (~200+ entries).

You can inspect this manually:

``` r
# Find the source IDs for a variable
pgvariables[pgvariables$name == "cru_tmp", ]
#> # A tibble: 1 × 3
#>   name    static source_ids                          
#>   <chr>   <lgl>  <chr>                               
#> 1 cru_tmp FALSE  ac037134-3567-49d9-a3ba-64f37c1ee698

# Look up citation keys in pgsources
source_id <- pgvariables[pgvariables$name == "cru_tmp", "source_ids"]
pgsources[pgsources$id == source_id, c("source_name", "citation_keys")]
#> # A tibble: 0 × 2
#> # ℹ 2 variables: source_name <chr>, citation_keys <chr>
```

## Working with Bibliography Entries Directly

### Get a bibliography entry by key

[`get_bibliography()`](http://prio-data.github.io/priogrid/reference/get_bibliography.md)
returns a
[`RefManageR::BibEntry`](https://docs.ropensci.org/RefManageR/reference/BibEntry.html)
object for one or more keys:

``` r
get_bibliography("harrisVersion4CRU2020")
#> [1] I. Harris, T. J. Osborn, P. Jones, et al. "Version 4 of the CRU TS
#> Monthly High-Resolution Gridded Multivariate Climate Dataset". In:
#> _Scientific Data_ 7.1 (Apr. 2020), p. 109. ISSN: 2052-4463. DOI:
#> 10.1038/s41597-020-0453-3.
```

Pass `as_biblatex = TRUE` for raw BibLaTeX text:

``` r
get_bibliography("harrisVersion4CRU2020", as_biblatex = TRUE)
#> @Article{harrisVersion4CRU2020,
#>   title = {Version 4 of the {{CRU TS}} Monthly High-Resolution Gridded Multivariate Climate Dataset},
#>   author = {Ian Harris and Timothy J. Osborn and Phil Jones and David Lister},
#>   year = {2020},
#>   month = {apr},
#>   journal = {Scientific Data},
#>   volume = {7},
#>   number = {1},
#>   pages = {109},
#>   issn = {2052-4463},
#>   doi = {10.1038/s41597-020-0453-3},
#>   abstract = {CRU TS (Climatic Research Unit gridded Time Series) is a widely used climate dataset on a 0.5{$^\circ$} latitude by 0.5{$^\circ$} longitude grid over all land domains of the world except Antarctica. It is derived by the interpolation of monthly climate anomalies from extensive networks of weather station observations. Here we describe the construction of a major new version, CRU TS v4. It is updated to span 1901--2018 by the inclusion of additional station observations, and it will be updated annually. The interpolation process has been changed to use angular-distance weighting (ADW), and the production of secondary variables has been revised to better suit this approach. This implementation of ADW provides improved traceability between each gridded value and the input observations, and allows more informative diagnostics that dataset users can utilise to assess how dataset quality might vary geographically.},
#> }
```

### Extract specific fields

[`get_bib_element()`](http://prio-data.github.io/priogrid/reference/get_bib_element.md)
pulls out a single field (author, title, journal, or year):

``` r
get_bib_element("harrisVersion4CRU2020", element = "author")
#> [1] "Ian Harris"        "Timothy J. Osborn" "Phil Jones"       
#> [4] "David Lister"
get_bib_element("harrisVersion4CRU2020", element = "year")
#> [1] "2020"
get_bib_element("harrisVersion4CRU2020", element = "title")
#> [1] "Version 4 of the {{CRU TS}} Monthly High-Resolution Gridded Multivariate Climate Dataset"
```

### Parse semicolon-separated keys from pgsources

Source entries can have multiple citations separated by `"; "`.
[`extract_bib_elements()`](http://prio-data.github.io/priogrid/reference/extract_bib_elements.md)
handles this:

``` r
# Get all citation keys for the first source
first_source_keys <- pgsources$citation_keys[1]
first_source_keys
#> [1] "elvidgeAnnualTimeSeries2021"

extract_bib_elements(first_source_keys, bib_element = "author")
#> [[1]]
#> [1] "Christopher D. Elvidge" "Mikhail Zhizhin"        "Tilottama Ghosh"       
#> [4] "Feng-Chi Hsu"           "Jay Taneja"
```

## Citing the PRIOGRID Package Itself

[`pgcitations()`](http://prio-data.github.io/priogrid/reference/pgcitations.md)
always prints the package citation first. To get it directly:

``` r
citation("priogrid")
#> To cite package 'priogrid' in publications use:
#> 
#>   Tollefsen AF, Strand H, Buhaug H (2012). "PRIO-GRID: A Unified
#>   Spatial Data Structure." _Journal of Peace Research_, *49*(2),
#>   363-374. doi:10.1177/0022343311431287
#>   <https://doi.org/10.1177/0022343311431287>.
#> 
#>   Vestby J, Tollefsen AF, Helskog K, Benz G (2025). _priogrid: the
#>   R-package_. R package version 3.0.1,
#>   <http://prio-data.github.io/priogrid>.
#> 
#> To see these entries in BibTeX format, use 'print(<citation>,
#> bibtex=TRUE)', 'toBibtex(.)', or set
#> 'options(citation.bibtex.max=999)'.
```

## Summary

| Function                                     | Purpose                                    |
|----------------------------------------------|--------------------------------------------|
| `pgcitations(vars)`                          | Print all citations for a set of variables |
| `pgcitations(vars, as_biblatex = TRUE)`      | Export as BibLaTeX                         |
| `get_bibliography(keys)`                     | Get BibEntry objects by key                |
| `get_bibliography(keys, as_biblatex = TRUE)` | Get raw BibLaTeX text                      |
| `get_bib_element(key, element)`              | Extract author / title / year / journal    |
| `extract_bib_elements(keys_str)`             | Parse semicolon-separated key strings      |
