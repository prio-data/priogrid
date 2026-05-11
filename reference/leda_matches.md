# SIDE-LEDA Match Table

Preprocessed annual links between SIDE ethnic groups and their annual
political relevance categories derived from LEDA/EPR matching.

## Usage

``` r
leda_matches
```

## Format

### `leda_matches`

A data frame with 5 columns:

- iso3c:

  Three-letter country code.

- side_group:

  SIDE ethnic group label.

- year:

  Calendar year.

- link_year_source:

  Whether the SIDE-EPR year match was observed or carried by the
  upstream workflow.

- status3:

  Collapsed EPR status category: included, excluded, or irrelevant.

## Details

This dataset is bundled with the package so the expensive LEDA matching
workflow does not have to run every time SIDE is aggregated to
PRIO-GRID.
