# Read and Process Global SPEI database data

While this data can be downloaded from the Global SPEI database, it is
only open for download through manual operations on the web-page. The
code for reproducing SPEIbase is open, however. We use this here to
process CRU PET and PRE data. The input for SPEIbase 2.11 is CRU v.4.09.

## Usage

``` r
read_speibase(interval = 6, config = pg_current_config())
```

## Arguments

- interval:

  Integer. The month interval to calculate SPEI over. E.g., if the
  interval is 6, then monthly precipitation and potential
  evapotranspiration are aggregated over the last 6 months, then the
  SPEI (anomaly) is calculated. This is done for each month.

## Details

We use R-code from Beguería S. (2017) SPEIbase: R code used in
generating the SPEI global database, doi:10.5281/zenodo.834462.

## References

Beguería S, Latorre B, Reig F, Vicente-Serrano SM (2024). “SPEIbase
v.2.11.” Beguería S, Vicente-Serrano SM, Reig F, Latorre B (2014).
“Standardized Precipitation Evapotranspiration Index (SPEI) Revisited:
Parameter Fitting, Evapotranspiration Models, Tools, Datasets and
Drought Monitoring.” *International Journal of Climatology*, **34**(10),
3001–3023. ISSN 1097-0088.
[doi:10.1002/joc.3887](https://doi.org/10.1002/joc.3887) .
[2018-08-29](http://prio-data.github.io/priogrid/reference/2018-08-29).

## See also

[`get_pgfile`](http://prio-data.github.io/priogrid/reference/get_pgfile.md)
for file retrieval functionality,
[`pg_date_intervals`](http://prio-data.github.io/priogrid/reference/pg_date_intervals.md)
for PRIO-GRID temporal boundaries
