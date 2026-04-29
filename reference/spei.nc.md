# Compute SPEI from very large arrayed data sets in netCDF format.

Compute SPEI from very large arrayed data sets in netCDF format.

## Usage

``` r
spei.nc(
  sca,
  inPre,
  outFile,
  inEtp = NA,
  title = NA,
  comment = NA,
  version = NA,
  inMask = NA,
  block = 18,
  tlapse = NA
)
```

## Arguments

- sca:

  Integer. The time scale of the SPEI.

- inPre:

  Character vector. Path to the input precipitation netCDF file.

- outFile:

  Character vector. Path of the output netCDF file.

- inEtp:

  Character vector. Path to the input evapotranspiration netCDF file.

- title:

  Character vector. Title of the dataset.

- comment:

  Character vector. A comment.

- version:

  Character vector. Dataset version number.

- inMask:

  Character vector. Path to a netCDF mask file.

- block:

  Integer. Number of latitude blocks to be processed at the same time.
  Must be an integer dividend of 360.

## Value

Computes the SPEI time series and stores it in outFile following the
same data structure of inPre.

## References

Code from Beguería S. (2017) SPEIbase: R code used in generating the
SPEI global database, doi:10.5281/zenodo.834462.
