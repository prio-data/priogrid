# PRIOGRID <img src="man/figures/priogridlogo.png" align="right" />

## Installation

Install PRIOGRID from the Github repository using a package that can draw from Github, such as `remotes` or `renv`.

```
install.packages(“renv”)
renv::install(“prio-data/priogrid”)
```

PRIOGRID depends on terra, sf, and exactextractr for spatial operations. When installation fails,
it can often be traced to one of these packages as they rely on shared geo-libraries in your system.
See install instructions on https://github.com/rspatial/terra, https://github.com/r-spatial/sf/, and 
https://github.com/isciences/exactextractr.

If you have problems installing the package after following the above instructions, please file an issue.

## Setup
PRIOGRID uses settings that are stored locally on your computer and persists across sessions. 
To start using PRIOGRID, we recommend that you set a folder where PRIOGRID data (raw data from
data providers and transformed data) are stored on your computer.

You can set other options for PRIOGRID, such as spatial resolution, extent, and projection, and temporal resolution and extent.
These options will only affect the output you create yourself. We support a global 0.5x0.5 deg EPSG:4326 with 
1 year temporal resolution from 1850 until today (given that there is data). We recommend setting the date at the last day of
the temporal increment you want (year, quarter, month, week). E.g., setting the start date at 1850-12-31 instead of 1850-01-01.

```
pgoptions
pgoptions$set_rawfolder("/path/to/folder")
pgoptions$set_start_date(as.Date("1990-12-31"))
pg_dates()
pg_date_intervals()
prio_blank_grid()
```

## Basic Usage
To read data from PRIOGRID, you will first need to download it. This will download
PRIOGRID data using `download_priogrid()` to the folder specified using `pgoptions$set_rawfolder()`.
This will download the latest version of PRIOGRID to your local machine. `read_priogrid()` will then read
this file into memory.

```
download_priogrid()
df <- read_priogrid()
View(df)
```

## Advanced Usage
If you want to transform data to a user specified format, or use the original data, you will
first need to download it. We keep a list of sources in `pgsources`, and we have
a convenience function `pg_rawfiles()` that returns a dataset with one line per
file to download from each source. `download_pg_rawdata()` downloads these files
to the folder specified using `pgoptions$set_rawfolder()`.

```
View(pgsources)
files_to_download <- pg_rawfiles() |> dplyr::filter(source_name == "UCDP GED")
download_pg_rawdata(file_info = files_to_download)
```

We use `read_[source]()` to read a specific source into R. Large files are not read into memory,
but certain operations involve reading to memory. Please see documentation in [terra](https://github.com/rspatial/terra).

PRIOGRID contains source specific functions for transforming data and defining new variables. See "R/data_[source].R" for
code. Many of these source specific functions are exported to the PRIOGRID namespace. These functions commonly have arguments
that allows calculating more variables than what can be found in `priogrid()`.

```
r <- read_ghsl_population_grid()

ttime_max <- calc_traveltime(aggregation_function = "max")
```

By setting alternative `pgoptions`, you get different results using these functions.
This is limited by the resolution of the input data.

```
pgoptions$set_start_date(as.Date("2022-12-31"))
pgoptions$set_temporal_resolution("1 month")
r_monthly <- gen_cru_tmp()

pgoptions$set_temporal_resolution("1 quarter")
r_quarterly <- gen_cru_tmp()

pgoptions$set_ncol(36)
pgoptions$set_nrow(18)
pgoptions$set_extent(c(0,180, 0, 90))
prio_blank_grid() # beware that changing extent affects resolution
r_quarterly_low_res_upper_east <- gen_cru_tmp()
plot(r_quarterly_low_res_upper_east)
```

When changing spatial resolution, extent, or projection, the "PRIOGRID ID" is not 
the same as for the default resolution. These ids should only be used to match data
using the same system, and is primarily a way to store PRIO-GRID data on default settings
in a tabular format.

```
pgoptions$set_ncol(360)
pgoptions$set_nrow(180)
pgoptions$set_extent(c(-180,180, -90, 90))
plot(prio_blank_grid())
pgoptions$set_ncol(36)
pgoptions$set_nrow(18)
plot(prio_blank_grid())
```

## How to contribute
