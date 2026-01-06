# PRIOGRID

> [!NOTE]
> PRIOGRID v.3.0.1 is an unstable Alpha release. We will be releasing a Beta version shortly.

An R-package for collecting and standardizing open spatial data. 

## Installation

Install PRIOGRID from GitHub using `remotes` or `renv`:

```r
install.packages("renv")
renv::install("prio-data/priogrid")
```

### Troubleshooting Installation

PRIOGRID depends on several spatial libraries (`terra`, `sf`, and `exactextractr`) that require system-level geo-libraries. If installation fails, please refer to the installation guides for these dependencies:

- [terra installation guide](https://github.com/rspatial/terra)
- [sf installation guide](https://github.com/r-spatial/sf/)
- [exactextractr installation guide](https://github.com/isciences/exactextractr)

If you continue to experience issues after following these guides, please [file an issue](https://github.com/prio-data/priogrid/issues).

#### SSL Certificate Issues

If you encounter SSL certificate errors when downloading data, try:

1. **Install system certificates:**

**Mac (Homebrew users):**
```bash
   brew update
   brew install ca-certificates
```

**Linux (Ubuntu/Debian):**
```bash
   sudo apt-get update
   sudo apt-get install ca-certificates
```

2. **Install CURL R-package from source**
```r
   install.packages("curl", type = "source")
```

## Initial Setup

PRIOGRID stores settings locally that persist across R sessions. Before using the package, configure where PRIOGRID will store downloaded raw data and transformed datasets:

```r
# Set the folder for PRIOGRID data storage
pgoptions$set_rawfolder("/path/to/your/data/folder")

# Configure temporal settings (optional)
pgoptions$set_start_date(as.Date("1990-12-31"))
pgoptions$set_end_date(as.Date("2023-12-31"))

# View your configuration
pg_dates()
pg_date_intervals()
prio_blank_grid()
```

### Default Settings

- **Spatial resolution:** 0.5×0.5 degrees
- **Projection:** EPSG:4326 (WGS84)
- **Temporal resolution:** 1 year
- **Temporal extent:** 1850 to present (where data exists)

> **Note:** When setting dates, use the last day of your desired temporal increment. For example, use `1850-12-31` for yearly data instead of `1850-01-01`.

## Basic Usage

### Download and Read PRIOGRID Data

The simplest workflow involves downloading the complete PRIOGRID dataset and reading it into R:

```r
# Download the latest PRIOGRID dataset to your local folder
download_priogrid()

# Read the dataset into memory
df <- read_priogrid()

# Explore the data
View(df)
```

### Citing Data Sources

**Always cite the original data providers.** Most data licenses require attribution (e.g., CC-BY), and proper citation is fundamental to good research practice. 

When using PRIOGRID data with original variable names, retrieve all necessary citations with:

```r
pgcitations(names(df))
```

## Advanced Usage

### Working with Original Data Sources

For users who need to transform data to custom specifications or work with original source data:

```r
# View available data sources
View(pgsources)

# List all downloadable files
files_to_download <- pg_rawfiles()

# Download specific sources
ucdp_files <- pg_rawfiles() |> 
  dplyr::filter(source_name == "UCDP GED")
download_pg_rawdata(file_info = ucdp_files)
```

### Reading and Transforming Source Data

Each data source has dedicated functions for reading and transformation. Large files use memory-efficient processing via [terra](https://github.com/rspatial/terra):

```r
# Read population data
r <- read_ghsl_population_grid()

# Calculate travel time with custom aggregation
ttime_max <- calc_traveltime(aggregation_function = "max")
```

Source-specific transformation functions are documented in the corresponding `R/data_[source].R` files.

### Customizing Spatial and Temporal Resolution

PRIOGRID supports flexible spatial and temporal configurations:

#### Temporal Resolution Examples

```r
# Monthly data
pgoptions$set_start_date(as.Date("2022-12-31"))
pgoptions$set_temporal_resolution("1 month")
r_monthly <- gen_cru_tmp()

# Quarterly data
pgoptions$set_temporal_resolution("1 quarter")
r_quarterly <- gen_cru_tmp()
```

#### Spatial Resolution Examples

```r
# Lower resolution with custom extent
pgoptions$set_ncol(36)
pgoptions$set_nrow(18)
pgoptions$set_extent(c("xmin" = 0, "xmax" = 180, "ymin" =  0, "ymax" = 90))
r_low_res <- gen_cru_tmp()
plot(r_low_res)

# Compare different resolutions
pgoptions$set_ncol(360)
pgoptions$set_nrow(180)
pgoptions$set_extent(c("xmin" = -180, "xmax" = 180, "ymin" =  -90, "ymax" = 90))
plot(prio_blank_grid())  # High resolution

pgoptions$set_ncol(36)
pgoptions$set_nrow(18)
plot(prio_blank_grid())  # Low resolution
```

> **Important:** When changing spatial resolution, extent, or projection, PRIOGRID IDs will differ from the default configuration. These custom IDs should only be used within datasets sharing the same spatial configuration.

## Contributing

We welcome contributions. Report issues or suggest new data sources or variable ideas using our [Issue Tracker](https://github.com/prio-data/priogrid/issues/new/choose).

Please see our [contribution guidelines](CONTRIBUTING.md) for details on how you can contribute with code.
