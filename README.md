# PRIOGRID

> [!NOTE]
> PRIOGRID v.3.0.2 is a Beta version. Please report any issues and we will aim to fix them as soon as possible.

An R-package for collecting and standardizing open spatial data into a common grid format.

**Resources:**
- [R-package repository](https://github.com/prio-data/priogrid)
- [Documentation](https://prio-data.github.io/priogrid/)
- [Suggest data sources and variables, or report issues](https://github.com/prio-data/priogrid/issues)
- [Download PRIOGRID data as .zip](https://www.prio.org/data/40)

## What's New in PRIOGRID v.3.x

- **Better metadata handling** — Stores information about data licenses, citations, and download URLs. Automatically downloads data and handles local data with user-specified options.
- **R, not SQL** — More researchers know R, and the package leverages excellent spatial-data infrastructure with `sf`, `terra`, and `exactextractr`.
- **Flexible spatio-temporal configuration** — Change resolution, extent, and projection to test the modifiable areal unit problem or create tailored datasets (e.g., area-equal projections for polar regions).
- **PRIOGRID is a research tool, not just a dataset.**
- **Efficient, self-describing outputs** — Variables are distributed as Cloud-Optimized GeoTIFFs. The tabular tables (a static wide table and Hive-partitioned Parquet for time-varying data) are built locally from those GeoTIFFs on first read, after which `read_pg_timevarying()` pushes year/date/cell/variable filters down to Arrow before loading.

## Installation

Install PRIOGRID from GitHub using `remotes` or `renv`:

```r
install.packages("renv")
renv::install("prio-data/priogrid")
```

### Optional R Packages

`terra`, `sf`, and `exactextractr` are listed in `Suggests` and are **not installed automatically**. They are only required for spatial functionality (working with rasters, `extent`-based subsetting, and building the tabular tables from the downloaded GeoTIFFs on first read) and will be requested the first time you use a function that needs them.

### Troubleshooting Installation

`terra`, `sf`, and `exactextractr` depend on system-level geo-libraries. If installation of these packages fails, refer to their installation guides:

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

2. **Install CURL R-package from source:**
```r
install.packages("curl", type = "source")
```

## Getting Started

Set a local folder for PRIOGRID to store downloaded and processed data (this persists across R sessions):

```r
library(priogrid)

pg_set_rawfolder("/path/to/your/data/folder")
```

Browse available variables:

```r
pgvariables
```

Plot a variable (this will automatically download the required data to the folder you have set).

```r
plot_pgvariable("cru_tmp", "2010-12-31", extent = "Asia", add_borders = T)
```

Read in raw-data:

```r
df <- read_cshapes()
```

Download the official release and read it into R in tabular format:

```r
pg_static      <- read_pg_static()
pg_timevarying <- read_pg_timevarying()
```

The first `read_pg_*()` call builds the tabular tables from the downloaded GeoTIFFs (requires `terra`) and caches them; later reads use the cache and need only `arrow`.

Load only the rows and columns you need — filters push down to Arrow before anything is collected:

```r
pg_sub <- read_pg_timevarying(
  years     = 2010:2015,
  extent    = c(xmin = -20, xmax = 55, ymin = -35, ymax = 40),
  variables = "cru_tmp"
)
```

## Documentation

Full documentation is available in the package vignettes:

| Vignette | Description |
|---|---|
| [Getting Started](https://prio-data.github.io/priogrid/articles/getting-started.html) | Setup, downloading, and reading tabular data |
| [Accessing as Rasters](https://prio-data.github.io/priogrid/articles/working-with-rasters.html) | Working with individual variables using `terra` |
| [Citations and Bibliography](https://prio-data.github.io/priogrid/articles/citation.html) | Citing data providers in publications |
| [Custom Configurations](https://prio-data.github.io/priogrid/articles/custom-config.html) | Custom resolution, extent, projection, and time periods |
| [Understanding Metadata](https://prio-data.github.io/priogrid/articles/metadata.html) | Exploring `pgsources`, `pgvariables`, and `pgsearch()` |
| [Contributing](https://prio-data.github.io/priogrid/articles/contributing.html) | Adding new data sources and variables |

## Contributing

We welcome contributions. Report issues or suggest new data sources or variable ideas using our [Issue Tracker](https://github.com/prio-data/priogrid/issues/new/choose).

Please see our [contribution guidelines](CONTRIBUTING.md) for details on how you can contribute with code.
