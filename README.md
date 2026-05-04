# PRIOGRID

> [!NOTE]
> PRIOGRID v.3.0.1 is an unstable Alpha release. We will be releasing a Beta version shortly.

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

## Installation

Install PRIOGRID from GitHub using `remotes` or `renv`:

```r
install.packages("renv")
renv::install("prio-data/priogrid")
```

### Optional R Packages

`terra`, `sf`, `exactextractr`, and `arrow` are listed in `Suggests` and are **not installed automatically**. They are only required for specific functionality (working with rasters) and will be requested the first time you use a function that needs them.

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

Download the official release and read it into R:

```r
download_priogrid()

pg_static     <- read_pg_static()
pg_timevarying <- read_pg_timevarying()
```

Browse available variables:

```r
pgvariables
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
