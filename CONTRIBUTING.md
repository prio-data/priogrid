---
title: "Contributing to PRIO-GRID v3"
output: 
  github_document:
    toc: true
    toc_depth: 3
    html_preview: false
editor_options: 
  markdown: 
    wrap: 72
---

Thank you for showing interest in contributing to PRIO-GRID! This guide
will help you get started with contributing to our open-source
geospatial platform for conflict research and political science.

PRIO-GRID is a unified spatial data structure that provides
cell-specific information on political, economic, demographic,
environmental, and conflict variables organized in a consistent gridded
framework.

### Quick overview of the contributing process

1.  **Fork** the [PRIO-GRID
    repository](https://github.com/prio-data/priogrid)
2.  **Install** the package `renv::install("prio-data/priogrid")`
3.  **Set up** your development environment (see Development Setup)
4.  **Make** your changes following our guidelines
5.  **Submit** a pull request

### What is PRIO-GRID? 

PRIO-GRID is an R package that provides:

-   Easy data access:

    -   Download processed data through our R package

    -   Get data that is already harmonized across sources and time
        periods

-   Raw data management:

    -   Download and work with original source data through our package
        functions

    -   Maintain full transparency about data transformation

-   Automated citations:

    -   Generate proper citations for the data sources you use

    -   Get both required citation and suggested methodological
        references

-   Customizable calculations through `pgoptions`:

    -   Configure spatial and temporal resolution, extent, and scope

-   Extensible functions:

    -   Modify existing functions

    -   Create new functions for variables or data sources

### Ways to contribute 

#### Suggest new data sources or variables 

The easiest way to contribute is by identifying valuable data sources or
suggesting new variables that would benefit the users of PRIO-GRID.

Use our [issue form](https://github.com/prio-data/priogrid/issues) to:

-   Propose new data sources

-   Request new variables

-   Report bugs or issues

#### Add data sources 

1.  Add citations to `inst/REFERENCES.bib`
2.  Add source metadata to `data_raw/sources.csv`
3.  Run `data_raw/pgsources.R` to update `data/pgsources.rds`
4.  Create `R/data_yoursource.R` with a `read_yoursource()` function

#### Create new variables 

Write `gen_variablename()` functions that:

-   Can run without arguments (use sensible defaults)

-   Return a `terra::SpatRaster` object

-   Follow our naming conventions

### Development setup 

#### Prerequisites 

-   R (version 4.0 or higher is recommended)

-   Git and GitHub account for version control

-   Sufficient disk space

-   Required system libraries for spatial packages

**R package dependencies:** The spatial R packages requires careful
setup. You will need working installations of the packages `terra`,
`sf`, `exactextractr`, which depend on system libraries like `GDAL`,
`GEOS`, `PROJ`

#### Installation process

``` R
# Install the package 
install.packages("renv")
renv::install("prio-data/priogrid")

# Set data directory 
library(priogrid)
pgoptions$set_rawfolder("/path/to/data/directory")
```

#### Troubleshooting installation 

-   Missing packages: Use `renv::install("package_name")`

-   System path issues: Add paths to `~/.Renviron` or use `Sys.setenv()`

-   macOS/Homebrew issues: See installation guides for `sf` and `terra`
    packages

### Data sources and licensing 

Licensing determines how we can integrate and distribute the data in
PRIO-GRID

We accept data sources that are:

-   Open access (not behind paywalls)

-   Sub-national (spatial resolution below country-level)

-   Covering more than one country

-   Relevant to conflict research and political science

-   From 1850 onwards (focus on contemporary data)

**API sources:** For frequently updated data, we are interested in
building functions that access APIs rather than storing static copies.
These will not appear in our final database, but provide dynamic access
to current information.

#### License compatibility 

| License type | Status | Notes |
|------------------------|------------------------|------------------------|
| Permissive (MIT, Unlicense) | 🟩 Supported | Full distribution and modification allowed |
| Attribution (CC-BY 4.0) | 🟩 Supported | Citations required (we provide these) |
| Non-commercial (CC-BY-NC 4.0) | 🟩 Supported | Commercial use restrictions noted |
| No derivatives (CC-BY-ND 4.0) | 🟥 Not supported | Cannot include modified data in the database |
| Copyleft (CC-BY-SA 4.0, GPL 3.0) | 🟨 Metadata only | Functions to access data, but no direct inclusion |
| All rights reserved | 🟨 Functions only | Download and read functions only, no redistribution |

### Code contribution guidelines 

Our coding standards ensure that contributions work well together and
remain maintainable over time.

**Source-specific files:** Each data source gets its own file following
the pattern `R/data_sourcename.R`. This keeps related functionality
together and makes it easy to track which functions work with which data
source.

#### Function naming conventions

-   Read functions: `read_sourcename()`

    -   Every data source must have a read function that handles the
        basic task of loading raw data into R

-   Variable functions: `gen_variablename()`

    -   Functions to create research variables that appear in the
        PRIO-GRID database

-   Utility functions: `sourcename_utilityfunction()`

    -   Helper function that support data processing but don't create
        final variables

#### Function requirements

`read_` functions:

``` R
read_yoursource <- function() {
    # Use get_pgfile(source_name = "source",
    #                source_version = "version",
    #                id = "source_id") 
    # Return sf data.frame or terra SpatRaster
    # Minimal processing - preserve original data structure
}
```

`gen_` functions:

``` R
gen_variablename <- function() {
    # Must work without arguments (sensible defaults)
    # Return terra SpatRaster
    # Static data: layer name = variable name 
    # Time series: layer names = dates as character strings 
}
```

### Coding standards 

-   Namespace everything: Use `package::function()` format (except for
    priogrid functions)

-   Use renv: Install packages with `renv::install()`, update with
    `renv::status()`

-   Global options: Access via `pgoptions$get_youroption()`

-   Testing: Write unit tests using `testthat`

-   Documentation: Use `roxygen2` format

### Variable registration 

When adding `gen_` functions:

1.  Add variable to `data_raw/variables.csv`
2.  Run `data_raw/pgvariables.R` (set `overwrite = TRUE` temporarily)
3.  Verify changes with `git diff`

### Citations and references

-   Use [Zotero](https://zotero.org) with [Better
    Bibtext](https://retorque.re/zotero-better-bibtex/) addon

-   Citation key format: `"auth.lower + shorttitle(3,3) + year"`

-   Add to function documentation:
    `#' @references \insertRef{bibkey}{priogrid}`

-   Include both data source citations and usage references

### Pull request process

1.  Fork the repository and create a feature branch
2.  Install dependencies: Run `renv::install()` in your fork
3.  Make changes following our guidelines
4.  Test thoroughly:
    -   Restart R and run devtools::load_all()

    -   Test your functions

    -   Run `devtools::document()` to update documentation

    -   Run existing tests with `devtools::test()`
5.  Stay updated: Merge upstream/master regularly
6.  Create pull request with clear description

#### Testing your changes 

``` R
# Restart your R session 
devtools::load_all()

# Test your functions
your_function()

# Update documentation 
devtools::document()

# Run tests 
devtools::test()

# Check documentation 
?your_function 
```

### Package dependencies 

We use these key packages:

-   Spatial: `sf` (vector), `terra` (raster), `exactextractr` (accurate
    raster-vector conversion)

-   Data: `tidyverse` (including `lubridate` for dates)

-   Development: `devtools`, `usethis`, `testthat`, `assertthat`

-   Performance: `memoise` (for caching expensive functions)

-   Citations: `RefManageR`, `Rdpack`

*We try to minimize new dependencies. Discuss additions in your pull
request.*

### FAQ

#### Q: Can I use PRIO-GRID at different spatial/temporal resolutions?

**A:** Yes! Use `pgoptions` to set custom extent, projection, spatial
resolution, temporal resolution, and scope. Our server provides 0.5x0.5
yearly resolution globally (1850-present). We are planning to provide
monthly resolution from 1990.

#### Q: My data source is already in PRIO-GRID but needs updates 

**A:** We welcome input from data providers. Please open an issue or
contact us directly to discuss corrections or updates to ensure proper
representation.

#### Q: How do I handle large datasets or frequent updates? 

**A:** For frequently updates data, consider creating functions that
access APIs rather than including static downloads. We can include these
in our metadata while providing dynamic access methods.

#### Q: What if my data has complex licensing requirements? 

**A:** Contact us to discuss specific licensing situations. We'll work
with you to find an appropriate way to include your data while
respecting license terms.

------------------------------------------------------------------------

**Need help?** Open an issue on our [GitHub
repository](https://github.com/prio-data/priogrid/issues) or review our
existing documentation and examples.

**Resources:**

-   [Happy Git with R](https://happygitwithr.com): Git and GitHub guide
    for R users

-   [R Packages book](https://r-pkgs.org): Comprehensive guide to R
    package development

-   [Choose a License](https://choosealicense.com): License comparison
    tool

-   [Creative
    Commons](https://creativecomms.org/share-your-work/cclicenses/): CC
    license details \
