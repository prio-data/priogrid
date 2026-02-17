Contributing to PRIO-GRID v3
================

- [Table of Contents](#table-of-contents)
- [Quick overview of the contributing
  process](#quick-overview-of-the-contributing-process)
- [What is PRIO-GRID?](#what-is-prio-grid)
- [Prerequisites: Git, GitHub, and R Setup](#prerequisites)
  - [Install Git](#install-git)
  - [Create a GitHub Account](#create-a-github-account)
  - [Configure Git with RStudio](#configure-git-with-rstudio)
  - [Configure Git Identity](#configure-git-identity)
  - [Generate SSH Key](#generate-ssh-key)
  - [Install devtools](#install-devtools)
  - [Required system libraries](#required-system-libraries)
- [Development setup](#development-setup)
  - [Prerequisites](#prerequisites)
  - [Fork the Repository on GitHub](#fork-the-repository-on-github)
  - [Clone Your Fork to Your
    Computer](#clone-your-fork-to-your-computer)
  - [Install R Package Dependencies](#install-r-package-dependencies)
  - [Set Up Your Data Directory](#set-up-your-data-directory)
  - [Verify Setup](#verify-setup)
- [Contribution Workflow Overview](#contribution-workflow)
- [Ways to contribute](#ways-to-contribute)
- [Adding Data Sources](#adding-data-sources)
  - [Data sources and licensing](#data-sources-and-licensing)
- [Code contribution guidelines](#code-contributions-guidelines)
- [Coding standards](#coding-standards)
  - [Variable registration](#variable-registration)
  - [Citations and references](#citations-and-references)
- [Testing your changes](#testing-your-changes)
  - [Package dependencies](#package-dependencies)
- [Submitting a Pull Request](#submitting-a-pull-request)
  - [Create a Feature Branch](#create-a-feature-branch)
  - [Commit Your Changes](#commit-your-changes)
  - [Stay Updated](#stay-updated)
  - [Push to Your Fork](#push-to-your-fork)
  - [Create Pull Request on GitHub](#create-pull-request-on-github)
  - [FAQ](#faq)

Thank you for showing interest in contributing to PRIO-GRID! This guide
will help you get started with contributing to our open-source
geospatial platform for conflict research and political science.

PRIO-GRID is a unified spatial data structure that provides
cell-specific information on political, economic, demographic,
environmental, and conflict variables organized in a consistent gridded
framework.

## Table of Contents

1.  [What is PRIO-GRID?](#what-is-prio-grid)
2.  [Prerequisites: Git, GitHub, and R Setup](#prerequisites)
3.  [Development Environment Setup](#development-setup)
4.  [Contribution Workflow Overview](#contribution-workflow)
5.  [Ways to Contribute](#ways-to-contribute)
6.  [Adding Data Sources](#adding-data-sources)
7.  [Code contribution guidelines](#code-contributions-guidelines)
8.  [Coding Standards](#coding-standards)
9.  [Testing Your Changes](#testing-your-changes)
10. [Submitting a Pull Request](#submitting-a-pull-request)
11. [FAQ](#faq)
12. [Resources](#resources)

## Quick overview of the contributing process

7.  [Creating New Variables](#creating-new-variables)

8.  [Coding Standards](#coding-standards)

9.  [Testing Your Changes](#testing-your-changes)

10. [Documentation and Citations](#documentation-and-citations)

11. [Submitting a Pull Request](#submitting-a-pull-request)

12. [FAQ](#faq)

13. [Resources](#resources)

14. **Fork** the [PRIO-GRID
    repository](https://github.com/prio-data/priogrid)

15. **Install** the package `renv::install("prio-data/priogrid")`

16. **Set up** your development environment (see Development Setup)

17. **Make** your changes following our guidelines

18. **Submit** a pull request

## What is PRIO-GRID?

PRIO-GRID is an R package that provides:

- Easy data access:

  - Download processed data through our R package

  - Get data that is already harmonized across sources and time periods

- Raw data management:

  - Download and work with original source data through our package
    functions

  - Maintain full transparency about data transformation

- Automated citations:

  - Generate proper citations for the data sources you use

  - Get both required citation and suggested methodological references

- Customizable calculations through `pgoptions`:

  - Configure spatial and temporal resolution, extent, and scope

- Extensible functions:

  - Modify existing functions

  - Create new functions for variables or data sources

## Prerequisites: Git, GitHub, and R Setup

Before you can contribute, you need to set up Git and GitHub integration
with RStudio.

### Install Git

Git is version control software that tracks changes to your code.

- **Windows**: Download and install [Git for
  Windows](https://git-scm.com/download/win). This includes Git Bash,
  which provides a Unix-like command line interface.
- **macOS**: Git is usually pre-installed. Open Terminal and type
  `git --version`. If not installed, you will be prompted to install it.
- **Linux**: Use your package manager.

### Create a GitHub Account

If you don’t have one, sign up at [github.com](https://github.com).

### Configure Git with RStudio

1.  Open RStudio
2.  Go to **Tools \> Global Options \> Git/SVN**
3.  **Git executable**: Click **Browse** and locate your Git executable:
    - Windows: `C:\Program Files\Git\bin\git.exe`
    - macOS: `/usr/bin/git` or `/usr/local/bin/git`
    - Linux: `/usr/bin/git`
4.  Click **OK**

### Configure Git Identity

In the RStudio Console, run:

``` r
system('git config --global user.name "Your Name"')
system('git config --global user.email "your.email@example.com"')
```

### Generate SSH Key

SSH keys allow secure communication between your computer and GitHub
without entering your password every time.

In RStudio:

1.  Go to **Tools \> Global Options \> Git/SVN**
2.  Click **Create RSA Key** (accept defaults)
3.  Click **View public key** and copy the entire key
4.  Go to GitHub.com \> **Settings \> SSH and GPG keys \> New SSH key**
5.  Paste your key and save

### Install devtools

Important: You must install devtools before using any development
functions:

``` r
install.packages("devtools")
```

### Required system libraries

**R package dependencies:** The spatial R packages requires careful
setup. You will need working installations of the packages `terra`,
`sf`, `exactextractr`, which depend on system libraries like `GDAL`,
`GEOS`, `PROJ`

## Development setup

### Prerequisites

- R (version 4.0 or higher is recommended)

- Git and GitHub account for version control

- Sufficient disk space

- Required system libraries for spatial packages

### Fork the Repository on GitHub

Go to github.com/prio-data/priogrid Click the Fork button (top-right
corner) This creates your personal copy of the repository under your
GitHub account

### Clone Your Fork to Your Computer

In RStudio:

Go to File \> New Project \> Version Control \> Git Repository URL:
Paste the URL of your fork (e.g.,
<https://github.com/YOUR_USERNAME/priogrid.git>) Project directory name:
priogrid (or your preference) Create project as subdirectory of: Choose
where to save the project Click Create Project

Alternatively, via command line:

``` r
system('git clone https://github.com/YOUR_USERNAME/priogrid.git')
```

### Install R Package Dependencies

``` r
# Install the package 
install.packages("renv")
renv::install("prio-data/priogrid")

# Set data directory 
library(priogrid)
pgoptions$set_rawfolder("/path/to/data/directory")
```

#### Troubleshooting installation

- Missing packages: Use `renv::install("package_name")`

- System path issues: Add paths to `~/.Renviron` or use `Sys.setenv()`

- macOS/Homebrew issues: See installation guides for `sf` and `terra`
  packages

### Set Up Your Data Directory

``` r
library(priogrid)
pgoptions$set_rawfolder("/path/to/data/directory")  # Where source data will be stored
```

### Verify Setup

``` r
install.packages("devtools")
devtools::load_all()        # Load the package in development mode
?priogrid         # Check basic documentation
```

## Contribution Workflow Overview

The standard contribution process:

- Create a branch for your feature (don’t work directly on main)
- Make your changes following this guide
- Test your changes thoroughly
- Commit your changes with clear messages
- Push your branch to your fork on GitHub
- Submit a Pull Request from your fork to the main repository

## Ways to contribute

#### Suggest new data sources or variables

The easiest way to contribute is by identifying valuable data sources or
suggesting new variables that would benefit the users of PRIO-GRID.

Use our [issue form](https://github.com/prio-data/priogrid/issues) to:

- Propose new data sources

- Request new variables

- Report bugs or issues

#### Add data sources

1.  Add citations to `inst/REFERENCES.bib`
2.  Add source metadata to `data_raw/sources.csv`
3.  Run `data_raw/pgsources.R` to update `data/pgsources.rds`
4.  Create `R/data_yoursource.R` with a `read_yoursource()` function

#### Create new variables

Write `gen_variablename()` functions that:

- Can run without arguments (use sensible defaults)

- Return a `terra::SpatRaster` object

- Follow our naming conventions

## Adding Data Sources

Before adding any data, verify the license is compatible (see License
Compatibility below).

### Data sources and licensing

Licensing determines how we can integrate and distribute the data in
PRIO-GRID

We accept data sources that are:

- Open access (not behind paywalls)

- Sub-national (spatial resolution below country-level)

- Covering more than one country

- Relevant to conflict research and political science

- From 1850 onwards (focus on contemporary data)

**API sources:** For frequently updated data, we are interested in
building functions that access APIs rather than storing static copies.
These will not appear in our final database, but provide dynamic access
to current information.

#### License compatibility

| License type | Status | Notes |
|----|----|----|
| Permissive (MIT, Unlicense) | 🟩 Supported | Full distribution and modification allowed |
| Attribution (CC-BY 4.0) | 🟩 Supported | Citations required (we provide these) |
| Non-commercial (CC-BY-NC 4.0) | 🟩 Supported | Commercial use restrictions noted |
| No derivatives (CC-BY-ND 4.0) | 🟥 Not supported | Cannot include modified data in the database |
| Copyleft (CC-BY-SA 4.0, GPL 3.0) | 🟨 Metadata only | Functions to access data, but no direct inclusion |
| All rights reserved | 🟨 Functions only | Download and read functions only, no redistribution |

## Code contribution guidelines

Our coding standards ensure that contributions work well together and
remain maintainable over time.

**Source-specific files:** Each data source gets its own file following
the pattern `R/data_sourcename.R`. This keeps related functionality
together and makes it easy to track which functions work with which data
source.

#### Function naming conventions

- Read functions: `read_sourcename()`

  - Every data source must have a read function that handles the basic
    task of loading raw data into R

- Variable functions: `gen_variablename()`

  - Functions to create research variables that appear in the PRIO-GRID
    database

- Utility functions: `sourcename_utilityfunction()`

  - Helper function that support data processing but don’t create final
    variables

#### Function requirements

`read_` functions:

``` r
read_yoursource <- function() {
    # Use get_pgfile(source_name = "source",
    #                source_version = "version",
    #                id = "source_id") 
    # Return sf data.frame or terra SpatRaster
    # Minimal processing - preserve original data structure
}
```

`gen_` functions:

``` r
gen_variablename <- function() {
    # Must work without arguments (sensible defaults)
    # Return terra SpatRaster
    # Static data: layer name = variable name 
    # Time series: layer names = dates as character strings 
}
```

## Coding standards

- Namespace everything: Use `package::function()` format (except for
  priogrid functions)

- Use renv: Install packages with `renv::install()`, update with
  `renv::status()`

- Global options: Access via `pgoptions$get_youroption()`

- Testing: Write unit tests using `testthat`

- Documentation: Use `roxygen2` format

### Variable registration

When adding `gen_` functions:

1.  Add variable to `data_raw/variables.csv`
2.  Run `data_raw/pgvariables.R` (set `overwrite = TRUE` temporarily)
3.  Verify changes with `git diff`

### Citations and references

- Use [Zotero](https://zotero.org) with [Better
  Bibtext](https://retorque.re/zotero-better-bibtex/) addon

- Citation key format: `"auth.lower + shorttitle(3,3) + year"`

- Add to function documentation:
  `#' @references \insertRef{bibkey}{priogrid}`

- Include both data source citations and usage references

## Testing your changes

``` r
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

- Spatial: `sf` (vector), `terra` (raster), `exactextractr` (accurate
  raster-vector conversion)

- Data: `tidyverse` (including `lubridate` for dates)

- Development: `devtools`, `usethis`, `testthat`, `assertthat`

- Performance: `memoise` (for caching expensive functions)

- Citations: `RefManageR`, `Rdpack`

*We try to minimize new dependencies. Discuss additions in your pull
request.*

## Submitting a Pull Request

### Create a Feature Branch

Never work directly on `main`.

In RStudio Terminal or Git Bash:

    git checkout -b feature/your-feature-name

Or use RStudio:

1.  Click the Git tab (top-right panel)

2.  Click “New Branch”

3.  Name it descriptively (e.g., `add-modis-data`, `fix-gpw-function`)

### Commit Your Changes

Commit early and often with clear messages:

    git add R/data_newsource.R
    git commit -m "Add read function for Global Power Plant Database"

### Stay Updated

Before submitting, merge any changes from the main repository:

    # Add the main repo as upstream (only first time)
    git remote add upstream https://github.com/prio-data/priogrid.git

    # Fetch updates
    git fetch upstream

    # Merge into your branch
    git merge upstream/main

Or in RStudio:

1.  Click the “Pull” button with “upstream” selected.

### Push to Your Fork

    git push origin feature/your-feature-name

Or RStudio: Click the “Push” button.

### Create Pull Request on GitHub

1.  Go to your fork on GitHub

2.  Click “Compare & pull request”

3.  Select the base repository: `prio-data/priogrid` and base branch:
    `main`

4.  Write a clear description:

    - What changes you made

    - Why you made them

    - Any issues they address (use “Fixes \#123” to auto-close issues)

5.  Submit the pull request

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

**A:** Contact us to discuss specific licensing situations. We’ll work
with you to find an appropriate way to include your data while
respecting license terms.

------------------------------------------------------------------------

**Need help?** Open an issue on our [GitHub
repository](https://github.com/prio-data/priogrid/issues) or review our
existing documentation and examples.

**Resources:**

- [Happy Git with R](https://happygitwithr.com): Git and GitHub guide
  for R users

- [R Packages book](https://r-pkgs.org): Comprehensive guide to R
  package development

- [Choose a License](https://choosealicense.com): License comparison
  tool

- [Creative
  Commons](https://creativecomms.org/share-your-work/cclicenses/): CC
  license details  
