# Contributing to PRIO-GRID

## Welcome, and thank you for being here

PRIO-GRID is an open source project, built by and for the scientific community. Its purpose is to lower the barriers of subnational spatial research for social scientists. PRIO-GRID takes messy and mismatched geospatial data, from climate rasters, conflict events, population grids, and administrative boundaries and more, and standardizing it into one consistent grid. The goal is that any researcher can use PRIO-GRID regardless of their GIS or data science background.

For over a decade PRIO-GRID has helped researchers across domains. Our goal is that PRIO-GRID will continue to benefit researchers in more decades to come.

Our mission is that PRIO-GRID will remain a pillar for spatial science science research. For this to happen, we need the community to contribute, identifying problems, sharing ideas, ask questions, and help us make PRIO-GRID better for everyone. Most importantly, we hope you find PRIO-GRID useful in your own work, and that you want to use it and tell your colleagues and students about it.

If you want to contribute to PRIO-GRID technically, refer to the *Contributing to PRIO-GRID tutorial*.

## New to PRIO-GRID? Start with the tutorials

The best first contribution is to use PRIO-GRID. Our tutorials are written to guide you when using PRIO-GRID in your own work, and are the fastest way to understand what the project can do:

-   *Getting Started* show you how to set up the package, download the official release, and read PRIO-GRID as tabular data.

-   *Accessing PRIO-GRID as Rasters* show you how to work with individual variables as spatial rasters using `terra` for spatial analysis and mapping.

-   *Custom Spatial and Temporal Configuraions* show you how to change the resolution, extent, projection, or time step to build tailored regional datasets or conduct sensitivity analyses.

-   *Understanding PRIO-GRID Metadata* show you the sources and variables behind the grid with `pgsources`, `pgvariables`, and `pgsearch()`.

-   *Citation and Bibliography* show you how to automatically retrieve every citation the data you used requires, and how to export it as BibLaTeX for your paper.

Please let us know if a tutorial is confusing or you encounter errors.

## Ways to contribute

Acts of contribution by the whole scientific community help PRIO-GRID to grow and improve. Here are ways to contribute, from the easiest to the most involved. We welcome and appreciate all of them.

-   **Spread the word.** Mention PRIO-GRID in methods sections, workshops, reading groups, and to students.

-   **Use it in your research and teaching.** Every time PRIO-GRID is used in research, it helps grow the project.

-   **Cite the data you use.** PRIO-GRID builds on data from many upstream providers that must be cited and credited. The package makes this easy with `pgcitations()` (see the *Citations tutorial)*.

-   **Send us issues.** If you encounter errors, failed downloads, or bugs, *open an issue* (see *Reporting a bug.*

-   **Suggest a data source or variable.** If you know a great open dataset that should be incorporated into PRIO-GRID, please suggest it for us (see *Suggesting a data source*).

-   **Improve the documentation**. Clearer explanations, better examples, fixed typos, or other improvements that may help future users are welcomed.

-   **Contribute code**. If you want to fix a bug or add a data source yourself we welcome contributions (see *Contributing code*).

### Reporting a bug or issue

Clear issue reports help us maintainers of PRIO-GRID. Therefore, before opening a new issue, it helps to:

1.  Search existing issues in case someone else has already brought it up. If so, please add any new detail as a comment.
2.  Confirm you are on the current version, and if you can, boil the problem down to the snippet of code that reproduces the issue.

Then open a new issue and ideally provide the following:

**What happened**

Provide a clear description of the problem

**How to reproduce it**

The code snippet that triggers the issue:

```{r}
# issue code

```

**The error message**

Paste it in full

**Your setup**

Provide the output of:

```{r}
sessionInfo()
packageVersion("priogrid")
```

**Did the failure happen during a download, a spatial operation, or installation?**

Note that downloads are large and some functions rely on system geo-library (GDAL, GEOS, PROJ), which may take time to execute.

## Suggesting a data source, variable, or feature

We welcome ideas for new open datasets to incorporate into PRIO-GRID. If you want a full overview of what already exists in PRIO-GRID before you suggesting, have a look at the *Metadata tutorial* and `pgsearch()` to see whether something similar already exists. Then *open an issue* with:

**What are you proposing?**

A feature, a new source, or a new variable.

**For new data source, please include:**

-   Dataset name and version

-   Provider and URL

-   License (e.g. CC BY 4.0, ODC-By)

-   Spatial extent and temporal resolution

-   Citation/DOI

**Would you like to help add it?**

This is optional. We appreciate suggestions either way, but contributing with code to add it might speed up the process of integrating the data.

## **Contributing code**

Would you like to fix a bug or add a feature? The basic workflow follows GitHub standards.

1.  Fork the repository
2.  Create a branch for your change
3.  Make your edits, and add or update a test for anything you change
4.  Open a pull request on `prio-data/priogrid`, describing what it does and why.

Additional notes for contributors:

-   **The package is set up for development with `devtools`.** `devtools::load_all()` loads your working copy, `devtools::test()` and `devtools::document()` can also be useful commands to run before opening a pull request.

-   **Match the existing style.** PRIO-GRID follows the tidyverse style guide and uses established naming patterns such as `read_*()`, `gen_*()`, `pg_*()`. Documentation is written with `roxygen2`

-   **Draft pull request are also welcome.** If you want early feedback or you get stuck halfway it may still help to contribute what you have done, and one of our maintainers can take over or guide you.

If your contribution is a new data source or variable, our *Contributing to PRIOGRID tutorial* walks through every step from registering the source, writing `read_*()` and `gen_*()` functions, adding tests, and verifying the result.

We are a small research team working on PRIO-GRID alongside other research project. Therefore we encourage patience with reviews, but a friendly nudge is always OK if things take time. Also remember that review comments are aimed at keeping PRIO-GRID a reliable tool for the whole community and not a judgement of you or your coding skills. We will try and be clear about what would help a contribution get merged into PRIO-GRID.

## Code of Conduct

We are committed to a friendly, safe, and welcoming environment for everyone. By taking part in contributing through issues, pull requests or any other contributions to the project you agree to uphold our Code of Conduct. Please report any unacceptable behavior should you encounter it. Reports are handled confidentially.

## Getting help

Do you need assistance, have any questions, or simply want to discuss ideas? Please reach out.

**Questions, bugs, and ideas:** the *Issue Tracker.*

**PRIO-GRID research team:**

-   Jonas Vestby (Head of Development): jonves\@prio.org

-   Andreas Forø Tollefsen (Author): andreas\@prio.org

-   Kristine Helskog (Author): krihel\@prio.org

### A note on credit and licensing

The `priogrid` code is released under the MIT License and the PRIO-GRID dataset is released under ODC-BY v1.0. Data derived from third-party sources remains subject to those providers' licenses. See `LICENSE.md` for citation details. By contributing, you agree your work is offered under these same terms.

**Thank you for contributing to PRIO-GRID.**

Whether you use it, share it, report issues, or contribute with code, we appreciate that you want to be part of keeping spatial research open, reproducible and accessible!
