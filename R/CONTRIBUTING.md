---
editor_options: 
  markdown: 
    wrap: 72
---

# How to contribute to PRIO-GRID

Thank you for showing interest in contributing to PRIO-GRID! PRIO-GRID
is an open-source initiative to survey, collect, and provide easy access
to the wealth of open sub-national data relevant for conflict research
and political science. We also want to facilitate cross-fertilization
across data-sets and use the platform to build new and innovative
data-resources based on existing ones.

For PRIO-GRID to thrive, we have built it as an open platform *for*
academic researchers. First and foremost, it provides easy access to
data, citations, and further references so that we do not need to do
mundane jobs twice:

-   You can download ready processed data from our web-server through
    our R-package or from our web-page (we have a server at PRIO that
    runs all calculations and outputs a new version of PRIO-GRID every
    time a new version is released).
-   You can download and open the raw-data from our R-package.
-   If you wonder which sources to cite given the variables you have
    used in your research, you can use a function in our R-package to do
    that.

PRIO-GRID also provides the ability to customize how data is calculated:

-   Through the global `pgoptions`, you can set extent, projection,
    spatial and temporal resolution, and temporal scope. Then you can
    calculate all or parts of PRIO-GRID using your new parameters.
-   You can explore the functions in the library and use the parameters
    exposed there.
-   You can re-write functions to expand functionality, or write new
    functions (and we take pull requests!).

We also appreciate suggestions for new data sources or new variables:

-   The easiest way to add new sources or suggest new variables is to
    use [our issue
    form](https://github.com/prio-data/priogrid/issues/new/choose). If
    the data fits with our guidelines, we will include it, or provide
    guidance for how you can write the functions you need.

-   You can also fork the repo, add the proper citations to
    `inst/REFERENCES.bib`, add the source to `data_raw/sources.csv`, run
    `data_raw/pgsources.R` to create `data/pgsources.rds`, create a new
    `R/data_yoursource.R`, write a `read_yoursource()` function, save,
    add, commit and push to your own repository, and then create a pull
    request to prio-grid.

-   New variables can be written as functions within a
    `data_anyrelevantsource.R` file. If your variables pulls from
    multiple sources, just choose one of the R-files to write your
    function in.

# Sources and licenses

We are interested in data sources that provide open sub-national data
relevant for conflict research, political sciences, etc. For this end,
we are interested in data with data across more than one country, and
with a spatial resolution lower than the full country (e.g., admin 1,
point data, raster data). We will not be able to fully support data
sources with frequent updates - direct APIs are likely better to use.
However, we might want to build functions that draw on available APIs to
build useful data-products. Therefore, we are interested in including
such sources in our meta-data (but not including direct download links,
etc.). We are mainly interested in data from 1850- (this is just an
arbitrary point), and with a focus on contemporary data.

We define open as any data that is not behind a pay-wall. Open data come
with different licenses. The default license is "All rights are
reserved". This means that there is no explicit permission given to
publish any new dataset (like a replication data). For PRIO-GRID, this
means that we can only provide functions to download, read, and
calculate data, but not publish the result in our final database.
Permissive licenses such as the MIT or the Unlicense gives permissions
to distribute and modify data, and even use it commercially. Other
licenses permit distribution and modification, but only given certain
restrictions. CC-BY 4.0, for instance, require you to cite the authors
of the data whenever it is used. CC-BY-NC 4.0 requires additionally that
you are not using it commercially. PRIO-GRID will provide citations for
all sources even if they do not require it, and we can inform you if the
subset of data you are using can be used commercially or not. CC-BY-ND
4.0 restricts distribution of modified materials, which means that we
cannot include it (or any variable using it) in our final database.

Certain restrictive licenses cannot be accommodated in PRIO-GRID.
CC-BY-SA 4.0 (and any other copy-left license, like GPL 3.0) requires us
to distribute our contributions with the same license if we remix,
transform, or build upon their material. While we are no lawyers, it is
sufficiently unclear whether just writing functions to transform their
data would be sufficient to invoke this license. While we do include
such data in our source meta-data repository, we cannot provide any more
functionality. The reason for why we do not want to use a copy-left
license is that if we have multiple sources with non-compatible
copy-left licenses, then we would have to choose which source to support
and which not to support. We generally think that copy-left licensing on
data is bad practice, and should be avoided.

See <https://choosealicense.com> and
<https://creativecommons.org/share-your-work/cclicenses/>.

# Conventions

1.  If you are new to Git and GitHub in R, read
    <https://happygitwithr.com>. <https://happygitwithr.com/ssh-keys> is
    very useful.
2.  Read <https://r-pkgs.org>. We try to follow conventions from there:
    -   Testing using `testthat`. A unit-test per function is great, and
        something we strive towards (although not there yet).
    -   Documentation using `roxygen2`. Remember to Build -\> Document
        (`devtools::document()`). (There is also a useful pull-down-menu
        function in RStudio whilst inside a function with your cursor:
        `RStudio -> Code -> Insert Roxygen Skeleton)`.
    -   Normal R-package behavior, such as putting all package R-code
        inside the the R folder, tests in tests folder, etc. Use the
        usethis functions to add package dependencies, etc. Importantly,
        all code must be wrapped into functions. Think about use-cases
        for your functions, and make useful functions.
    -   Always apply the namespace to any function. E.g.,
        `dplyr::mutate()` instead of just `mutate()`. This is also true
        for base package. The only exception is functions written in the
        priogrid package.
3.  We use `renv` for our package management. The easiest way to install
    is to clone the repo, and run `renv::install()`. Install new
    packages with `renv::install()`, and `renv::status()` can provide
    insights as to how to update the lock-file with your new
    dependencies.
4.  You set all global options in `pgoptions`. To get global options in
    functions, use `pgoptions$get_youroption()` (e.g.,
    `pgoptions$get_rawfolder()`).
5.  R-files starting with `R/data_` are tied to a particular source. At
    a minimum, such a file needs a `read_yoursource()` (e.g.,
    `read_cshapes()`) function.
    -   Make sure that the function name is not already taken. Use
        lower-case and underscore. Try to keep the function name
        relatively short.
    -   A `read_`-function must use
        `get_pgfile(source_name, source_version, id)` to find the local
        file. Make sure your source are included in the meta-data first.
        The `read_`-function should do little else than reading the
        raw-file into R. Please ensure correct variable types (Dates,
        etc), and if a utility variable is useful down the line, you can
        add these. However, data should not be changed. Return either a
        sf data-frame or a terra SpatRaster.
    -   You can write as many `source_yourfunctionname()` functions
        (e.g., `cshapes_cover()`) as you want in a `R/data_...R` file.
    -   Functions that return final variables should be named
        `gen_variablename()` (e.g. `gen_cshapes_cover()`).
        `gen_`-functions must be able to run without any arguments
        (e.g., with sensible defaults). When you add a `gen_`-function,
        you also need to add the variable to `data_raw/variables.csv`,
        double-check that you did not affect other entries using the
        `git diff` functionality, and run `data_raw/pgvariables.R`
        (temporary set `overwrite = T` in the `usethis::use_data()`
        function). The `gen_`-functions must return a terra SpatRaster.
        If it is static (i.e., no temporal dimension), the name should
        be the name of the variable. Else, the names of the SpatRaster
        (`names(rast)`) should be the dates as character
        (`as.character(dates)`) for each raster.
6.  Citations:
    -   We use [Zotero](https://www.zotero.org) with the [Better
        Bibtex](https://retorque.re/zotero-better-bibtex/) addon to
        manage our references.
    -   You can import `inst/REFERENCES.bib` to Zotero to be updated. We
        appreciate corrections to our citations.
    -   We do not support the advanced biblatex item types in Zotero as
        we are using `RefManageR` in R. When exporting the library, use
        "Better Bibtex" as Format.
    -   If you want to add citations, you need to use Zotero to make
        sure the bibliography key is correct. We use this citation key
        formula: "auth.lower + shorttitle(3,3) + year"
    -   Make sure author, title, and date/year is included (to get
        correct citation key), and that the citation is correct.
    -   We are interested in both required citations/citations of the
        data source, as well as references using/discussing the data
        source.
    -   In functions using a data source, add
        `#' @references \insertRef{bibkey}{priogrid}` to the
        documentation. This uses `Rdpack` to output a proper citation in
        the documentation. Run `devtools::document()` to generate the
        documentation. It will not render properly before the package is
        built (`build -> install`). See the [Rdpack
        vignette](https://cloud.r-project.org/web/packages/Rdpack/vignettes/Inserting_bibtex_references.pdf)
        for more details.
7.  Data sources:
    -   We document our data sources in `data_raw/sources.csv`. This is
        a tab-separated file. Each source gets a randomly generated
        UUID. We use `uuid::UUIDGenerate()` (make sure *not* to
        set.seed). See `priogrid::pgsources` for documentation of the
        rest of the entries.
    -   After updating `data_raw/sources.csv`, double-check that you did
        not change other entries (e.g., with the `git diff`
        functionality, then run `data_raw/pgsources.R` (setting
        `overwrite = TRUE` temporarily in the `usethis::use_data()`
        function).
    -   Read the section about Sources in this document to get an idea
        whether your source is relevant for us.
8.  Contributing changes to PRIO-GRID:
    1.  Fork [our
        repository](https://github.com/prio-data/priogrid/tree/master).
        [How to fork a
        repository](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks/fork-a-repo).
    2.  Make a new branch with a clear and unique name.
    3.  Make your changes following our guidelines. Test running your
        functions by restarting R, running `devtools::load_all()`, and
        then run your function. Test your functions, and preferably
        write tests. Run your tests. Document all functions properly and
        run `devtools::document()`. Proof-read your documentation in R
        (using `?yourfunction`).
    4.  Make sure that your branch is up-to-date with the remote
        upstream master branch. [How to sync a
        fork.](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks/syncing-a-fork)
        In terminal: `git checkout yourbranch`, then
        `git merge upstream/master`. Then run your tests again. Merging
        with the upstream master is good practice to do every once in a
        while when you are working on a new feature. (If you have direct
        access to the priogrid repo, you can `git merge origin/master`
        instead.) It is also well advised to run `git fetch` to update
        all local branches from the remote. Fetching does not merge. We
        prefer [merging to
        rebasing](https://stackoverflow.com/questions/16666089/whats-the-difference-between-Git-merge-and-git-rebase),
        but we are happy with whatever gets the work done.
    5.  When you are ready, create a pull-request on our repository. You
        will get a reviewer assigned, and you might have to "revise and
        resubmit". [How to make a pull
        request.](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request)
    6.  If we are happy, we will merge your additions into our main
        repository, and it will be included into our next release
        (changes will be immediately available on the repository, of
        course).
9.  Packages we use:
    -   We use `exactextractr` for any conversions from vector to
        raster. terra is not accurate enough.
    -   We use `sf` for most vector work.
    -   We use `terra` for everything else raster related.
    -   We use `tidyverse` for data wrangling, including `lubridate` for
        date handling.
    -   We use `testthat` and `assertthat` for testing and assertions,
        and `devtools` + `usethis` in development.
    -   Certain functions can be cached using `memoise` (particularly
        functions that will be called multiple times).
    -   We try to avoid adding package dependencies.

# Frequently asked questions

1.  I would like to have PRIO-GRID in another spatial or temporal
    resolution. Can you support another resolution?

Our server calculates PRIO-GRID at a global 0.5x0.5 WGS84 yearly
resolution from 1850 until today. We are considering adding a monthly
resolution from 1990 until today. If you want to have another
resolution, you will need to build it yourself. This might also include
additional testing, as we cannot guarantee our functions work with all
options. Please contact us if you want instructions for how we set up
our server, and/or if you are interested in hosting other versions.

2.  My data source is in PRIO-GRID. I would like to change X.

We try as best to represent the open data sources, and provide correct
citations, etc. We will of course listen to any input from the data
providers, be in a dialog, and make changes as quickly as we can so that
your data is represented correctly.
