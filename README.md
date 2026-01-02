
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Capstone MovieLens

*A movie recommendation system using the MovieLens dataset*

This is the ***MovieLens*** project as part of the [edX HarvardX
PH125.9x Data Science Capstone
Course](https://pll.harvard.edu/course/data-science-capstone) to build a
*Movie Recommendation System* according to the course requirements.

The goal of the project is to build a Recommendation System using
datasets derived from a [10M version of the MovieLens
dataset](http://grouplens.org/datasets/movielens/10m/).

These datasets are generated as part of a separate project,
[edx.capstone.movielens.data](https://github.com/AzKurban-edX-DS/edx.capstone.movielens.data/blob/main/README.md)
that provides all required data after being installed as an *R package*
(the source code of the package is also available on
[GitHub](https://github.com/AzKurban-edX-DS/edx.capstone.movielens.data)).

## Essential Directories and Files

The following are essential directories and files of the project:

- [capstone-movielens.main.R](r/src/capstone-movielens.main.R): The
  *Main `R` Script*;

- [support-functions](r/src/support-functions): Folder for the
  *User-defined Functions* used by the *Main `R` Script*;

- [setup.R](r/src/support-scripts/setup.R): Support script to install
  all the necessary packages, load required libraries, and resolve
  conflicts;

- [\_startup](r/_startup): Folder for the project starter scripts;

- [capstone-movielens.site](reports/capstone-movielens.site): Folder for
  the *RMD-Report* files;

\_
[capstone-movielens.report.pdf](reports/capstone-movielens.site/final-pdf/capstone-movielens.report.pdf):
Ultimately rendered *PDF-Report*;

- `data` folder: (automatically created if it does not exist, during the
  *Main `R` Script* execution) Directory for cache files, intended for
  storing intermediate results and required datasets used by the project
  scripts;

## Running the project

The simplest (and recommended) way to run the project for the very first
time is to run the [\_\_run-all.R](r/_startup/__run-all.R) script
located in the [\_startup](r/_startup) folder.

This script is a runner for the [\_starter.R](r/_startup/_starter.R)
script, which in turn is a launcher for the following four scripts:

1.  [0.setup.R](r/_startup/0.setup.R): The script is a launcher for the
    [setup.R](r/src/support-scripts/setup.R) script mentioned above in
    the [Essential Directories and
    Files](#essential-directories-and-files) section.

2.  [1.init.R](r/_startup/1.init.R): Initializes all the required
    datasets and loads the general-purpose and data helper functions
    from the following scripts:

- [logging-functions.R](r/src/support-functions/logging-functions.R):
  User-defined *logging functions*;

- [common-helper.functions.R](r/src/support-functions/common-helper.functions.R):
  User-defined *common helper functions*;

- [data.helper.functions.R](r/src/support-functions/data.helper.functions.R):
  User-defined *data helper functions*.

3.  [2.run-main-script.R](r/_startup/2.run-main-script.R): The script is
    a launcher for the
    [capstone-movielens.main.R](r/src/capstone-movielens.main.R) script
    mentioned above in the [Essential Directories and
    Files](#essential-directories-and-files) section.

4.  [3.rmd.render.R](r/_startup/3.rmd.render.R): Renders the *Capstone
    PDF Report*.

> \[!NOTE\] The very first execution of the script
> [\_\_run-all.R](r/_startup/__run-all.R) may take several hours.

Subsequent executions of the project scripts will be significantly
faster, as the data stored in the cache during the initial run will not
be recalculated.

To force recalculation of the particular data, the corresponding cache
file must be manually deleted.

Additionally, project scripts can subsequently be run individually to
perform a specific task. These tasks are described in more detail below.

Before running the scripts below, make sure that the following
variables, containing the *File System paths*, are initialized:

- `startup.path`;
- `setup.script_path`;
- `init.script_path`;
- `run_main.script_path`;
- `rmd_render.script_path`;

These variables can be initialized by the following [code
snippet](r/_startup/_starter.R#L2) provided in the script
[\_starter.R](r/_startup/_starter.R#L2):

``` r
## Source File Paths -----------------------------------------------------------
startup.path <- "r/_startup"
setup.script_path <- file.path(startup.path, "0.setup.R")
init.script_path <- file.path(startup.path, "1.init.R")
run_main.script_path <- file.path(startup.path, "2.run-main-script.R")
rmd_render.script_path <- file.path(startup.path, "3.rmd.render.R")
```

### Installing Packages and/or Loading External Libraries

As mentioned above in section [Essential Directories and
Files](#essential-directories-and-files), the script
[setup.R](r/src/support-scripts/setup.R) installs all necessary
packages, loads required libraries, and resolves conflicts. It can also
be launched by the following [code snippet](r/_startup/_starter.R#L11)
provided in the script [\_starter.R](r/_startup/_starter.R#L11):

``` r
source(setup.script_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)
```

### Initializing Project Datasets and Loading Core Helper Functions

As mentioned above in *this section*, the script
[1.init.R](r/_startup/1.init.R) initializes all the required datasets
and loads the general-purpose and data helper functions. It can also be
launched by the following [code snippet](r/_startup/_starter.R#L20)
provided in the script [\_starter.R](r/_startup/_starter.R#L20):

``` r
source(init.script_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)
```

### Launching the *Main `R` Script*

As mentioned above in section [Essential Directories and
Files](#essential-directories-and-files), the script
[capstone-movielens.main.R](r/src/capstone-movielens.main.R) is the
*Project Main `R` Script*. It can also be launched by the following
[code snippet](r/_startup/_starter.R#L27) provided in the script
[\_starter.R](r/_startup/_starter.R#L27):

``` r
source(run_main.script_path,
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)
```

### Rendering the Capstone PDF Report

As mentioned above in *this section*, the script
[3.rmd.render.R](r/_startup/3.rmd.render.R) renders the *Capstone PDF
Report*. It can also be launched by the following [code
snippet](r/_startup/_starter.R#L35) provided in the script
[\_starter.R](r/_startup/_starter.R#L35):

``` r
source(rmd_render.script_path,
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)
```
