
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
  all the required packages and resolve conflicts;

- [\_startup](r/_startup): Folder for the project starter scripts;

- [data](data): Directory for cache files, intended for storing
  intermediate results and required datasets used by the project
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

> [!NOTE] 
> The very first execution of the script [__run-all.R](r/_startup/__run-all.R) may take several hours.
