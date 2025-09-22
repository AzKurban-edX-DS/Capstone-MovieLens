# Setup

## Source File Paths -----------------------------------------------------------
r.path <- "r"
src.folder <- "src"
support_scripts.folder <- "support-scripts"
support_functions.folder <- "support-functions"

r.src.path <- file.path(r.path, src.folder)
support_scripts.path <- file.path(r.src.path, support_scripts.folder)
support_functions.path <- file.path(r.src.path, support_functions.folder)

## Setup -----------------------------------------------------------------------
setup_script.file_path <- file.path(support_scripts.path, "setup.R")

source(setup_script.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)
