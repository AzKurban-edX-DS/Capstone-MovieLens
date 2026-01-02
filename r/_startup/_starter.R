# Run All Scripts
## Source File Paths -----------------------------------------------------------
startup.path <- "r/_startup"
setup.script_path <- file.path(startup.path, "0.setup.R")
init.script_path <- file.path(startup.path, "1.init.R")
run_main.script_path <- file.path(startup.path, "2.run-main-script.R")
rmd_render.script_path <- file.path(startup.path, "3.rmd.render.R")

## Run Scripts -----------------------------------------------------------------

### Installing Packages and/or Loading External Libraries
source(setup.script_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)

### Initializing Project Datasets and Loading Core Helper Functions
source(init.script_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)

### Launching the *Main `R` Script*
source(run_main.script_path,
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)

### Rendering the Capstone PDF Report
source(rmd_render.script_path,
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)
