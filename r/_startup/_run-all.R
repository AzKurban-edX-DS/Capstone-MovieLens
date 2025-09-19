# Run All Scripts
## Source File Paths -----------------------------------------------------------
startup.path <- "r/_startup"
init.script_path <- file.path(startup.path, "1.init.R")
run_main.script_path <- file.path(startup.path, "2.run-main-script.R")
rmd_render.script_path <- file.path(startup.path, "3.rmd.render.R")

## Run Scripts -----------------------------------------------------------------

source(init.script_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)

source(run_main.script_path,
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)

# source(path, 
#        catch.aborts = TRUE,
#        echo = TRUE,
#        spaced = TRUE,
#        verbose = TRUE,
#        keep.source = TRUE)
# 

