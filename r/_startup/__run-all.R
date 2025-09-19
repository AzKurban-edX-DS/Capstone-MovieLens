startup.path <- "r/_startup"
starter.script_path <- file.path(startup.path, "_starter.R")

## Run Scripts -----------------------------------------------------------------

source(starter.script_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)
