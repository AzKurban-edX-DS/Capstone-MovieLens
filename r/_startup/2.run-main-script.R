main.script_path <- file.path(r.src.path, "capstone-movielens.main.R")

source(main.script_path,
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)

