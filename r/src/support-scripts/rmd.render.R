# Setup & Render ---------------------------------------------------------------
## Setup
r.path <- "r"
src.folder <- "src"
support_scripts.folder <- "support-scripts"
support_functions.folder <- "support-functions"

r.src.path <- file.path(r.path, src.folder)
support_scripts.path <- file.path(r.src.path, support_scripts.folder)
support_functions.path <- file.path(r.src.path, support_functions.folder)
setup_script.file_path <- file.path(support_scripts.path,
                                    "setup.R")
# main_script.file_path <- file.path(r.src.path,
#                                    "capstone-movielens.main.R")

stopifnot(file.exists(setup_script.file_path))

source(setup_script.file_path, 
       local = knitr::knit_global())
# or sys.source("your-script.R", envir = knitr::knit_global())


# rmarkdown::render("reports/capstone-movielens-report.draft4.Rmd",
#                   output_format = "pdf_document2")

## Load Source Datasets from Specially Designed Package
if(!require(edx.capstone.movielens.data)) {
  start <- put_start_date()
  pak::pak("AzKurban-edX-DS/edx.capstone.movielens.data")
  put_end_date(start)
}

put_log("Dataset loaded from `edx.capstone.movielens.data` package: edx")
put(str(edx))
sum(is.na(edx$rating))
#> [1] 0
put(summary(edx))

put_log1("Dataset loaded from `edx.capstone.movielens.data` package: final_holdout_test:
%1", str(final_holdout_test))
sum(is.na(final_holdout_test$rating))
#> [1] 0
put(summary(final_holdout_test))

# Render RMD Report
rmarkdown::render("reports/capstone-movielens.site/index.Rmd",
                  output_format = "pdf_document2",
                  output_file = "capstone-movielens.report.pdf")
