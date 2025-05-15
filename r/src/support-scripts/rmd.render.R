# Setup & Render ---------------------------------------------------------------

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
                  #output_format = "pdf_document2",
                  output_file = "capstone-movielens.report.pdf",
                  run_pandoc = TRUE)

# rmarkdown::render("reports/capstone-movielens.site/index.Rmd",
#                   output_file = "capstone-movielens.report.pdf",
#                   # output_format = "pdf_document2",
#                   output_format = rmarkdown::pdf_document(
#                     pandoc_args = c("--metadata-file", "main.yaml")
#                   ),
#                   envir = .GlobalEnv)


