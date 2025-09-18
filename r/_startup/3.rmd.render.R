# RMD Render

# rmarkdown::render("reports/capstone-movielens-report.draft4.Rmd",
#                   output_format = "pdf_document2")

# Render RMD Report
# rmarkdown::render("reports/capstone-movielens.site/index.Rmd",
#                   output_file = "capstone-movielens.report.pdf",
#                   # output_format = "pdf_document2",
#                   output_format = rmarkdown::pdf_document(
#                     pandoc_args = c("--metadata-file", "main.yaml")
#                   ),
#                   envir = .GlobalEnv)

# rmarkdown::render("reports/capstone-movielens.site/index.Rmd",
#                   #output_format = "pdf_document2",
#                   output_file = "capstone-movielens.report.pdf",
#                   run_pandoc = TRUE)

rmarkdown::render("reports/capstone-movielens.site/index.Rmd",
                  output_file = "capstone-movielens.report.pdf",
                  run_pandoc = TRUE)


