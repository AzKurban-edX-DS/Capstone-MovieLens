# RMD Render

# Render RMD Report
rmarkdown::render("reports/capstone-movielens.site/index.Rmd",
                  output_file = "capstone-movielens.report.pdf",
                  run_pandoc = TRUE)


