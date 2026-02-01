# RMD Render

# Render RMD Report
start <- print_start_date()
rmarkdown::render("reports/capstone-movielens.site/index.Rmd",
                  output_file = "capstone-movielens.report.pdf",
                  run_pandoc = TRUE)
print_end_date(start)
