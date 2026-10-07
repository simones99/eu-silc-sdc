# Render reports/report.Rmd from the files in output/ into site/index.html.
output_dir <- normalizePath("output", mustWork = TRUE)
dir.create("site", showWarnings = FALSE)
rmarkdown::render("reports/report.Rmd", output_file = "index.html",
                  output_dir = normalizePath("site"), params = list(output_dir = output_dir),
                  quiet = TRUE)
