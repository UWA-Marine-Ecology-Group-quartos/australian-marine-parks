# Clear your environment
rm(list = ls())

# Set the study name
script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park

pdf_name <- "Project 4.21-Geographe-2-Appendix A-q-Natural values.pdf"

dest_dir <- "D:/GIT/australian-marine-parks/quartos/geographe"
if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

file.rename(
  from = file.path(
    "D:/GIT/australian-marine-parks/r/geographe/02_create-report_appendix-A-natural-values",
    pdf_name
  ),
  to = file.path(dest_dir, pdf_name)
)
