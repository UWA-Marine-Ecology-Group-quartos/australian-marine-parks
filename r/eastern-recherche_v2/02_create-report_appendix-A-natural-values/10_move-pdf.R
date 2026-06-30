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

# TODO Change pdf_name to your quarto name
pdf_name <- "Project 4.21-Eastern-Recherche-2-Appendix A-q-Natural values.pdf"

dest_dir <- paste0("quartos/", park)
if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

file.rename(
  from = file.path(
    paste0("r/", park, "/02_create-report_appendix-A-natural-values"),
    pdf_name
  ),
  to = file.path(dest_dir, pdf_name)
)
