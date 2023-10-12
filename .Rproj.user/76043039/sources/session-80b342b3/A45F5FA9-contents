# ------------------------------------------------------------------------------
# create_report.R
#
# Edit and run this script to compile Rmarkdown reports
#
# ------------------------------------------------------------------------------

library(rmarkdown)

rmd <- "BRS_ensemble_presentation.Rmd"

# Render pdf
render(
  input = rmd,
  beamer_presentation(
    theme = 'AnnArbor', 
    toc = T, 
    colortheme = 'dolphin', 
    fonttheme = 'structurebold'),
  output_dir = "../report"
)