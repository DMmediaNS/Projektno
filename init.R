# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("shiny", "tidyverse", "readxl", "shinythemes", "shinyWidgets", "plotly", "scales", "viridis", "crosstalk", "lubridate", "writexl", "shinycssloaders")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
