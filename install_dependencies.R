#------------------------------------------------------------------------------#
# Code for "The value of environmental surveillance for pandemic response"
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Use this script to install dependencies for this project.

# Alternatively, first install the renv package with install.packages("renv"),
# then run renv::restore() to install the same package versions we used or use renv::install().

# CRAN packages
cran_packages <- c("Hmisc", "R6", "tidyr", "odin", "dplyr", "ggplot2", "dde",
                   "gt", "lemon", "lubridate", "parallel", "purrr", "readxl",
                   "scales", "writexl", "yaml", "pkgbuild", "pkgload")

install.packages(cran_packages)

# github packages
remotes::install_github("https://github.com/RANDCorporation/randplot")

remotes::install_github("https://github.com/RANDCorporation/R6Sim")

