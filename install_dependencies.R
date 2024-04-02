#------------------------------------------------------------------------------#
# Code for "The value of environmental surveillance for pandemic response"
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Pre-requisite packages:

pre_packages <- c("renv", "yaml", "remotes")

install.packages(pre_packages, repos = "https://cran.rstudio.com")

# Use this script to install dependencies for this project.

s <- yaml::read_yaml("settings.yml")

# install a fresh version from CRAN if not using renv
if(!s$use_renv) {

  # CRAN packages
  cran_packages <- c("Hmisc", "R6", "tidyr", "odin", "dplyr", "ggplot2", "dde",
                     "gt", "lemon", "lubridate", "purrr", "readxl",
                     "scales", "writexl", "pkgbuild", "pkgload", "svglite"
  )

  install.packages(cran_packages, repos = "https://cran.rstudio.com")

 #remotes::install_github("https://github.com/RANDCorporation/randplot")

 # To be uncommented once R6Sim is public on Github
 # remotes::install_github("https://github.com/RANDCorporation/R6Sim")

} else {

  renv::restore()

}
