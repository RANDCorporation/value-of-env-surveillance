#------------------------------------------------------------------------------#
# Code for "The value of environmental sampling surveillance"
# Copyright (C) 2024 by The RAND Corporation
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# See LICENSE.md and README.md for more information on usage and licensing
#
# Author: Pedro Nascimento de Lima
#------------------------------------------------------------------------------#

# Pre-requisite packages:

pre_packages <- c("renv", "yaml", "remotes")

install.packages(pre_packages, repos = "https://cran.rstudio.com")

# Use this script to install dependencies for this project.

s <- yaml::read_yaml("settings.yml")

# install a fresh version from CRAN if not using renv
if (!s$use_renv) {

  # CRAN packages
  cran_packages <- c(
    "Hmisc", "R6", "tidyr", "odin", "dplyr", "ggplot2", "dde",
    "gt", "lemon", "lubridate", "purrr", "readxl",
    "scales", "writexl", "pkgbuild", "pkgload", "svglite"
  )

  install.packages(cran_packages, repos = "https://cran.rstudio.com")

  # remotes::install_github("https://github.com/RANDCorporation/randplot")

  # To be uncommented once R6Sim is public on Github
  # remotes::install_github("https://github.com/RANDCorporation/R6Sim@v1.0.0")
} else {
  renv::restore()
}
