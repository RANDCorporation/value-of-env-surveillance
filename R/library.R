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

# load standard packages --------------------------------------------------

# unfortunately, this is necessary so that we can run experiments one after the other
# rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
# gc() #free up memrory and report the memory usage.

# showConnections()

# Load settings:
s <- yaml::read_yaml("settings.yml")

if (s$use_renv) {
  source("renv/activate.R")
}

# Set seed
set.seed(s$seed)

# tidyverse
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(scales)
library(readxl)
library(writexl)
library(parallel)
library(yaml)
library(svglite)

# Miscelaneous
library(R6)
library(Hmisc)
library(lemon)
library(yaml)
library(gt)

# Simulation packages
library(odin)
library(dde)

# Necessary because of odin
# defining here because of renv
library(pkgbuild)
library(pkgload)

# RAND packages
# install with remotes::install_github("randcorporation/randplot")
library(randplot)

# install with remotes::install_github("randcorporation/r6sim")
library(R6Sim)






# Source all functions
invisible(sapply(
  setdiff(
    list.files(path = s$lib_path, pattern = "*.R", full.names = T),
    s$lib_file
  ),
  source,
  echo = F
))

ggplot <- function(...) {
  ggplot2::ggplot(...) +
    theme_rand_ppt(font = "Helvetica")
}

# into the parent folder of the c19-paths repository
# this will be necessary until we release the package externally
if (s$r6sim_load_all) {
  devtools::load_all("../R6Sim")
} else {
  library(R6Sim)
}
