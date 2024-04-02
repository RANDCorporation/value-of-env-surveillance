#------------------------------------------------------------------------------#
# Code for "The value of environmental surveillance for pandemic response"
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# load standard packages --------------------------------------------------

# unfortunately, this is necessary so that we can run experiments one after the other
#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#gc() #free up memrory and report the memory usage.

#showConnections()


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



# Load settings:
s <- yaml::read_yaml("settings.yml")


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
if(s$r6sim_load_all) {
  devtools::load_all("../R6Sim")
} else {
  library(R6Sim)
}

