

# Use code below to discover dependencies:


dependencies <- renv::dependencies(path = "./R")

packages <- unique(dependencies$Package)

cran_packages <- c("Hmisc", "R6", "tidyr", "odin", "dplyr", "ggplot2", "dde",
                  "gt", "lemon", "lubridate", "parallel", "purrr", "readxl", "scales", "writexl", "yaml")

rand_packages_github <- c("R6Sim", "randplot")


