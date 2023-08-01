


#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# load standard packages --------------------------------------------------

library(dplyr)
library(tidyr)
library(lubridate)
library(randplot)
library(ggplot2)
library(showtext)
library(yaml)
library(odin)
library(dde)


# Load settings:
s <- yaml::read_yaml("settings.yml")

# Clone this repository https://gitlab.com/covidpaths/c19model
# into the parent folder of the c19-paths repository
# this will be necessary until we release the package externally
if(s$c19model_load_all) {
  devtools::load_all("../../R6Sim")
} else {
  library(R6Sim)
}


# Source all functions
invisible(sapply(setdiff(list.files(path = s$lib_path, pattern = "*.R",full.names = T),
                         s$lib_file),
                 source,
                 echo = F)
)

# Set up ggplot theme:

font_add_google("PT Sans", "PT Sans")

ggplot <- function(...) ggplot2::ggplot(...) + randplot::theme_rand(font = "PT Sans")
