


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
library(officer)
library(R6Sim)
library()

# Load settings:
s <- yaml::read_yaml("settings.yml")


# Source all functions
invisible(sapply(setdiff(list.files(path = s$lib_path, pattern = "*.R",full.names = T),
                         s$lib_file),
                 source,
                 echo = F)
)

ggplot <- function(...) ggplot2::ggplot(...) + theme_rand_ppt(font = "Helvetica")
