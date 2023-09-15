
#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# This script demonstrates how to run the model for a single run:

source("./R/library.R")

# Single run examples -----------------------------------------------------

model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

# run the model with default inputs 
model$simulate(0:100, reps = 100)

# results can be viewed in the model$summary_* objects
#  model$summary

# One can also set inputs manually and run the model again
model$set_input("r", 0.1)$
  simulate()
