
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

# all default model inputs are in the inputs object
model$inputs

# We can run the model with default inputs by using the simulate function
# i.e., run for 100 days, for 100 replications
model$simulate(step = 0:100, reps = 100)

# here, I set the infection fatality rate to 0.1
model$set_input("r", 0.1)$
  simulate()

# and setting it to a higher number causes mroe deaths:
model$set_input("r", 0.5)$
  simulate()
