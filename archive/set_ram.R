


# This script demonstrates how to run the model for a single run:

source("./R/library.R")

# Single run examples -----------------------------------------------------
model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

# all default model inputs are in the inputs object
model$inputs

# We can run the model with default inputs by using the simulate function
# i.e., run for 100 days, for 100 replications
model$simulate(step = 0:100, reps = 100)

model$set_input("coordination_mode", 2)$
  simulate()

