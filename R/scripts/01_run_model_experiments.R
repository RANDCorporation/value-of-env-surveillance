
#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Use this file to test single model runs
#------------------------------------------------------------------------------#

source("./R/library.R")

model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

model$simulate(0:100, reps = 100)

View(model$summary)


# This is now working:
# Need to pre-process whenever changing inputs that influence pre-processed inputs.
model$set_input("r", 0.1)$
  simulate()
  simulate(0:100, reps = 500)$
  post_process()

model$summary

# Create sample parameter for the sake of demonstration.
# Other model parameters can be added in this way
model$set_param_dist(params_list = list(a = data.frame(sample_param = 1, weights = 1)), use_average = T, param_dist_weights = "weights")

# Need to set parameters after

experiment <- R6Experiment$new(model)

experiment$
  set_parameter(parameter_name = "obs_lag", experimental_design = "grid", values = c(1,5,15,30))$
  set_parameter("c", "grid", c(0,0.5,5,15,30))$
  #set_parameter("R0", "grid", c(1,2,3,4,5))$
  set_parameter("R0", "lhs", min = 1, max = 3)


# Need to set a parameters table in the model.

experiment$set_design(n_lhs = 10)


experiment$policy_design


# Run experiment in parallel:
experiment$run()


# Default way is to set input for each parameter, and run the model.
