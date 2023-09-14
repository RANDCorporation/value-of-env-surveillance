
#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

source("./R/library.R")


# Single run examples -----------------------------------------------------


model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

# run the model for default inputs:
model$simulate(0:100, reps = 100)


# Change one input manually and run the model:
model$set_input("r", 0.1)$
  simulate()


# Run full-factorial experiment -------------------------------------------

# Create sample parameter for the sake of demonstration.
# Other model parameters can be added in this way
model$set_param_dist(params_list = list(a = data.frame(sample_param = 1, weights = 1)), use_average = T, param_dist_weights = "weights")


# Instantiate the experiment passing the model - multiple models can be passed as well:
experiment <- R6Experiment$new(model)

# Set experimental parameters.
experiment$
  set_parameter(parameter_name = "obs_lag", experimental_design = "grid", values = c(1,5,30))$
  set_parameter("c", "grid", c(0,5,30))$
  set_parameter("R0", "grid", c(1,2,3,4))


# Set designs creates the experimental design data.frame:
experiment$set_design()

# See the experimental design:
experiment$policy_design

# Run experiment (parallel option not available with odin at this time):
exp_results <- experiment$run(parallel = F)

