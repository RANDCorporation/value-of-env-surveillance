
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

model <- odinmetapop$new("stochastic_metapopulation.R", s$data_file,obs_lag = 100)





model$simulate(0:100, reps = 100)

# Set user can be used to set individual odin inputs.
# We can then have odin or non-odin inputs, and use that as input type.
# That helps clear the confusion
model$o$set_user()

model$set_input(name = )


# So, what we can do is to override set_input such that it updates the model inputs *and* odins user inputs


model$params_df

model$set_param_dist()

# Need to set parameters after

experiment <- R6Experiment$new(model)

experiment$
  set_parameter(parameter_name = "obs_lag", experimental_design = "grid", values = c(1,5,15,30))$
  set_parameter("c", "grid", c(0,0.5,5,15,30))$
  set_parameter("R0", "grid", c(1,2,3,4,5))$
  set_parameter("R0", "lhs", min = )


# Need to set a parameters table in the model.

experiment$set_design()
