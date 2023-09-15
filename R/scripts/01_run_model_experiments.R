
#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Specify and run model experiments

source("./R/library.R")

# Instantiate model -------------------------------------------------------

model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

# Run full-factorial experiment -------------------------------------------

# Create sample parameter set
# In a fully probabilistic analysis, one would set a posterior distribution in this way
model$set_param_dist(params_list = list(a = data.frame(sample_param = 1, weights = 1)), use_average = T, param_dist_weights = "weights")

# Instantiate an experiment passing the model - multiple models can be passed as well
experiment <- R6Experiment$new(model)

# Set experimental parameters
experiment$
  set_parameter(parameter_name = "obs_lag", experimental_design = "grid", values = c(1,5,30))$
  set_parameter("c", "grid", c(0,5,30))$
  set_parameter("R0", "grid", c(1,2,3,4))

# Set designs creates the experimental design data.frame:
# The policy design is in the experiment$policy_design object
experiment$set_design()

# Run experiment (parallel option not available with odin at this time):
exp_results <- experiment$run(parallel = F)

# Write experiment results:
#writexl::writexlsx(exp)