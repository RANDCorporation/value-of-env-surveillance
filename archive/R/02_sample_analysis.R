#------------------------------------------------------------------------------#
# Code for "The value of environmental surveillance for pandemic response"
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# This script demonstrates how to run an experimental design using the model

# Set up an experimental design and run the model:
source("./R/library.R")

# Instantiate model -------------------------------------------------------

model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

# Run full-factorial experiment -------------------------------------------

# Create sample parameter set
# This is here to demonstrate that data.frame with parameter sets (i.e., a
# sample from the posterior distribution of model parameters)
# can be assigned to the model. This is required for R6Sim models that
# are used within the R6Experiment class, but not necessary otherwise.
model$set_param_dist(params_list = list(param_dist_a = data.frame(sample_param = 1, weights = 1)), use_average = T, param_dist_weights = "weights")

# Instantiate an experiment

# Note that if the experiment is to be run in parallel, there is an underlying
# assumption that the model will be instantiated in the cluster eval
# script
experiment <- R6Experiment$new(model)

# Set experimental parameters
experiment$
  set_parameter("total_surv_lag", "grid", seq.default(from = 0, to = 21, by = 3))$ # Surveillance lag
  set_parameter("p", "grid", c(0.5, 1))$ # case ascertainment proportion
  set_parameter("tau", "grid", c(0.12, 0.15, 0.18))$ # intevention effectiveness per intervention level
  set_parameter("c", "grid", c(0.01, 1, 5, 10, 100))$ # case intervention threshold per 100,000 people
  set_parameter("R0", "grid", c(1.5, 2, 2.5))$ # Initial reproduction number
  set_parameter("cost_max_npi", "grid", c(0.1, 0.25, 0.87))$ # Initial reproduction number
  set_parameter("L_c", "grid", c(0, 1)) # Turn intervention coordination on and off


# Set designs creates the experimental design data.frame
experiment$set_design()

# Run experiment (parallel option not available with odin at this time):
exp_results <- experiment$run(parallel = T, cluster_eval_script = "./R/scripts/sample_analysis_cluster_eval.R", n_cores = parallel::detectCores() - 3, model_from_cluster_eval = T)


# Save output (but do not push this file):

date_time <- paste0(gsub(pattern = ":| |-", replacement = "_", Sys.time()), "_", gsub("/", "_", Sys.timezone()))

write.csv(exp_results, paste0("./output/", "exp_results_", date_time, ".csv"), row.names = F)
