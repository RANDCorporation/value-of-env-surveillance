

#------------------------------------------------------------------------------#
# Code for "The value of environmental surveillance for pandemic response"
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# This script instantiates the cluster_experiment object
# and is invoked within the R6Sim package within each "node" when running
# the experiment in parallel

# We need to do this to be able to run odin models in parallel without running
# into C++ memory issues.
# To the best of my knowledge, this may be the only way to run odin models
# in parallel in a single machine.

# Source necessary functions:
source("./R/library.R")

#set.seed(1234)

# Instantiate model -------------------------------------------------------
model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

# Create the experiment object
# This object must be named cluster_experiment, and can include multiple
# model objects if the experiment is to include multiple model specifications.
cluster_experiment <- R6Experiment$new(model)
