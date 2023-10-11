


#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# This script demonstrates how to run an experiment using the model

# Set up an experimental design and run the model:
source("./R/library.R")

# Instantiate model -------------------------------------------------------

model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

# base case:

# It looks like we are not achieving disease elimination because
# Elimination:
# At the near-elimination regime, a lower surveillance lag doesn't mean higher costs!
model$set_input("c", 0.01)$
  set_input("R0", 2.5)$
  set_input("p", 0.5)$
  set_input("tau", 0.18)$
  set_input("surv_lag", 1)$
  set_input("a_up", 1)$
  simulate()


# Strict and Stringent scenario
# In this scenario disease elimination is achieved


m_stringent <- model$set_input()
