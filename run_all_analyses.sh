#!/bin/bash

#------------------------------------------------------------------------------#
# Code for "The value of environmental surveillance for pandemic response"
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Run main experiment
Rscript ./R/scripts/run_main_experiment.R

# Tau (NPI effectiveness) experiment
Rscript ./R/scripts/run_tau_experiment.R

# Figures and Tables
Rscript ./R/scripts/run_tables_figures.R
