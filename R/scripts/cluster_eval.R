#------------------------------------------------------------------------------#
# Code for "The value of environmental sampling surveillance"
# Copyright (C) 2024 by The RAND Corporation
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# See LICENSE.md and README.md for more information on usage and licensing
#
# Author: Pedro Nascimento de Lima
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

# Instantiate model -------------------------------------------------------
model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

# Create the experiment object
# This object must be named cluster_experiment, and can include multiple
# model objects if the experiment is to include multiple model specifications.
cluster_experiment <- R6Experiment$new(model)
