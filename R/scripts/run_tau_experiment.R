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

# Source all dependencies and model scripts
source("./R/library.R")

# results list contains the results we want to save
if (!file.exists("./output/r.rds")) {
  r <- list()
} else {
  r <- readRDS("./output/r.rds")
}


msg_time("## Starting NPI effectiveness (i.e., tau) experiment")

msg_time("Compiling model")

# 1. Set up experiments and constants ----------------------------------------

model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

model$set_param_dist(params_list = list(param_dist_a = data.frame(sample_param = 1, weights = 1)), use_average = T, param_dist_weights = "weights")

# tau vs R0 experimental design

tau <- c(seq.default(from = 0, to = 1, by = 0.025) / 5)
R0 <- 2.5 * c(0.5, 1, 1.5)

EWS <- c(T, F)
tau_design <- expand_grid(tau, EWS, R0) %>%
  mutate(p = ifelse(EWS, 1, 0.3)) %>%
  mutate(total_surv_lag = ifelse(EWS, 8, 13))

tau_experiment <- R6Experiment$new(model)

tau_experiment$set_design(grid_design_df = tau_design)

msg_time("Running tau experiment")

tau_results <- tau_experiment$run(
  parallel = s$parallel,
  cluster_eval_script = s$cluster_eval_script,
  n_cores = s$n_cores,
  model_from_cluster_eval = s$model_from_cluster_eval,
  reps = s$n_reps,
  seed = s$seed
)

msg_time("Post-processing tau experiment.")

r$fig_effectiveness_data <- tau_results %>%
  mutate(effectiveness = tau * 5) %>%
  select(effectiveness, EWS, any_of(r$outcomes)) %>%
  group_by(effectiveness, EWS) %>%
  summarise_all(summary_functions) %>%
  mutate(EWS = ifelse(EWS, "NPIs w/ EWS", "NPIs w/o EWS"))

# Comparator scenarios without EWS
tau_comp <- tau_results %>%
  filter(!EWS) %>%
  select(rep, tau, R0, C) %>%
  rename(C_no_EWS = C)

# Net monetary benefit
tau_NMB <- tau_results %>%
  filter(EWS) %>%
  left_join(tau_comp, by = join_by(rep, tau, R0)) %>%
  mutate(effectiveness = tau * 5) %>%
  mutate(NMB = C_no_EWS - C)

# Net monetary benefit summary stats
r$tau_NMB_summary <- tau_NMB %>%
  group_by(effectiveness, R0, EWS) %>%
  select(effectiveness, R0, EWS, NMB, C) %>%
  summarise_all(summary_functions) %>%
  mutate(EWS = ifelse(EWS, "NPIs w/ EWS", "NPIs w/o EWS"))

msg_time("Wrapping up and saving results.")

saveRDS(object = r, file = "./output/r.rds")
