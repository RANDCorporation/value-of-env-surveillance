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

# Run main results experiments

source("./R/library.R")

# results list contains the results we want to save
if (!file.exists("./output/r.rds")) {
  r <- list()
} else {
  r <- readRDS("./output/r.rds")
}

msg_time("## Starting main experiment")

msg_time("Compiling model")

# 1. Set up experiments and constants ----------------------------------------

model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

model$set_param_dist(params_list = list(param_dist_a = data.frame(sample_param = 1, weights = 1)), use_average = T, param_dist_weights = "weights")

r$base_scenarios <- readxl::read_xlsx("./data/scenarios.xlsx", sheet = "one-way-scenarios")

r$outcomes <- c("deaths_per_100k", "CH_illness", "CH_deaths", "CH", "L5_days", "L1plus_days", "CNPI", "C", "epi_size")

# counterfactual scenarios without enhanced EWS
count_scenarios <- r$base_scenarios %>%
  filter(Section == "Scenarios") %>%
  mutate(total_surv_lag = 13, p = 0.3, NMB_comparator = T)

all_scenarios <- rbind(r$base_scenarios, count_scenarios)

# create experiment
base_experiment <- R6Experiment$new(model)

base_experiment$set_design(grid_design_df = all_scenarios)


## 2.1 Run main experiment -----------------------------------------------------


# cl <- snow::makeCluster(n_cores)
# doSNOW::registerDoSNOW(cl)
# snow::clusterExport(cl, "cluster_eval_script", envir = environment())
# snow::clusterEvalQ(cl, source(cluster_eval_script))

msg_time("Running main results experiment")

# Adjust settings in the settings.yml file
base_results <- base_experiment$run(
  parallel = s$parallel,
  cluster_eval_script = s$cluster_eval_script,
  n_cores = s$n_cores,
  model_from_cluster_eval = s$model_from_cluster_eval,
  reps = s$n_reps_main,
  seed = s$seed
)

msg_time("Finishing post-processing")

# Compute differences at the replication level:
comp_results <- base_results %>%
  filter(NMB_comparator) %>%
  select(rep, counterfactual.id, any_of(r$outcomes)) %>%
  clear.labels() %>%
  rename_at(vars(any_of(r$outcomes)), function(x) paste0(x, "_no_ews"))


base_res_diff <- base_results %>%
  clear.labels() %>%
  left_join(comp_results, by = join_by(rep, counterfactual.id))

base_res_diff <- base_res_diff %>%
  mutate(across(any_of(r$outcomes), ~ pull(base_res_diff, paste0(cur_column(), "_no_ews")) - ., .names = "{.col}_diff")) %>%
  rename(NMB = C_diff)


base_results_rep_summaries <- base_res_diff %>%
  select(-c(rep, grid.id, lhs.id, params_design.id, param.id, model.id, all.params.id, c, p, total_surv_lag, a_up, a_down, p, R0, r, cost_max_npi, tau, policy.exp.id)) %>%
  group_by(Scenario, Section, Class, counterfactual.id, NMB_comparator) %>%
  summarise_all(summary_functions) %>%
  ungroup() %>%
  clear.labels()

# Create posterior summaries for the parameters:
r$base_results_long <- base_results_rep_summaries %>%
  group_by(Scenario, Section, Class, counterfactual.id) %>%
  pivot_longer(cols = -c(Scenario, Section, Class, NMB_comparator, counterfactual.id), names_to = "statistic", values_to = "value") %>%
  as.data.frame() %>%
  separate(col = statistic, into = c("variable", "stat"), sep = "_\\.")


msg_time("Saving results")

saveRDS(object = r, file = "./output/r.rds")

msg_time("## Finished main experiment")
