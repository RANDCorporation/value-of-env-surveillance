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


# Use the code below to calibrate tau and the policy stringency (c)

# Source all dependencies and model scripts
source("./R/library.R")

# This script is used to calibrate parameters tau and c,
# tau: marginal effectiveness of NPIs (in reducing transmission)
# c: disease incidence threshold used to introduce intervention

# Here, the expectation is that the number of days under maximum intervention level
# i.e., near a "lockdown" helps identify "c" and the number of deaths helps identify tau.
# For this study, I use a simple Nelder-mead algorithm.
# I find tau ~ 0.14, right in the middle of our expected range of 0.12 to 0.18.
# And c is ~ 17, which is necessary to reproduce the number of days under the maximum intervention level.

# Set targets:

# And median deaths per 100k during first year of pandemic
# Across US states
augm_inputs <- readRDS("./data/archive/augm_inputs.rds")

# Deaths target:
# Average deaths per 100,000 population across US states:
deaths_target <- augm_inputs$locationtimeseries %>%
  filter(Date <= as.Date("2021-03-01")) %>%
  group_by(LocationID) %>%
  summarise(DeathsPer100K = sum(DeathsPer100K)) %>%
  .$DeathsPer100K %>%
  mean()

# Number of days at maximum intervention level is computed
# From Oxford's COVID-19 policy tracking project.

# Data from:
# https://github.com/OxCGRT/covid-policy-tracker/tree/master
# Citation:  https://doi.org/10.1038/s41562-021-01079-8

median_days_near_max_intervention <- read.csv("./data/OxCGRT_USA_latest.txt") %>%
  dplyr::filter(Jurisdiction == "STATE_TOTAL") %>%
  mutate(day = lubridate::as_date(as.character(Date))) %>%
  filter(day <= as.Date("2021-03-01"), day >= as.Date("2020-03-01")) %>%
  select(RegionName, Date, StringencyIndex_Average) %>%
  group_by(RegionName) %>%
  mutate(max_stringency = max(StringencyIndex_Average)) %>%
  mutate(max_intervention = StringencyIndex_Average >= (max_stringency * (1 - (1 / 5)))) %>%
  summarise(days_max_intervention = sum(max_intervention), max_stringency = mean(max_stringency)) %>%
  .$days_max_intervention %>%
  median()


# priors
priors <- imabc::define_priors(
  c = add_prior(
    parameter_name = "c",
    dist_base_name = "unif",
    min = 10,
    max = 25
  ),
  tau = add_prior(
    parameter_name = "tau",
    dist_base_name = "unif",
    min = 0.1,
    max = 0.2
  )
)

# targets
target_df <- data.frame(target_names = c("deaths", "days_max_intervention"), targets = c(deaths_target, median_days_near_max_intervention)) %>%
  mutate(
    current_lower_bounds = 0.1 * targets,
    current_upper_bounds = 2 * targets,
    stopping_lower_bounds = 0.975 * targets,
    stopping_upper_bounds = 1.025 * targets,
    target_groups = paste0(target_names, "_group"),
    scales = 1
  )

targets_imabc = imabc::as.targets(target_df)

# instantiating model in global environment, assumes sequential runs:
model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)


# target_fn
target_function <- function(c, tau) {
  model$set_input("c", c)$
    set_input("tau", tau)

  # Can perform this with multiple replications:
  # Seed can be passed here:
  model$simulate(reps = 1, set_seed = F)

  return(c(deaths = as.numeric(model$summary_all$deaths_per_100k_.mean[1]),
           days_max_intervention = as.numeric(model$summary_all$L5_days_.mean[1])))

}

# Model seems deterministic
target_function(c = 15, tau = 0.15)

# imabc call
imabc_target_fun <- imabc::define_target_function(targets = targets_imabc,priors = priors, FUN = target_function, use_seed = FALSE)

# posterior distribution

imabc_results <- imabc(
  # improve_method = "direct",
  priors = priors,
  targets = targets_imabc,
  target_fun = imabc_target_fun,
  seed = 54321,
  N_start = 100,
  max_iter = 20,
  #max_fail_iter = 5,
  N_centers = 3,
  Center_n = 100,
  N_cov_points = 50,
  N_post = 500#,
  #output_directory = "./imabc-results"
)

# Save posterior:

write.csv(imabc_results$good_parm_draws, file = "./output/posterior.csv")


# Visualize posterior:
# Set posterior
model$set_param_dist(params_list = list(a = as.data.frame(imabc_results$good_parm_draws)),
                     param_dist_weights = "sample_wt",
                     cols_to_ignore = c("iter", "draw", "step", "seed"),
                     #use_average = T #,
                     n_sample = 500
                     )

# Summarise calibration results:


calibration_summaries <- model$params_df %>%
  select(param_dist.df.id,c,tau) %>%
  group_by(param_dist.df.id) %>%
  summarise_all(summary_functions) %>%
  ungroup() %>%
  clear.labels()

# Create posterior summaries for the parameters:
calibration_pretty_summary <- calibration_summaries %>%
  group_by(param_dist.df.id) %>%
  pivot_longer(cols = -c(param_dist.df.id), names_to = "statistic", values_to = "value") %>%
  as.data.frame() %>%
  separate(col = statistic, into = c("variable", "stat"), sep = "_\\.") %>%
  mutate(value = gt::vec_fmt_number(value, n_sigfig = 3, use_seps = T)) %>%
  pivot_wider(id_cols = c(param_dist.df.id, variable), names_from = stat, values_from = value) %>%
  mutate(estimate = paste0(mean, " (", lower, "-", upper, ")"))


calibration_pretty_summary


library(ggplot2)

# Visualizing posterior - c and tau correlated as expected:
model$params_df %>%
  ggplot(mapping = aes(x = c, y = tau * 100)) +
  ggdensity::geom_hdr() +
  geom_point() +
  ylab("Marginal intervention effectiveness (tau, percent)") +
  xlab("Intervention threshold (c, cases per 100,000)")


