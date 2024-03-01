

#------------------------------------------------------------------------------#
# Code for "The value of environmental surveillance for pandemic response"
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# This script demonstrates how to run an experiment using the model

# Set up an experimental design and run the model:
source("./R/library.R")


scenarios <- readxl::read_xlsx("./data/scenarios.xlsx",sheet = "policy_profile")


# Instantiate model -------------------------------------------------------

model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)


all_long_res <- list()

# Run the couple of scenarios and recover long results, under baseline assumptions

for(i in 1:nrow(scenarios)) {

  # Set inputs:
  for(var in colnames(scenarios)) {

    model$set_input(name = var, value = scenarios[[i,var]])

  }

  model$simulate(reps = 200)

  all_long_res[[i]] <- model$res_long

  all_long_res[[i]]$policy_profile <- scenarios$policy_profile[i]

}


all_long_df <- do.call(rbind, all_long_res)


# Strict and Stringent scenario
# In this scenario disease elimination is achieved

reps_data <- all_long_df %>%
  mutate(L_eff = floor(L)) %>%
  filter(rep %in% 1:3) %>%
  filter(jurisdiction.name == "NY")


fig_intlevel <- reps_data %>%
  ggplot(mapping = aes(x = step, group = rep, color = as.factor(rep))) +
  geom_line(mapping = aes(y = NPI)) +
  geom_line(mapping = aes(y = L), alpha = 0.2) +
  facet_wrap(~policy_profile, nrow = 1) +
  ylab("Intervention level") +
  xlab("Days from pandemic onset")


fig_prevalence <- reps_data %>%
  ggplot(mapping = aes(x = step, group = rep, color = as.factor(rep))) +
  geom_line(mapping = aes(y = I / population)) +
  facet_wrap(~policy_profile, nrow = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("Prevalence") +
  xlab("Days from pandemic onset")


fig_intlevel / fig_prevalence


# Figures with density


fig_prevalence_fan <- all_long_df %>%
  filter(jurisdiction.name == "NY") %>%
  ggplot(mapping = aes(x = step, y = I / population)) +
  ggfan::geom_fan() +
  facet_wrap(~policy_profile, nrow = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("Prevalence") +
  xlab("Days")


fig_deaths_fan <- all_long_df %>%
  filter(jurisdiction.name == "NY") %>%
  ggplot(mapping = aes(x = step, y = deaths_per_100k)) +
  ggfan::geom_fan() +
  facet_wrap(~policy_profile, nrow = 1) +
  ylab("Deaths per 100,000 people") +
  xlab("Days")

fig_intlevel_fan <- all_long_df %>%
  filter(jurisdiction.name == "NY") %>%
  ggplot(mapping = aes(x = step, y = NPI, group = rep)) +
  geom_line() +
  facet_wrap(~policy_profile, nrow = 1) +
  ylab("Intervention level") +
  xlab("Days from pandemic onset")


patch_plot <- fig_prevalence_fan / fig_intlevel_fan

ggsave("./output/patch.png", units = "in", width = 21, height = 9)
