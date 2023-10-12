


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
model$set_input("c", 0.001)$
  set_input("R0", 2.5)$
  set_input("p", 1)$
  set_input("tau", 0.18)$
  set_input("surv_lag", 1)$
  set_input("cost_max_npi", 0.25)$
  set_input("L_c", 1)$
  set_input("a_up", 1)$
  set_input("a_down", 45)$
  simulate()


#model$summary %>% View()


#model$res_long %>% View()

# Strict and Stringent scenario
# In this scenario disease elimination is achieved

reps_data <- model$res_long %>%
  mutate(L_eff = floor(L)) %>%
  filter(rep %in% 1:3)


fig_intlevel <- reps_data %>%
  ggplot(mapping = aes(x = step, group = rep, color = as.factor(rep))) +
  geom_line(mapping = aes(y = NPI)) +
  geom_line(mapping = aes(y = L), alpha = 0.2) +
  facet_wrap(~jurisdiction.name) +
  ylab("Intervention level") +
  xlab("Days from pandemic onset")


fig_prevalence <- reps_data %>%
  ggplot(mapping = aes(x = step, group = rep, color = as.factor(rep))) +
  geom_line(mapping = aes(y = (E + I + P) / population)) +
  facet_wrap(~jurisdiction.name) +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("Prevalence") +
  xlab("Days from pandemic onset")


fig_intlevel / fig_prevalence

