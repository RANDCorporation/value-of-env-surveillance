
# Source all dependencies and model scripts
source("./R/library.R")


# Base case runs

# Single run examples -----------------------------------------------------
model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

model$set_input("c", 10^5)$set_input("tau", 0.15)

model$simulate(reps = 500)

get_outcome(model$summary, outcome_var = "L5_days", sig_digits = 3)

get_outcome(model$summary, outcome_var = "epi_size", sig_digits = 3)

get_outcome(model$summary, outcome_var = "deaths_per_100k", sig_digits = 3)


library(ggplot2)

model$res_long %>%
  filter(jurisdiction.id == 1) %>%
  ggplot() +
  geom_line(mapping = aes(x = step, y = NPI, group = interaction(rep, jurisdiction.id) , color = as.factor(jurisdiction.id)))

model$res_long %>%
  filter(jurisdiction.id == 1) %>%
  ggplot() +
  geom_line(mapping = aes(x = step, y = I, group = interaction(rep, jurisdiction.id), color = as.factor(jurisdiction.id)))
