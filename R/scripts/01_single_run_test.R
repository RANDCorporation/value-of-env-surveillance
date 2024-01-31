
#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# This script demonstrates how to run the model for a single run:

source("./R/library.R")

# Single run examples -----------------------------------------------------
model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)
model2 <- OdinMetapop$new("stochastic_metapopulation2.R", "./data/metapopulation-two-strata.xlsx")

# all default model inputs are in the inputs object
model$inputs$beta
model2$inputs$beta

# IT looks like the mixing is not being partitioned correctly!
#. it should be 0.1, 0.1

# We can run the model with default inputs by using the simulate function
# i.e., run for 100 days, for 100 replications
model$simulate(step = 0:50, reps = 100)
model2$simulate(step = 0:50, reps = 100)

# Compare both models:
res1 <- model$res_long %>%
  group_by(rep, step) %>%
  summarise(I = sum(E + P + I)) %>%
  mutate(model = "one strata")

res2 <- model2$res_long %>%
  group_by(rep, step) %>%
  summarise(I = sum(E + P + I)) %>%
  mutate(model = "two strata")

all_res <-rbind(res1, res2)

rep_model <- all_res %>%
  select(rep, model) %>%
  distinct() %>%
  mutate(rep_model_id = row_number())

all_res %>%
  #left_join(rep_model) %>%
  ggplot() +
  geom_line(mapping = aes(x = step, y = I, group = interaction(model, rep), color = model)) #+
  #facet_wrap(~model)




model$inputs$R
model$inputs$beta

View(model$res_long %>% mutate(E_perc = E/(E+P+I), P_perc = P/(E+P+I), I_perc = I/(E+P+I)))

# Compute R0 from the first 60 days:
cases_ts <- model$res_long %>%
  filter(jurisdiction.id == 1) %>%
  group_by(rep, jurisdiction.id) %>%
  mutate(cases = -c(0,diff(S))) %>%
  group_by(rep, step) %>%
  summarise(cases = sum(cases)) %>%
  #dplyr::filter(step >= 30 & step <= 60) %>%
  mutate(log_outcome = log(cases))


library(ggplot2)


r_model <- lm(formula = log_outcome ~ step, data = cases_ts)$coefficients["step"]

tau.eff <- (1/model$inputs$delta + 1/model$inputs$gamma)

R0 <- 1 + r_model * tau.eff

R0

# Now we are at the ballpark, so I think I am correct.

# Tau is bigger than tau.eff. Tau is closer to 11 while tau.eff is closer to 6.

# Maybe that is one source of issues.





# here, I set the infection fatality rate to 0.1
model$set_input("r", 0.1)$
  simulate()

# and setting it to a higher number causes more deaths:
model$set_input("r", 0.5)$
  simulate()
