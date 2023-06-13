
#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Use this file to test single model runs
#------------------------------------------------------------------------------#

source("./R/library.R")

# stochastic metapopulation model -----------------------------------------

meta_SIR_stoc <- odinmetapop$new("stochastic_metapopulation.R", s$data_file,obs_lag = 5)

meta_SIR_stoc$run(0:100, reps = 100)

meta_SIR_stoc$post_process()


summary(meta_SIR_stoc$summary)

View(meta_SIR_stoc_res)




head(meta_SIR_stoc_res)








# deterministic metapopulation ode ----------------------------------------

meta_SIR <- odinmetapop$new("deterministic_metapopulation.R", s$data_file)

meta_SIR_res <- meta_SIR$run(0:100)

head(meta_SIR_res)

# 1-strata stochastic SIR with NPIs ---------------------------------------

# This is a test model
#set.seed(1234)

# Here we provide a proof of concept that the decision delay affects
# model outcomes.

# Test models
res_2_delay <- odinpbm$new("stochastic_SIR_NPIs.R", surv_delay = 2
)$run(1:100, reps = 20) %>%
  as.data.frame() %>%
  mutate(Scenario = "02-day delay")

res_5_delay <- odinpbm$new("stochastic_SIR_NPIs.R", surv_delay = 5)$run(1:100, reps = 20) %>%
  as.data.frame() %>%
  mutate(Scenario = "05-day delay")

res_10_delay <- odinpbm$new("stochastic_SIR_NPIs.R", surv_delay = 10)$run(1:100, reps = 20) %>%
  as.data.frame() %>%
  mutate(Scenario = "10-day delay")


rbind(res_2_delay, res_5_delay, res_10_delay) %>%
  as.data.frame() %>%
  group_by(Scenario, rep) %>%
  mutate(NPI = round(NPI, 0)) %>%
  mutate(TotalCases = cumsum(I),
         TotalNPICost = cumsum(NPI) * 1) %>% # Where 1 is the NPI cost
  tidyr::pivot_longer(cols = c(S,I,R,NPI,TotalCases,TotalNPICost)) %>%
  filter(name %in% c("I")) %>%
  ggplot(mapping = aes(x = step, y = value, color = Scenario, group = rep)) +
  geom_line() +
  xlab("Days") +
  facet_wrap(facets = ~name + Scenario)

