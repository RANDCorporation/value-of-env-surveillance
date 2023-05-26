
#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Use this file to test single model runs
#
#------------------------------------------------------------------------------#

source("./R/library.R")

# stochastic SIR with NPIs ------------------------------------------------

#set.seed(1234)

# Here we provide a proof of concept that the decision delay affects
# model outcomes.

# Test models
res_2_delay <- odinpbm$new("stochastic_SIR_NPIs.R", surv_delay = 2)$run(1:100, reps = 20) %>%
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
  filter(name %in% c("I", "NPI")) %>%
  ggplot(mapping = aes(x = step, y = value, color = Scenario, group = rep)) +
  geom_line() +
  xlab("Days") +
  facet_wrap(facets = ~name + Scenario)


# metapopulation ode ------------------------------------------------------


# SEIR free parameters --------------------------------------------------------#
## total number of patches in the model
nr_patches = 2
## relative migration propensity by disease status (S, P, E, I, R)
mp_S = 1
mp_P = 1
mp_E = 0.5
mp_I = 1
mp_R = 1

mp <- c(1, 1, 0.5, 1, 1)
## matrix of effective contact rates
# using the function above
#beta <- beta.mat(nr_patches)
# Or assigning the beta matrix directly
beta <- diag(1, nrow = nr_patches, ncol = nr_patches)


## Creating a symmetric mobility matrix:
# 1 % of the population travels every day in every jurisdiction
# It represents people leaving from each row, entering each
daily_travel <- 0.01

# Divide that by the number of destinations
travel_per_destination <- daily_travel / (nr_patches-1)

# Create the mobility matrix
mob <- matrix(data = daily_travel, nrow = nr_patches, ncol = nr_patches)

# The diagonal of the mobility matrix must be the number of people leaving
# So we first set it to zero:
diag(mob) <- 0

# Assign it to the Column sums
# row sums would also work because everything is symetric in this example.
# But the sum
diag(mob) <- -rowSums(mob)

# Is the mobility matrix balanced?
sum(mob) == 0

# run SEIR model --------------------------------------------------------------#
meta_SIR <- odinpbm$new("deterministic_metapopulation.R",
                        nr_patches=nr_patches, beta=beta, C=mob, mp=mp)



meta_SIR_res <- meta_SIR$run(0:100)

head(meta_SIR_res)



# stochastic metapopulation model -----------------------------------------

source("./R/library.R")

meta_SIR_stoc <- odinpbm$new("stochastic_metapopulation.R", s$data_file)

meta_SIR_stoc_res <- meta_SIR_stoc$run(0:100, reps = 100)

head(meta_SIR_stoc_res)



# setting inputs from file ------------------------------------------------

# any model can set inputs from a spreadsheet file





