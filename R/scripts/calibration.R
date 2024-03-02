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


# Instantiate model

# Policy effectiveness and stringency
model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)


# Create a calibration objective functio, with some notional defaults for testing
calib_tau_c_obj_fn <- function(x = c(c = 15, tau = 0.15), target_deaths = 160, target_l5_days = 100, deaths_weight = 0.5) {
  set.seed(1234)

  model$set_input("c", x[1])$
    set_input("tau", x[2])

  model$simulate(reps = 100)

  deaths_sim <- as.numeric(model$summary_all$deaths_per_100k_.mean[1])
  l5days_sim <- as.numeric(model$summary_all$L5_days_.mean[1])


  sq_distance <- deaths_weight * (target_deaths - deaths_sim)^2 + (1 - deaths_weight) * (target_l5_days - l5days_sim)^2
  deaths_ape <- round(100 * (deaths_sim - target_deaths) / target_deaths, 2)
  l5days_ape <- round(100 * (l5days_sim - target_l5_days) / target_l5_days, 2)

  print(paste0("Params: tau: ", signif(x[2], 3), ". c: ", signif(x[1], 3)))
  print(paste0("Abs Perc Error: Deaths: ", deaths_ape, ". L5 days: ", l5days_ape))

  return(sq_distance)
}

# Test function:
calib_tau_c_obj_fn(x = c(c = 5, tau = 0.115))

# Calibrate:
set.seed(1234)

solution <- optim(
  par = c(c = 15, tau = 0.15),
  control = list(maxit = 50, abstol = 5^2),
  # method = "Brent", # "L-BFGS-B",
  fn = calib_tau_c_obj_fn,
  lower = c(5, 0.1),
  upper = c(25, 0.18),
  target_deaths = deaths_target,
  target_l5_days = median_days_near_max_intervention,
  deaths_weight = 0.5
)

# These are the solutions we use as default values for c and tau
solution$par
