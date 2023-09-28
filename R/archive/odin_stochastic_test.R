



library(odin)
library(dplyr)
library(ggplot2)

sir_generator <- odin::odin({

  # I don't know how to access a time-step variable, so I'm creating a stock variable
  update(Time) <- Time + 1

  ## Core equations for transitions between compartments:
  update(S) <- S - n_SI + n_RS
  update(I) <- I + n_SI - n_IR + n_re_seeding
  update(R) <- R + n_IR - n_RS
  update(TimeLastNPIChange) <- if(new_NPI == NPI) TimeLastNPIChange else Time


  ## Individual probabilities of transition:
  p_SI <- 1 - exp(-beta_policy * I / N) # S to I
  p_IR <- 1 - exp(-gamma) # I to R

  # Assume complete loss of immunity
  p_RS <- 1 - exp(-theta) # R to S (waning of immunity)

  ## Draws from binomial distributions for numbers changing between
  ## compartments:
  n_SI <- rbinom(S, p_SI)
  n_IR <- rbinom(I, p_IR)
  n_RS <- rbinom(R, p_RS)

  # The new case importation follows a poisson distribution

  n_re_seeding <- rpois(d_reseeding)

  ## Total population size
  N <- S + I + R

  ## Initial states:
  initial(S) <- S_ini
  initial(I) <- I_ini
  initial(R) <- 0
  initial(NPI) <- 0
  initial(Time) <- 0
  initial(TimeLastNPIChange) <- 0

  ## User defined parameters - default in parentheses:
  S_ini <- user(1000)
  I_ini <- user(1)
  beta <- user(1/5)
  gamma <- user(1/10)

  # immunity waning rate
  theta <- user(1/180)

  # inbound net migration (i.e., re-seeding)
  # people per day entering with an infection
  d_reseeding <- user(0)

  a <- user(7)

  # absolute relative reduction in the beta parameter for every intervention level
  # 0.2 here means 20% reduction in transmission for every step in the NPI intervention scale.
  beta_effect <- user(0.2)

  # increase in the intervention level for every
  stringency <- user(0)

  # maximum intervention level
  max_intervention_level <- user(5)

}, verbose = T, debug_enable = T, workdir = "./cpp")
