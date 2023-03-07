

# originally from
# http://epirecip.es/epicookbook/chapters/sir-stochastic-discretestate-discretetime/r_odin


#if (!require("drat")) install.packages("drat")
#drat:::add("mrc-ide")
#install.packages("dde")
#install.packages("odin")

library(odin)
library(dplyr)
library(ggplot2)

sir_generator <- odin::odin({
  ## Core equations for transitions between compartments:
  update(S) <- S - n_SI + n_RS
  update(I) <- I + n_SI - n_IR + d_reseeding
  update(R) <- R + n_IR - n_RS
  # Actual intervention updates with a delay
  update(NPI) <- NPI + (target_NPI - NPI) / days_to_adjust_NPI

  # Intervention level, assuming instant policy update

  # targer intervention level
  target_NPI <- min((I/N) * stringency * 100, max_intervention_level)

  print("target_NPI: {target_NPI}")

  # beta parameter is infulenced by policy dynamically
  beta_policy <- max(beta * (1-(round(NPI, 0) * beta_effect)), 0)

  ## Individual probabilities of transition:
  p_SI <- 1 - exp(-beta_policy * I / N) # S to I
  p_IR <- 1 - exp(-gamma) # I to R
  p_RS <- 1 - exp(-theta) # R to S (waning of immunity)

  # Would have to account for loss of immunity to capture oscilations.

  ## Draws from binomial distributions for numbers changing between
  ## compartments:
  n_SI <- rbinom(S, p_SI)
  n_IR <- rbinom(I, p_IR)
  n_RS <- rbinom(R, p_RS)

  ## Total population size
  N <- S + I + R

  ## Initial states:
  initial(S) <- S_ini
  initial(I) <- I_ini
  initial(R) <- 0
  initial(NPI) <- 0

  ## User defined parameters - default in parentheses:
  S_ini <- user(1000)
  I_ini <- user(1)
  beta <- user(1/5)
  gamma <- user(1/10)

  # immunity waning rate
  theta <- user(1/120)

  # inbound net migration (i.e., re-seeding)
  # people per day
  d_reseeding <- user(5)

  days_to_adjust_NPI <- user(7)

  # absolute relative reduction in the beta parameter for every intervention level
  # 0.2 here means 20% reduction in transmission for every step in the NPI intervention scale.
  beta_effect <- user(0.2)

  # increase in the intervention level for every
  stringency <- user(0)

  # maximum intervention level
  max_intervention_level <- user(5)

}, verbose = T, debug_enable = T)


set.seed(1)
sir_a <- sir_generator$new(S_ini = 100000,
                         I_ini = 10,
                         beta = 0.2,
                         beta_effect = 0.2,
                         stringency = 3,
                         days_to_adjust_NPI = 7)$
  run(0:365) %>%
  as.data.frame() %>%
  mutate(Scenario = "Policy Lag = 1 day")

set.seed(1)
sir_b <- sir_generator$new(S_ini = 100000,
                           I_ini = 10,
                           beta = 0.2,
                           beta_effect = 0.2,
                           stringency = 3,
                           days_to_adjust_NPI = 21)$
                      run(0:365) %>% as.data.frame() %>%
  mutate(Scenario = "Policy Lag = 21 days")



# Comparing one trajectory:

consolidated_long_results <-rbind(sir_a, sir_b) %>%
  mutate(NPI = round(NPI, 0)) %>%
  group_by(Scenario) %>%
  mutate(TotalCases = cumsum(I),
         TotalNPICost = cumsum(NPI) * 1) %>% # Where 1 is the NPI cost
  tidyr::pivot_longer(cols = c(S,I,R,NPI,TotalCases,TotalNPICost))



consolidated_long_results %>%
  filter(name %in% c("I", "NPI", "TotalCases", "TotalNPICost")) %>%
  ggplot(mapping = aes(x = step, y = value, color = Scenario)) +
  geom_line() +
  xlab("Days") +
  facet_wrap(facets = ~name, scales = "free") +
  randplot::theme_rand()


# Other code:



# Other code to demonstrate stochastic runs:
# par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
# matplot(res[, 1], res[, -1], xlab = "Time", ylab = "Number of individuals",
#         type = "l", col = sir_col, lty = 1)
# legend("topright", lwd = 1, col = sir_col, legend = c("S", "I", "R"), bty = "n")
#
#
# res_200 <- sir$run(0:730, replicate = 1000)
# res_200 <- sir$transform_variables(res_200)
# res_200 <- cbind.data.frame(t = res_200[[1]], res_200[-1])
#
# col <- rep(sir_col, each = 1000)
#
# par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
# matplot(res_200[, 1], res_200[, -1], xlab = "Time", ylab = "Number of individuals",
#         type = "l", col = col, lty = 1)
# legend("topright", lwd = 1, col = sir_col, legend = c("S", "I", "R"), bty = "n")
