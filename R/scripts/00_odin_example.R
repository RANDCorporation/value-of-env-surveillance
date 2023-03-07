

# originally from
# http://epirecip.es/epicookbook/chapters/sir-stochastic-discretestate-discretetime/r_odin


#if (!require("drat")) install.packages("drat")
#drat:::add("mrc-ide")
#install.packages("dde")
#install.packages("odin")

library(odin)


sir_generator <- odin::odin({
  ## Core equations for transitions between compartments:
  update(S) <- S - n_SI
  update(I) <- I + n_SI - n_IR + d_reseeding
  update(R) <- R + n_IR

  # Intervention level, assuming instant policy update
  target_NPI <- min((I/N) * stringency * 1000, max_intervention_level)

  update(NPI) <- round((target_NPI - NPI) / days_to_adjust_NPI, 0)

  beta_policy <- max(beta * (1-(NPI * beta_effect)), 0)

  ## Individual probabilities of transition:
  p_SI <- 1 - exp(-beta_policy * I / N) # S to I
  p_IR <- 1 - exp(-gamma) # I to R

  ## Draws from binomial distributions for numbers changing between
  ## compartments:
  n_SI <- rbinom(S, p_SI)
  n_IR <- rbinom(I, p_IR)

  ## Total population size
  N <- S + I + R

  ## Initial states:
  initial(S) <- S_ini
  initial(I) <- I_ini
  initial(R) <- 0
  initial(NPI) <- 0

  #print("NPI: {NPI}")

  ## User defined parameters - default in parentheses:
  S_ini <- user(1000)
  I_ini <- user(1)
  beta <- user(0.2)
  gamma <- user(0.1)

  # inbound net migration (i.e., re-seeding)
  # people per day
  d_reseeding <- user(5)

  days_to_adjust_NPI <- user(10)

  # absolute relative reduction in the beta parameter for every intervention level
  # 0.05 means 0.05 reduction in the beta for every intervention level
  beta_effect <- user(0.2)

  # increase in the intervention level for every
  stringency <- user(0)

  # maximum intervention level
  max_intervention_level <- user(5)

}, verbose = FALSE)

sir <- sir_generator$new(S_ini = 10000, I_ini = 10, beta = 0.1, beta_effect = 0.15, stringency = 2)


set.seed(1)
sir_col <- c("#8c8cd9", "#cc0044", "#999966")

res <- sir$run(0:900)
par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
matplot(res[, 1], res[, -1], xlab = "Time", ylab = "Number of individuals",
        type = "l", col = sir_col, lty = 1)
legend("topright", lwd = 1, col = sir_col, legend = c("S", "I", "R"), bty = "n")










res_200 <- sir$run(0:100, replicate = 1000)
res_200 <- sir$transform_variables(res_200)
res_200 <- cbind.data.frame(t = res_200[[1]], res_200[-1])

col <- rep(sir_col, each = 1000)

par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
matplot(res_200[, 1], res_200[, -1], xlab = "Time", ylab = "Number of individuals",
        type = "l", col = col, lty = 1)
legend("topright", lwd = 1, col = sir_col, legend = c("S", "I", "R"), bty = "n")
