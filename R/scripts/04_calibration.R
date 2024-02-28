
# Source all dependencies and model scripts
source("./R/library.R")

# results list

# Policy effectiveness and stringency
model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)


calib_tau_c_obj_fn <- function(x = c(7.6, 0.125), target_deaths = 160, target_l5_days = 240, deaths_weight = 0.5) {

  set.seed(1234)

  model$set_input("c", x[1])$
        set_input("tau", x[2])

  model$simulate(reps = 100)

  deaths_sim <- as.numeric(model$summary_all$deaths_per_100k_.mean[1])
  l5days_sim <- as.numeric(model$summary_all$L5_days_.mean[1])


  sq_distance <- deaths_weight * (target_deaths - deaths_sim)^2 + (1-deaths_weight) * (target_l5_days - l5days_sim)^2
  deaths_ape <- round(100*(deaths_sim - target_deaths)/target_deaths, 2)
  l5days_ape <- round(100*(l5days_sim - target_l5_days)/target_l5_days, 2)

  print(paste0("Params: tau: ", signif(x[2], 3), ". c: ", signif(x[1], 3)))
  print(paste0("Abs Perc Error: Deaths: ", deaths_ape, ". L5 days: ", l5days_ape))

  return(sq_distance)

  }

# Test function:
calib_tau_c_obj_fn(x = c(c = 5, tau = 0.115))

# Calibrate:
solution <- optim(par = c(c = 10, tau = 0.137),
                  control = list(maxit = 50, abstol = 5^2),
                  #method = "Brent", # "L-BFGS-B",
                  fn = calib_tau_c_obj_fn,
                  lower = c(5, 0.1),
                  upper = c(15, 0.18),
                  target_deaths = 160,
                  target_l5_days = 240,
                  deaths_weight = 0.5)



solution$par

