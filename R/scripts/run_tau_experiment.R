

# Source all dependencies and model scripts
source("./R/library.R")

# results list contains the results we want to save
if (!file.exists("./output/r.rds")) {
  r <- list()
} else {
  r <- readRDS("./output/r.rds")
}


msg_time("## Starting NPI effectiveness (i.e., tau) experiment")

msg_time("Compiling model")

# 1. Set up experiments and constants ----------------------------------------

model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

model$set_param_dist(params_list = list(param_dist_a = data.frame(sample_param = 1, weights = 1)), use_average = T, param_dist_weights = "weights")

# tau vs R0 experimental design

tau <- c(seq.default(from = 0, to = 1, by = 0.025) / 5)
R0 <- 2.5 * c(0.5, 1, 1.5)

EWS <- c(T, F)
tau_design <- expand_grid(tau, EWS, R0) %>%
  mutate(p = ifelse(EWS, 1, 0.3)) %>%
  mutate(total_surv_lag = ifelse(EWS, 8, 13))

tau_experiment <- R6Experiment$new(model)

tau_experiment$set_design(grid_design_df = tau_design)

msg_time("Running tau experiment")

msg_time("This should take up to a minute to run for testing, but around 30 minutes to run on a macbookpro 2022.")

tau_results <- tau_experiment$run(
  parallel = s$parallel,
  cluster_eval_script = s$cluster_eval_script,
  n_cores = s$n_cores,
  model_from_cluster_eval = s$model_from_cluster_eval,
  reps = s$n_reps
)

msg_time("Post-processing tau experiment.")

r$fig_effectiveness_data <- tau_results %>%
  mutate(effectiveness = tau * 5) %>%
  select(effectiveness, EWS, any_of(r$outcomes)) %>%
  group_by(effectiveness, EWS) %>%
  summarise_all(summary_functions) %>%
  mutate(EWS = ifelse(EWS, "NPIs w/ EWS", "NPIs w/o EWS"))

# Comparator scenarios without EWS
tau_comp <- tau_results %>%
  filter(!EWS) %>%
  select(rep, tau, R0, C) %>%
  rename(C_no_EWS = C)

# Net monetary benefit
tau_NMB <- tau_results %>%
  filter(EWS) %>%
  left_join(tau_comp, by = join_by(rep, tau, R0)) %>%
  mutate(effectiveness = tau * 5) %>%
  mutate(NMB = C_no_EWS - C)

# Net monetary benefit summary stats
r$tau_NMB_summary <- tau_NMB %>%
  group_by(effectiveness, R0, EWS) %>%
  select(effectiveness, R0, EWS, NMB, C) %>%
  summarise_all(summary_functions) %>%
  mutate(EWS = ifelse(EWS, "NPIs w/ EWS", "NPIs w/o EWS"))

msg_time("Wrapping up and saving results.")

saveRDS(object = r, file = "./output/r.rds")
