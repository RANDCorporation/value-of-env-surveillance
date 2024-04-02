

source("./R/library.R")

# results list contains the results we want to save
if (!file.exists("./output/r.rds")) {
  r <- list()
} else {
  r <- readRDS("./output/r.rds")
}

msg_time("## Starting main experiment")

msg_time("Compiling model")

# 1. Set up experiments and constants ----------------------------------------

model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

model$set_param_dist(params_list = list(param_dist_a = data.frame(sample_param = 1, weights = 1)), use_average = T, param_dist_weights = "weights")

r$base_scenarios <- readxl::read_xlsx("./data/scenarios.xlsx", sheet = "one-way-scenarios")

r$outcomes <- c("deaths_per_100k", "CH_illness", "CH_deaths", "CH", "L5_days", "L1plus_days", "CNPI", "C", "epi_size")

# counterfactual scenarios without enhanced EWS
count_scenarios <- r$base_scenarios %>%
  filter(Section == "Scenarios") %>%
  mutate(total_surv_lag = 13, p = 0.3, NMB_comparator = T)

all_scenarios <- rbind(r$base_scenarios, count_scenarios)

# create experiment
base_experiment <- R6Experiment$new(model)

base_experiment$set_design(grid_design_df = all_scenarios)


## 2.1 Run main experiment -----------------------------------------------------


#cl <- snow::makeCluster(n_cores)
#doSNOW::registerDoSNOW(cl)
#snow::clusterExport(cl, "cluster_eval_script", envir = environment())
#snow::clusterEvalQ(cl, source(cluster_eval_script))

msg_time("Running main results experiment")

msg_time("this should take up to a minute with 50 replications (for testing) and about 15 minutes on a macbook pro for 1000 replications")

# Adjust settings in the settings.yml file
base_results <- base_experiment$run(
  parallel = s$parallel,
  cluster_eval_script = s$cluster_eval_script,
  n_cores = s$n_cores,
  model_from_cluster_eval = s$model_from_cluster_eval,
  reps = s$n_reps
)

msg_time("Finishing post-processing")

# Compute differences at the replication level:
comp_results <- base_results %>%
  filter(NMB_comparator) %>%
  select(rep, counterfactual.id, any_of(r$outcomes)) %>%
  clear.labels() %>%
  rename_at(vars(any_of(r$outcomes)), function(x) paste0(x, "_no_ews"))


base_res_diff <- base_results %>%
  clear.labels() %>%
  left_join(comp_results, by = join_by(rep, counterfactual.id))

base_res_diff <- base_res_diff %>%
  mutate(across(any_of(r$outcomes), ~ pull(base_res_diff, paste0(cur_column(), "_no_ews")) - ., .names = "{.col}_diff")) %>%
  rename(NMB = C_diff)


base_results_rep_summaries <- base_res_diff %>%
  select(-c(rep, grid.id, lhs.id, params_design.id, param.id, model.id, all.params.id, c, p, total_surv_lag, a_up, a_down, p, R0, r, cost_max_npi, tau, policy.exp.id)) %>%
  group_by(Scenario, Section, Class, counterfactual.id, NMB_comparator) %>%
  summarise_all(summary_functions) %>%
  ungroup() %>%
  clear.labels()

# Create posterior summaries for the parameters:
r$base_results_long <- base_results_rep_summaries %>%
  group_by(Scenario, Section, Class, counterfactual.id) %>%
  pivot_longer(cols = -c(Scenario, Section, Class, NMB_comparator, counterfactual.id), names_to = "statistic", values_to = "value") %>%
  as.data.frame() %>%
  separate(col = statistic, into = c("variable", "stat"), sep = "_\\.")


msg_time("Saving results")

saveRDS(object = r, file = "./output/r.rds")

msg_time("## Finished main experiment")
