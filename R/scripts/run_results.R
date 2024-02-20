
# Source all dependencies and model scripts
source("./R/library.R")

r <- list()

# Base case runs
model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

model$set_param_dist(params_list = list(param_dist_a = data.frame(sample_param = 1, weights = 1)), use_average = T, param_dist_weights = "weights")

base_scenarios <- readxl::read_xlsx("./data/scenarios.xlsx", sheet = "base-case")

base_experiment <- R6Experiment$new(model)

base_experiment$set_design(grid_design_df = base_scenarios)

base_results <- base_experiment$run(parallel = T,
                                    cluster_eval_script = "./R/scripts/cluster_eval.R",
                                    parallel::detectCores() - 3,
                                    model_from_cluster_eval = T)


# Create posterior summaries for the parameters:
r$base_results_long <- base_results %>%
  select(-c(grid.id, lhs.id, params_design.id, param.id, model.id, all.params.id, c, p, total_surv_lag, a_up, a_down, policy.exp.id)) %>%
  group_by(Scenario) %>%
  pivot_longer(cols = -Scenario, names_to = "statistic", values_to = "value") %>%
  as.data.frame() %>%
  separate(col = statistic,into = c("variable", "stat"), sep = "_\\.")

# table_param_summaries
r$base_results_summary_long <- r$base_results_long %>%
  #mutate(value = signif(value, digits = 3)) %>%
  mutate(value = gt::vec_fmt_number(value, n_sigfig = 3, use_seps = T)) %>%
  #mutate(value = formatC(value, format="f", big.mark=",")) %>%
  #mutate(value = scales::label_comma()(value)) %>%
  pivot_wider(id_cols = c(Scenario,variable), names_from = stat, values_from = value) %>%
  mutate(estimate = paste0(mean, " (", q.2.5, "-", q.97.5,")")) %>%
  select(Scenario, variable, estimate)

r$table_1 <- r$base_results_summary_long %>%
  #pivot_wider(id_cols = Scenario, names_from = variable, values_from = estimate)
  pivot_wider(id_cols = variable, names_from = Scenario, values_from = estimate) %>%
  mutate(variable = factor(variable, levels = c("epi_size", "CH_illness", "deaths_per_100k", "CH_deaths", "CH", "L1plus_days", "L5_days", "CNPI", "C"), ordered = T)) %>%
  arrange(variable) %>%
  left_join(var.labels.df, by = join_by(variable)) %>%
  select(-variable) %>%
  relocate(labels)

saveRDS(object = r, file = "./output/r.rds")

writexl::write_xlsx(r, "./output/table_1.xlsx")


# Simulate a couple of runs to gain intuition of results.

# Not sure I need this for the paper, maybe I need only

# temp_model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file,model_name= "base")
#
# long_results <- list()
# for(i in 1:nrow(base_scenarios)) {
#
#   temp_model$set_input("c", base_scenarios$c[i])$
#     set_input("total_surv_lag", base_scenarios$total_surv_lag[i])$
#     set_input("p", base_scenarios$p[i])
#
#   temp_model$simulate(reps = 100)
#
#   long_results[[base_scenarios$Scenario[i]]] <- temp_model$res_long %>% mutate(Scenario = base_scenarios$Scenario[i])
#
# }
#
# long_results_df <- do.call(rbind, long_results)
#
#
# long_results_df %>%
#   filter(jurisdiction.id == 1) %>%
#   #filter(step < 30) %>%
#   #filter(step <= 10 & L > 1 & Scenario == "NPIs + 5-day EWS") %>% View()
#   ggplot() +
#   geom_line(mapping = aes(x = step, y = NPI, group = interaction(rep, Scenario) , color = as.factor(Scenario))) +
#   facet_wrap(~Scenario)
#
#



#
#
# p$plots$bc_effects_plot = ggplot(bc_effects, aes(x=year,y=irr)) +
#   geom_fan(intervals = (1:100)/100)
#
#
# # Single run examples -----------------------------------------------------
#
# model$set_input("c", 10^5)$set_input("tau", 0.15)
#
# model$simulate(reps = 500)
#
# get_outcome(model$summary, outcome_var = "L5_days", sig_digits = 3)
#
# get_outcome(model$summary, outcome_var = "epi_size", sig_digits = 3)
#
# get_outcome(model$summary, outcome_var = "deaths_per_100k", sig_digits = 3)
#
#
# library(ggplot2)
#
# model$res_long %>%
#   filter(jurisdiction.id == 1) %>%
#   ggplot() +
#   geom_line(mapping = aes(x = step, y = NPI, group = interaction(rep, jurisdiction.id) , color = as.factor(jurisdiction.id)))
#
# model$res_long %>%
#   filter(jurisdiction.id == 1) %>%
#   ggplot() +
#   geom_line(mapping = aes(x = step, y = I, group = interaction(rep, jurisdiction.id), color = as.factor(jurisdiction.id)))
