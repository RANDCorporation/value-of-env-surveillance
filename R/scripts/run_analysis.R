
# Source all dependencies and model scripts
source("./R/library.R")

# results list

if(!file.exists("./output/r.rds")) {
  r <- list()
} else {
  r <- readRDS("./output/r.rds")
}


# 1. Set up experiments and constants ----------------------------------------

model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

model$set_param_dist(params_list = list(param_dist_a = data.frame(sample_param = 1, weights = 1)), use_average = T, param_dist_weights = "weights")

base_scenarios <- readxl::read_xlsx("./data/scenarios.xlsx", sheet = "one-way-scenarios")

outcomes <- c("deaths_per_100k", "CH_illness", "CH_deaths", "CH", "L5_days", "L1plus_days", "CNPI", "C", "epi_size")

# counterfactual scenarios without enhanced EWS
count_scenarios <- base_scenarios %>%
  filter(Section == "Scenarios") %>%
  mutate(total_surv_lag = 13, p = 0.3, NMB_comparator = T)

all_scenarios <- rbind(base_scenarios, count_scenarios)

# create experiment
base_experiment <- R6Experiment$new(model)

base_experiment$set_design(grid_design_df = all_scenarios)


# tau vs R0 experimental design

tau <- c(seq.default(from = 0, to = 1, by = 0.05)/5, 1-1/3)
R0 <- 2.5 * c(0.5, 1, 1.5)

EWS <- c(T,F)
tau_design <- expand_grid(tau, EWS, R0) %>%
  mutate(p = ifelse(EWS, 1, 0.3)) %>%
  mutate(total_surv_lag = ifelse(EWS, 8,13))

tau_experiment <- R6Experiment$new(model)

tau_experiment$set_design(grid_design_df = tau_design)



# 2. Run experiments ------------------------------------------------------

## 2.1 Run main experiment -----------------------------------------------------

base_results <- base_experiment$run(parallel = T,
                                    cluster_eval_script = "./R/scripts/cluster_eval.R",
                                    parallel::detectCores() - 2,
                                    model_from_cluster_eval = T)


# Compute differences at the replication level:
comp_results <- base_results %>%
  filter(NMB_comparator) %>%
  select(rep, counterfactual.id, any_of(outcomes)) %>%
  clear.labels() %>%
  rename_at(vars(any_of(outcomes)), function(x) paste0(x, "_no_ews"))


base_res_diff <- base_results %>%
  clear.labels() %>%
  left_join(comp_results, by = join_by(rep, counterfactual.id))

base_res_diff <- base_res_diff %>%
  mutate(across( any_of(outcomes), ~ pull(base_res_diff, paste0(cur_column(), "_no_ews") ) - ., .names = "{.col}_diff") ) %>%
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
  separate(col = statistic,into = c("variable", "stat"), sep = "_\\.")


## 2.2 Run tau experiment ------------------------------------------------------

tau_results <- tau_experiment$run(parallel = T,
                                  cluster_eval_script = "./R/scripts/cluster_eval.R",
                                  parallel::detectCores() - 2,
                                  model_from_cluster_eval = T)


# Comparator scenarios without EWS
tau_comp <-  tau_results %>%
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



# 4. Tables ---------------------------------------------------------------

# table_param_summaries
r$table_1_long_all <- r$base_results_long %>%
  #mutate(value = signif(value, digits = 3)) %>%
  mutate(value = gt::vec_fmt_number(value, n_sigfig = 3, use_seps = T)) %>%
  #mutate(value = formatC(value, format="f", big.mark=",")) %>%
  #mutate(value = scales::label_comma()(value)) %>%
  pivot_wider(id_cols = c(Scenario,Section, Class, NMB_comparator, counterfactual.id, variable), names_from = stat, values_from = value) %>%
  mutate(estimate = paste0(mean, " (", lower, "-", upper,")"))

r$table_1_long_numeric <- r$base_results_long %>%
  pivot_wider(id_cols = c(Scenario,Section, Class, NMB_comparator, counterfactual.id, variable), names_from = stat, values_from = value)


r$table_1_long <- r$table_1_long_all %>%
  filter(Section == "Base case") %>%
  select(Scenario, variable, estimate)


## Table 1 -----------------------------------------------------------------

r$table_1 <- r$table_1_long %>%
  pivot_wider(id_cols = variable, names_from = Scenario, values_from = estimate) %>%
  mutate(variable = factor(variable, levels = c("epi_size", "CH_illness", "deaths_per_100k","deaths_per_100k_diff", "CH_deaths", "CH", "L1plus_days", "L5_days", "CNPI", "C", "NMB"), ordered = T)) %>%
  arrange(variable) %>%
  left_join(var.labels.df, by = join_by(variable)) %>%
  select(-variable) %>%
  relocate(labels) %>%
  rename(Outcome = labels) %>%
  filter(!is.na(Outcome)) %>%
  relocate(Outcome, `No NPIs`, `NPIs w/o EWS`, `NPIs + 2-day EWS`, `NPIs + 5-day EWS`, `NPIs + 10-day EWS`)

writexl::write_xlsx(r$table_1, path = "./output/table_1.xlsx")

## Supplementary Table 2 ---------------------------------------------------

r$sup_table_2 <- r$table_1_long_all %>%
  filter(Section == "Scenarios") %>%
  mutate(EWS = ifelse(NMB_comparator, "N", "Y")) %>%
  mutate(variable = factor(variable, levels = c("epi_size", "CH_illness", "deaths_per_100k","deaths_per_100k_diff", "CH_deaths", "CH", "L1plus_days", "L5_days", "CNPI", "C", "NMB"), ordered = T)) %>%
  filter(variable %in% c("deaths_per_100k", "CH", "CNPI", "C", "NMB")) %>%
  left_join(var.labels.df, by = join_by(variable)) %>%
  select(Scenario, EWS, labels, estimate) %>%
  pivot_wider(id_cols = c(Scenario, EWS), names_from = labels, values_from = estimate) %>%
  mutate(Scenario = factor(Scenario, levels = unique(base_scenarios$Scenario), ordered = T)) %>%
  arrange(Scenario, EWS)

writexl::write_xlsx(r$table_1, path = "./output/table_1.xlsx")

# 4. Figures -----------------------------------------------------------------


# 5. Additional figures ---------------------------------------------------

# dot plot figure data

r$fig_1_data <- r$table_1_long_numeric %>%
  mutate(EWS = ifelse(!NMB_comparator, "NPIs w/ EWS", "NPIs w/o EWS")) %>%
  filter(variable %in% c("CH", "CNPI", "C", "NMB", "deaths_per_100k")) %>%
  left_join(var.labels.df, by = join_by(variable)) %>%
  filter(Section == "Scenarios") %>%
  mutate(variable = factor(variable,levels = c("CH", "CNPI", "C", "NMB", "deaths_per_100k"), ordered = T)) %>%
  mutate(labels = factor(labels,levels = c("Health costs", "NPI costs", "Total costs", "Net monetary benefit", "Deaths per 100,000 people"), ordered = T)) %>%
  mutate(Scenario = factor(Scenario, levels = rev(unique(base_scenarios$Scenario)), ordered = T)) %>%
  arrange(variable, Scenario) %>%
  filter(!(variable == "NMB" & NMB_comparator))


colors = c("#8856a7", "#9ebcda")

# Deaths & NMB figure -----------------------------------------------------

r$deaths_figure <- r$fig_1_data %>%
  filter(variable %in% c("deaths_per_100k", "NMB")) %>%
  filter(Scenario %in% c("Base-case", "0.5x transmissible", "1.5x transmissible")) %>%
  ggplot() +
  geom_segment(aes(x = Scenario, xend = Scenario, y = lower, yend = upper, color = EWS), linewidth = 1.5) +
  geom_point(aes(x = Scenario, y = mean, color = EWS), size = 3) +
  #geom_hline(yintercept = 1500, color = "grey") +
  coord_flip() +
  #facet_grid(rows = vars(Class), cols = vars(labels), scales = "free", space = "free") +
  facet_wrap(~labels, scales = "free", nrow = 2, as.table = F) +
  labs(x = "Scenario",
       y = "",
       color = "EWS system") +
  scale_color_manual(values = colors) +
  theme(panel.grid.major.y = element_line(color = "gray",
                                          linewidth = 0.2,
                                          linetype = 1)) +
  theme(legend.text=element_text(size=12)) +
  theme(panel.spacing = unit(1, "lines"))

ggsave(plot = r$deaths_figure, filename = "./output/deaths_plot.svg",units = "in", width = 5, height = 7, scale = 1.2, bg = "white")


# r$fig_A <- r$fig_1_data %>%
#   filter(variable != "deaths_per_100k") %>%
#   ggplot() +
#   geom_segment(aes(x = Scenario, xend = Scenario, y = lower, yend = upper, color = EWS)) +
#   geom_point(aes(x = Scenario, y = mean, color = EWS), size = 3) +
#   #geom_hline(yintercept = 1500, color = "grey") +
#   coord_flip() +
#   #facet_grid(rows = vars(Class), cols = vars(labels), scales = "free", space = "free") +
#   facet_wrap(~labels, scales = "free", nrow = 2, as.table = F) +
#   labs(x = "Scenario",
#        y = "",
#        color = "EWS system") +
#   scale_color_manual(values = colors) +
#   scale_y_continuous(labels = scales::dollar_format(scale = 1e-3,prefix="$", suffix = "K"))


r$fig_A_epi <- r$fig_1_data %>%
  filter(Scenario %in% c("Base-case", "0.5x transmissible", "1.5x transmissible", "0.5x deadly", "1.5x deadly")) %>%
  ggplot() +
  geom_segment(aes(x = Scenario, xend = Scenario, y = lower, yend = upper, color = EWS)) +
  geom_point(aes(x = Scenario, y = mean, color = EWS), size = 3) +
  #geom_hline(yintercept = 1500, color = "grey") +
  coord_flip() +
  #facet_grid(rows = vars(Class), cols = vars(labels), scales = "free", space = "free") +
  facet_wrap(~labels, scales = "free_x", nrow = 2, as.table = F) +
  labs(x = "Scenario",
       y = "",
       color = "EWS system") +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3,prefix="$", suffix = "K"))


r$fig_A_other <- r$fig_1_data %>%
  filter(Scenario %in% c("Base-case", "1.5x NPI costs", "0.5x NPI costs", "3-day decision lag", "2-week decision lag", "1.5x stringent", "0.5x stringent", "90% max NPI eff", "50% max NPI eff")) %>%
  #filter(labels %in% c("Health costs", "NPI costs", "Net monetary benefit")) %>%
  ggplot() +
  geom_segment(aes(x = Scenario, xend = Scenario, y = lower, yend = upper, color = EWS)) +
  geom_point(aes(x = Scenario, y = mean, color = EWS), size = 3) +
  #geom_hline(yintercept = 1500, color = "grey") +
  coord_flip() +
  #facet_grid(rows = vars(Class), cols = vars(labels), scales = "free", space = "free") +
  facet_wrap(~labels, scales = "free_x", nrow = 2, as.table = F) +
  labs(x = "Scenario",
       y = "",
       color = "EWS system") +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3,prefix="$", suffix = "K"))






r$fig_effectiveness_data <- tau_results %>%
  mutate(effectiveness = tau * 5) %>%
  select(effectiveness, EWS, any_of(outcomes)) %>%
  group_by(effectiveness, EWS) %>%
  summarise_all(summary_functions) %>%
  mutate(EWS = ifelse(EWS, "NPIs w/ EWS", "NPIs w/o EWS"))


r$fig_effectiveness_total_cost <- r$fig_effectiveness_data %>%
  ggplot() +
  geom_line(mapping = aes(x = effectiveness, y = C_.mean, color = EWS)) +
  geom_ribbon(mapping = aes(x = effectiveness, ymin = C_.lower, ymax = C_.upper, fill = EWS), alpha = 0.5) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(labels = scales::percent_format()) +
  ylab("Total cost") +
  xlab("NPI effectiveness")

r$fig_effectiveness_total_cost

nmb_colors <- c("#FAAE7B", "#9F6976", "#432371")

r$fig_effectiveness_NMB <- r$tau_NMB_summary %>%
  mutate(R0 = as.factor(R0)) %>%
  ggplot() +
  geom_line(mapping = aes(x = effectiveness, y = NMB_.mean, color = R0, group = R0)) +
  geom_ribbon(mapping = aes(x = effectiveness, ymin = NMB_.lower, ymax = NMB_.upper, fill = R0, group = R0), alpha = 0.2) +
  scale_fill_manual(values = nmb_colors) +
  scale_color_manual(values = nmb_colors) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(labels = scales::percent_format()) +
  ylab("EWS net monetary benefit") +
  xlab("Maximum NPI effectiveness") +
  xlim(c(0,1)) +
  geom_hline(yintercept = 0, color = "gray", alpha = 0.5)


r$fig_effectiveness <- r$fig_effectiveness_data %>%
  ggplot() +
  geom_line(mapping = aes(x = effectiveness, y = CH_.mean, color = EWS)) +
  geom_ribbon(mapping = aes(x = effectiveness, ymin = CH_.lower, ymax = CH_.upper, fill = EWS), alpha = 0.5) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(labels = scales::percent_format()) +
  ylab("Health cost") +
  xlab("NPI effectiveness") +
  xlim(c(0,1))


r$fig_effectiveness <- r$fig_effectiveness_data %>%
  ggplot() +
  geom_line(mapping = aes(x = effectiveness, y = CNPI_.mean, color = EWS)) +
  geom_ribbon(mapping = aes(x = effectiveness, ymin = CH_.lower, ymax = CNPI_.upper, fill = EWS), alpha = 0.5) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(labels = scales::percent_format()) +
  ylab("NPI cost") +
  xlab("NPI effectiveness") +
  xlim(c(0,1))

# save results object and table -------------------------------------------

saveRDS(object = r, file = "./output/r.rds")

