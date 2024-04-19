#------------------------------------------------------------------------------#
# Code for "The value of environmental sampling surveillance"
# Copyright (C) 2024 by The RAND Corporation
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# See LICENSE.md and README.md for more information on usage and licensing
#
# Author: Pedro Nascimento de Lima
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# This script produces the analyses, tables and figures.
#
#------------------------------------------------------------------------------#

# Source all dependencies and model scripts

# 2. Run experiments ------------------------------------------------------

# The following two steps will take about 30 minutes to run on a macbook pro.

# source("./R/scripts/run_main_experiment.R")

# source("./R/scripts/run_tau_experiment.R")

source("./R/library.R")

msg_time("Post-processing analysis")

# results list contains the results we want to save
if (!file.exists("./output/r.rds")) {
  r <- list()
} else {
  r <- readRDS("./output/r.rds")
}


msg_time("Creating and saving tables and figures")

# 3. Tables ---------------------------------------------------------------

# long table with summary statistics
r$table_1_long_all <- r$base_results_long %>%
  mutate(value = gt::vec_fmt_number(value, n_sigfig = 3, use_seps = T)) %>%
  pivot_wider(id_cols = c(Scenario, Section, Class, NMB_comparator, counterfactual.id, variable), names_from = stat, values_from = value) %>%
  mutate(estimate = paste0(mean, " (", lower, "-", upper, ")"))

# Numeric version for plots
r$table_1_long_numeric <- r$base_results_long %>%
  pivot_wider(id_cols = c(Scenario, Section, Class, NMB_comparator, counterfactual.id, variable), names_from = stat, values_from = value)

# Table 1 only includes base case scenarios
r$table_1_long <- r$table_1_long_all %>%
  filter(Section == "Base case") %>%
  select(Scenario, variable, estimate)


## Table 1 -----------------------------------------------------------------

r$table_1 <- r$table_1_long %>%
  pivot_wider(id_cols = variable, names_from = Scenario, values_from = estimate) %>%
  mutate(variable = factor(variable, levels = c("epi_size", "CH_illness", "deaths_per_100k", "deaths_per_100k_diff", "CH_deaths", "CH", "L1plus_days", "L5_days", "CNPI", "C", "NMB"), ordered = T)) %>%
  arrange(variable) %>%
  left_join(var.labels.df, by = join_by(variable)) %>%
  select(-variable) %>%
  relocate(labels) %>%
  rename(Outcome = labels) %>%
  filter(!is.na(Outcome)) %>%
  relocate(Outcome, `No NPIs`, `NPIs w/o ESS`, `NPIs + 2-day ESS`, `NPIs + 5-day ESS`, `NPIs + 10-day ESS`)

writexl::write_xlsx(r$table_1, path = "./output/table_1.xlsx")

## Supplementary Table 2 ---------------------------------------------------

r$sup_table_2 <- r$table_1_long_all %>%
  filter(Section == "Scenarios") %>%
  mutate(ESS = ifelse(NMB_comparator, "N", "Y")) %>%
  mutate(variable = factor(variable, levels = c("epi_size", "CH_illness", "deaths_per_100k", "deaths_per_100k_diff", "CH_deaths", "CH", "L1plus_days", "L5_days", "CNPI", "C", "NMB"), ordered = T)) %>%
  filter(variable %in% c("deaths_per_100k", "CH", "CNPI", "C", "NMB")) %>%
  left_join(var.labels.df, by = join_by(variable)) %>%
  select(Scenario, ESS, labels, estimate) %>%
  pivot_wider(id_cols = c(Scenario, ESS), names_from = labels, values_from = estimate) %>%
  mutate(Scenario = factor(Scenario, levels = unique(r$base_scenarios$Scenario), ordered = T)) %>%
  arrange(Scenario, ESS)

writexl::write_xlsx(r$sup_table_2, path = "./output/sup_table_2.xlsx")

# 4. Figures -----------------------------------------------------------------

r$fig_1_data <- r$table_1_long_numeric %>%
  mutate(ESS = ifelse(!NMB_comparator, "NPIs w/ ESS", "NPIs w/o ESS")) %>%
  filter(variable %in% c("CH", "CNPI", "C", "NMB", "deaths_per_100k")) %>%
  left_join(var.labels.df, by = join_by(variable)) %>%
  mutate(variable = factor(variable, levels = c("CH", "CNPI", "C", "NMB", "deaths_per_100k"), ordered = T)) %>%
  mutate(labels = factor(labels, levels = rev(c("NPI costs", "Health costs", "Net monetary benefit", "Total costs", "Deaths per 100,000 people")), ordered = T)) %>%
  mutate(Scenario = factor(Scenario, levels = rev(unique(r$base_scenarios$Scenario)), ordered = T)) %>%
  arrange(variable, Scenario)


colors <- c("#8856a7", "#9ebcda")


## Fig 1 -------------------------------------------------------------------

r$fig_1 <- r$fig_1_data %>%
  filter(Section == "Base case") %>%
  filter(Scenario != "No NPIs") %>%
  filter(variable != "deaths_per_100k") %>%
  ggplot() +
  geom_segment(aes(x = Scenario, xend = Scenario, y = lower, yend = upper, color = ESS)) +
  geom_point(aes(x = Scenario, y = mean, color = ESS), size = 3) +
  coord_flip() +
  lemon::facet_rep_wrap(~ factor(labels, levels = c("Health costs", "NPI costs", "Total costs", "Net monetary benefit")), scales = "free_x") +
  labs(
    x = "Scenario",
    y = "Thousands of dollars per person",
    color = "ESS system"
  ) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, prefix = "", accuracy = 0.5)) +
  theme(plot.margin = margin(
    t = 0,
    r = 0,
    b = 0,
    l = 0
  ))

r$fig_1

ggsave(plot = r$fig_1, filename = "./output/fig_1.svg", units = "in", width = 6.5, height = 5, scale = 1.4, bg = "white", dpi = 300)

## Fig 2 ----------------------------------------------------------------

nmb_colors <- c("#FAAE7B", "#9F6976", "#432371")

r$fig_2 <- r$tau_NMB_summary %>%
  mutate(R0 = as.factor(R0)) %>%
  ggplot() +
  geom_line(mapping = aes(x = effectiveness, y = NMB_.mean, color = R0, group = R0)) +
  geom_ribbon(mapping = aes(x = effectiveness, ymin = NMB_.lower, ymax = NMB_.upper, fill = R0, group = R0), alpha = 0.2) +
  scale_fill_manual(values = nmb_colors) +
  scale_color_manual(values = nmb_colors) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(labels = scales::percent_format()) +
  ylab("ESS net monetary benefit") +
  xlab("Maximum NPI effectiveness") +
  xlim(c(0, 1)) +
  geom_hline(yintercept = 0, color = "gray", alpha = 0.5) +
  # Showing calibrated tau as a dashed line
  geom_vline(xintercept = 0.1422919 * 5, color = nmb_colors[2], linetype = "dashed")

r$fig_2

ggsave(plot = r$fig_2, filename = "./output/fig_2.svg", units = "in", width = 6.5, height = 5, scale = 1.4, bg = "white", dpi = 300)


## Sup Figure 1 ------------------------------------------------------------

r$sup_fig_1 <- r$fig_1_data %>%
  filter(!(variable == "NMB" & NMB_comparator)) %>%
  filter(Section == "Scenarios") %>%
  filter(variable != "deaths_per_100k") %>%
  ggplot() +
  geom_segment(aes(x = Scenario, xend = Scenario, y = lower, yend = upper, color = ESS)) +
  geom_point(aes(x = Scenario, y = mean, color = ESS), size = 3) +
  coord_flip() +
  lemon::facet_rep_wrap(~ factor(labels, levels = c("Health costs", "NPI costs", "Total costs", "Net monetary benefit")), scales = "free_x") +
  labs(
    x = "Scenario",
    y = "Thousands of dollars per person",
    color = "ESS system"
  ) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, prefix = "", accuracy = 1)) +
  theme(plot.margin = margin(
    t = 0,
    r = 12,
    b = 0,
    l = 0
  ))

r$sup_fig_1

ggsave(plot = r$sup_fig_1, filename = "./output/sup_fig_1.svg", units = "in", width = 6.5, height = 6.5, scale = 1.4, bg = "white", dpi = 300)


## Sup figure 2


nmb_to_period <- function(nmb, cost) {
  cost / nmb
}

costs <- data.frame(cost = c(5,10,50,100))

r$sup_fig_2_data <- r$fig_1_data %>%
  filter(variable == "NMB") %>%
  filter(!NMB_comparator) %>%
  filter(Section == "Scenarios") %>%
  expand_grid(., costs) %>%
  mutate(lower_years = lower / cost) %>%
  mutate(upper_years = upper / cost) %>%
  mutate(mean_years = mean / cost)

r$sup_fig_2 <- r$sup_fig_2_data  %>%
  ggplot() +
  geom_segment(aes(x = Scenario, xend = Scenario, y = lower_years, yend = upper_years), color = colors[1]) +
  geom_point(aes(x = Scenario, y = mean_years, color = ESS), size = 3, color = colors[1]) +
  coord_flip() +
  lemon::facet_rep_wrap(~ paste0("$ ", factor(cost), " per year") , scales = "free_x") +
  labs(
    x = "Scenario",
    y = "Pandemic frequency for payoff (years)"
  ) +
  #scale_color_manual(values = colors) +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  theme(plot.margin = margin(
    t = 0,
    r = 12,
    b = 0,
    l = 0
  ))

r$sup_fig_2

ggsave(plot = r$sup_fig_2, filename = "./output/sup_fig_2.svg", units = "in", width = 6.5, height = 6.5, scale = 1.4, bg = "white", dpi = 300)


# 5. Additional figures ---------------------------------------------------

## Deaths & NMB figure -----------------------------------------------------

r$deaths_figure <- r$fig_1_data %>%
  filter(variable %in% c("deaths_per_100k", "NMB")) %>%
  filter(Scenario %in% c("Base-case", "0.5x transmissible", "1.5x transmissible")) %>%
  ggplot() +
  geom_segment(aes(x = Scenario, xend = Scenario, y = lower, yend = upper, color = ESS), linewidth = 1.5) +
  geom_point(aes(x = Scenario, y = mean, color = ESS), size = 3) +
  coord_flip() +
  facet_wrap(~labels, scales = "free", nrow = 2, as.table = F) +
  labs(
    x = "Scenario",
    y = "",
    color = "ESS system"
  ) +
  scale_color_manual(values = colors) +
  theme(legend.text = element_text(size = 12)) +
  theme(panel.spacing = unit(1, "lines"))

r$deaths_figure

ggsave(plot = r$deaths_figure, filename = "./output/deaths_plot.svg", units = "in", width = 5, height = 7, scale = 1.2, bg = "white")

msg_time("Save plots and figures")
## Save results -----------------------------------------------------------
saveRDS(object = r, file = "./output/r.rds")


msg_time("Finished!")
