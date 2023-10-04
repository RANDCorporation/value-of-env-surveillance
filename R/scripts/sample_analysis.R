


# Set up an experimental design and run the model:
source("./R/library.R")

# Instantiate model -------------------------------------------------------

model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

# Run full-factorial experiment -------------------------------------------

# Create sample parameter set
model$set_param_dist(params_list = list(param_dist_a = data.frame(sample_param = 1, weights = 1)), use_average = T, param_dist_weights = "weights")

# Instantiate an experiment
experiment <- R6Experiment$new(model)

# Set experimental parameters
experiment$
  set_parameter(parameter_name = "surv_lag", experimental_design = "grid", values = seq.default(from = 0, to = 20, by = 2))$
  set_parameter("c", "grid", c(2.5,5,10))$
  set_parameter("R0", "grid", c(2,4))$
  set_parameter("L_c", "grid", c(0,1))


# Set designs creates the experimental design data.frame
experiment$set_design()

# Run experiment (parallel option not available with odin at this time):
exp_results <- experiment$run(parallel = F)


# Save output (but do not push this file):

date_time <- paste0(gsub(pattern = ":| |-", replacement = "_", Sys.time()),"_", gsub("/", "_",Sys.timezone()))


# Summary figures and tables ----------------------------------------------

write.csv(exp_results, paste0("./output/", "exp_results_",date_time,".csv"), row.names = F)


# Create sample figures

# Sample plot:
#. This is not very thought out yet, just demonstrates how you can create plots with the results


# Right now, these numbers don't make much sense to me:

# start figure names with fig

fig_1 <- exp_results %>%
  ggplot(mapping = aes(x = surv_lag, y = CNPI, color = as.factor(c))) +
  geom_line() +
  facet_wrap(~R0+L_c, labeller = label_both, scales = "free") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Lower surveillance lags increase NPI costs")


fig_1

fig_2 <- exp_results %>%
  ggplot(mapping = aes(x = surv_lag, y = C / 10^9, color = as.factor(c))) +
  geom_line() +
  facet_wrap(~R0+L_c, labeller = label_both, scales = "free") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Lower surveillance lags can reduce pandemic costs")

fig_2

fig_3 <- exp_results %>%
  ggplot(mapping = aes(x = surv_lag, y = CH / 10^9, color = as.factor(c))) +
  geom_line() +
  facet_wrap(~R0+L_c, labeller = label_both, scales = "free") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Lower surveillance lags reduce health costs")

fig_3


# Save plots to ppt

doc <- read_pptx(path = "./output/template.pptx")

names_figs <- ls()[grepl("^fig*", ls())]

for (i in names_figs) {
  doc <- doc %>%
    add_slide(master = "Retrospect") %>%
    ph_with(
      value = rvg::dml(print(eval(sym(i)))),
      location = ph_location_fullsize()
    )
}


# Add summary tables


# We can also add tables to the slides, like so:
# doc <- doc %>%
#   add_slide(master = "Retrospect") %>%
#   ph_with(
#     value = table %>% filter(case_type == "edge case") %>% select(-perc.cost.reduction, -case_type),
#     location = ph_location_type("body")
#   ) %>%
#   add_slide(master = "Retrospect") %>%
#   ph_with(
#     value = table %>% filter(case_type == "cost-effective-blood") %>% select(-case_type),
#     location = ph_location_type("body")
#   )

# Save figs powerpoint file for editable figures
print(doc, target = paste0("./output/", "figs_",date_time,".pptx"))
