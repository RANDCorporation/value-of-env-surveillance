


#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# This script demonstrates how to run an experiment using the model

# Set up an experimental design and run the model:
source("./R/library.R")

# Instantiate model -------------------------------------------------------

model <- OdinMetapop$new("stochastic_metapopulation.R", s$data_file)

# Run full-factorial experiment -------------------------------------------

# Create sample parameter set
# This is here to demonstrate that data.frame with parameter sets (i.e., a
# sample from the posterior distribution of model parameters)
# can be assigned to the model. This is required for R6Sim models that
# are used within the R6Experiment class, but not necessary otherwise.
model$set_param_dist(params_list = list(param_dist_a = data.frame(sample_param = 1, weights = 1)), use_average = T, param_dist_weights = "weights")

# Instantiate an experiment

# Note that if the experiment is to be run in parallel, there is an underlying
# assumption that the model will be instantiated in the cluster eval
# script
experiment <- R6Experiment$new(model)

# Set experimental parameters
experiment$
  set_parameter("surv_lag", "grid", seq.default(from = 0, to = 21, by = 3))$ # Surveillance lag
  set_parameter("p", "grid", c(0.5,1))$ # case ascertainment proportion
  set_parameter("tau", "grid", c(0.12,0.15,0.18))$ # intevention effectiveness per intervention level
  set_parameter("c", "grid", c(1,5,7,10,50,100))$ # case intervention threshold per 100,000 people
  set_parameter("R0", "grid", c(1.5,2,2.5))$ # Initial reproduction number
  set_parameter("L_c", "grid", c(0,1)) # Turn intervention coordination on and off


# Set designs creates the experimental design data.frame
experiment$set_design()

# Run experiment (parallel option not available with odin at this time):
exp_results <- experiment$run(parallel = T,cluster_eval_script = "./R/scripts/sample_analysis_cluster_eval.R", n_cores = parallel::detectCores() - 3, model_from_cluster_eval = T)


# Save output (but do not push this file):

date_time <- paste0(gsub(pattern = ":| |-", replacement = "_", Sys.time()),"_", gsub("/", "_",Sys.timezone()))

write.csv(exp_results, paste0("./output/", "exp_results_",date_time,".csv"), row.names = F)


# Summary figures and tables ----------------------------------------------

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
  labs(title = "Shorter surveillance lags increase NPI costs")


fig_1

fig_2 <- exp_results %>%
  ggplot(mapping = aes(x = surv_lag, y = C / 10^9, color = as.factor(c))) +
  geom_line() +
  facet_wrap(~R0+L_c, labeller = label_both, scales = "free") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Shorter surveillance lags can reduce pandemic costs")

fig_2

fig_3 <- exp_results %>%
  ggplot(mapping = aes(x = surv_lag, y = CH / 10^9, color = as.factor(c))) +
  geom_line() +
  facet_wrap(~R0+L_c, labeller = label_both, scales = "free") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Shorter surveillance lags reduce health costs")

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
