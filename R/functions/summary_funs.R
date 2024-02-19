

# Create summary functions list:
# One can add more percentiles to the calculation by changing the vector below.
p <- c(0.025, 0.975)
p_names <- map_chr(p, ~paste0(".q.",.x*100))
summary_functions = map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
  set_names(nm = p_names)
summary_functions$.mean = mean

# Get estimate in Text Form:

get_estimate = function(summary_tbl, strategy, col.sens, model_name, outcome, sig_numbers = 3) {

  r = summary_tbl %>%
    filter(Strategy == strategy, model == model_name, sensitivity == col.sens) %>%
    mutate(across(where(is.numeric), .fns = ~signif(x = .x, digits = sig_numbers)))

  vars = paste0(outcome, "_.", c("mean", "q.2.5", "q.97.5"))

  paste0(r[,vars[1]], " (95% PI [", r[,vars[2]], ", ", r[,vars[3]],"])")

}

get_outcome <- function(df, outcome_var, sig_digits = 3, ...) {
  r <- df %>%
    dplyr::filter(...) %>%
    as.data.frame() %>%
    mutate(across(where(is.numeric), .fns = ~signif(x = .x, digits = sig_digits)))

  vars = paste0(outcome_var, "_.", c("mean", "q.2.5", "q.97.5"))

  paste0(r[,vars[1]], " (", r[,vars[2]], "-", r[,vars[3]],")")

}

# get_outcome(df, "outcome", a == 2, b == 2)


