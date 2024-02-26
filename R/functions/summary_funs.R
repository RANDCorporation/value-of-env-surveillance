

# Create summary functions list:
# One can add more percentiles to the calculation by changing the vector below.
alpha <- 0.99
p <- c((1-alpha)/2, 1-(1-alpha)/2)
p_names <- c(".lower", ".upper")

summary_functions <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
  set_names(nm = p_names)

#p_names <- map_chr(p, ~paste0(".q.",.x*100))
summary_functions = map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
  set_names(nm = p_names)
summary_functions$.mean = mean

# Get estimate in Text Form:

pull_estimate <- function(df, ...) {
  df %>%
    dplyr::filter(...) %>%
  .$estimate
}

get_outcome <- function(df, outcome_var, sig_digits = 3, ...) {
  r <- df %>%
    dplyr::filter(...) %>%
    as.data.frame() %>%
    mutate(across(where(is.numeric), .fns = ~signif(x = .x, digits = sig_digits)))

  vars = paste0(outcome_var, "_.", c("mean", "lower", "upper"))

  paste0(r[,vars[1]], " (", r[,vars[2]], "-", r[,vars[3]],")")
}




# get_outcome(df, "outcome", a == 2, b == 2)


