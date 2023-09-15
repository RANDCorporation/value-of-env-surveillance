



ymax <- 1
x_transition <- 60
x_mid <- 40


library(dplyr)
library(ggplot2)

sol <- calib_logistic_fn(y_max = 1.5, x_mid_point = x_mid, x_trans = x_transition, x_vector = seq.default(from = 0, to = 60, by = 0.01))

data.frame(x = 0:100) %>%
  mutate(y = logistic_fn(y_max = ymax, x_mid_point = x_mid, x_trans = x_transition, x = x, scale_factor = sol)) %>%
  ggplot(mapping = aes(x = x, y = y)) +
  geom_line()
