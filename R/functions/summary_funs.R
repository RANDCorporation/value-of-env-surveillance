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

# Summary functions list:
# One can add more percentiles to the calculation by changing the vector below.
alpha <- 0.95
p <- c((1 - alpha) / 2, 1 - (1 - alpha) / 2)
p_names <- c(".lower", ".upper")

summary_functions <- map(p, ~ partial(quantile, probs = .x, na.rm = TRUE)) %>%
  set_names(nm = p_names)

# p_names <- map_chr(p, ~paste0(".q.",.x*100))
summary_functions <- map(p, ~ partial(quantile, probs = .x, na.rm = TRUE)) %>%
  set_names(nm = p_names)
summary_functions$.mean <- mean

# Get estimate in text form
pull_estimate <- function(df, ...) {
  df %>%
    dplyr::filter(...) %>%
    .$estimate
}

# get outcome from summary table
get_outcome <- function(df, outcome_var, sig_digits = 3, ...) {
  r <- df %>%
    dplyr::filter(...) %>%
    as.data.frame() %>%
    mutate(across(where(is.numeric), .fns = ~ signif(x = .x, digits = sig_digits)))

  vars <- paste0(outcome_var, "_.", c("mean", "lower", "upper"))

  paste0(r[, vars[1]], " (", r[, vars[2]], "-", r[, vars[3]], ")")
}
