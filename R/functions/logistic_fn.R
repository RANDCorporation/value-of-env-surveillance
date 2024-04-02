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
# Scenario-based Logistic Function Estimation
#
# Use these functions to find the scale parameter of a logistic curve that
# asymptotes at a set y_max value, passes through x_mid_point at (y_max - 0)/2
# and that has a  pre-specified transition duration in the "x" axis.
# The purpose of this function is to create interpretable transition curves
# to be used as an input in a simulation model.
#------------------------------------------------------------------------------#


# See
# https://www.tjmahr.com/anatomy-of-a-logistic-growth-curve
# note that scale is related to the slope of the function at the midpoint,
# but I do not know a closed-form solution that relates the scale parameter
# to a desired "transition time" from zero to y_max.
# Hence, I use the optim function to find the scale parameter that produces
# the desired logistic growth function.


#' Logistic growth function
#'
#' @description
#' Produces a logistic growth function
#'
#'
#' @param y_max asymptote of the function. it always starts in zero.
#' @param x_mid_point x point at which y value reaches the midpoint between 0 and y_max
#' @param x_trans x units that it takes to go from 0 to the asymptote
#' @param x x value at which function needs to be computed.
#' @param scale_factor scaling_factor so that the x_transition time can be achieved
#'
#' @return a vector or a number for y
logistic_fn <- function(y_max, x_mid_point, x_trans, x, scale_factor = 1 / 6) {
  # note scale_factor*x_trans *is* the scale parameter in the logistic growth function.
  y_max / (1 + exp((x_mid_point - x) * (scale_factor * x_trans)))
}

# x is the scale_factor
#' Objective function for the calibration of the logistic growth function
#' @seealso [logistic_fn()]
#'
#' @param x scale_factor to be used
#' @param y_max asymptote
#' @param x_mid_point midpoint
#' @param x_trans transition duration in terms of x values
#' @param x_vector a vector of x's over which the scale will be evaluated
log_calib_obj_fn <- function(x, y_max, x_mid_point, x_trans, x_vector) {
  # compute ys for this scale factor
  ys <- logistic_fn(y_max, x_mid_point, x_trans, x = x_vector, scale_factor = x)

  # compute the range of x values where a transition is happening
  trans_range <- range(x_vector[c(F, diff(ys) > 10e-6)])

  # target is to get the transition range right
  # Hence, we minimize objective function:
  (x_trans - (trans_range[2] - trans_range[1]))^2
}


#' Calibrate logistic growth function
#'
#' @param y_max asymptote
#' @param x_mid_point midpoint
#' @param x_trans transition duration in terms of x values
#' @param x_vector a vector of x's over which the scale will be evaluated
#'
calib_logistic_fn <- function(y_max, x_mid_point, x_trans, x_vector = seq.default(from = 0, to = x_mid_point * 3, by = 0.01)) {
  # The optimization routine does not work very well without a good
  # starting point and ranges for the search. Hence, I provide a
  # good starting point for the function, and verified that it worked.

  # let:
  # by construction of the logistic function:
  # scale = x_trans * scale_factor

  # by approximation:
  # (imagine there is a triangle starting from the point where y starts
  # to increase to the point where it reaches y_max. Hence, the slope of this
  # triangle is.
  # slope = y_max / x_trans
  # The slope of this triangle is also close to the slope of the logistic
  # function at that x_mid_point.

  # by the property of the logistic function:
  # see: https://www.tjmahr.com/anatomy-of-a-logistic-growth-curve/#fn:other-terms
  # slope = y_max * scale / 4

  # Then, solving for scale, we have that a good guess for the scale factor is:

  scale_guess <- 4 / (x_trans^2)

  # And we impose a range around it for the search.
  # This approach has proven to produce stable results.

  solution <- optim(
    par = scale_guess, method = "Brent", # "L-BFGS-B",
    fn = log_calib_obj_fn,
    lower = scale_guess * 0.1,
    upper = scale_guess * 3,
    y_max = y_max,
    x_mid_point = x_mid_point,
    x_trans = x_trans,
    x_vector = x_vector
  )

  # print(solution)

  solution$par
}
