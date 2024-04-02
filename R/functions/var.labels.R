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

# Variale labels for consistent naming and labeling in tables and within the model.
# Names can be re-used across data.frames

var.labels <- c(
  rep = "replication id",
  step = "simulation day",
  jurisdiction.id = "jurisdiction id",
  L = "continuous NPI level (0 = no intervention)",
  NPI = "NPI level in effect",
  S = "susceptibles",
  E = "exposed (not infectious)",
  P = "pre-symptomatic (infectious)",
  I = "infected",
  A = "asymptomatic",
  R = "removed",
  jurisdiction.name = "jurisdiction name",
  population = "population",
  cost.npi = "daily cost of npis per day per person, per intervention level",
  prevalence = "prevalence of symptomatic disease",
  IFR = "infection fatality rate",
  IFR_time_mult = "time-varying ifr risk ratio",
  new_removed = "individuals removed from the simulation",
  deaths_per_100k = "Deaths per 100,000 people",
  deaths_per_100k_diff = "Deaths averted per 100,000 people",
  total_cost_of_illness = "total cost of ilness per person, exc. deaths",
  CNPI = "NPI costs",
  CH_illness = "Cost of illness",
  CH_deaths = "Cost of deaths",
  CH = "Health costs",
  CH_diff = "value of life saved",
  CSURV = "Surveillance cost",
  C = "Total costs",
  epi_size = "Epidemic size",
  L5_days = "Days of max NPI",
  L1plus_days = "Days of any NPI",
  NMB = "Net monetary benefit"
)

var.labels.df <- data.frame(variable = names(var.labels), labels = var.labels)
