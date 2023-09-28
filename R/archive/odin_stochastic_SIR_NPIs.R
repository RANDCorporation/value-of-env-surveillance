


#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Stochastic SIR model with adaptive NPIs
#
# This file specifies a stochastic odin model including NPIs.
# This is *not* an R script. The R extension is used for syntax highlighting.
# For information on syntax, see https://mrc-ide.github.io/odin/index.html
#------------------------------------------------------------------------------#


# parameters --------------------------------------------------------------

S_ini <- user(1000)
I_ini <- user(1)
beta <- user(1/4)
gamma <- user(1/10)

# immunity waning rate
theta <- user(1/180)

# inbound net migration (i.e., re-seeding)
# people per day entering with an infection
d_reseeding <- user(0)

a <- user(7) # adjust NPIs every n days

# absolute relative reduction in the beta parameter for every intervention level
# 0.2 here means 20% reduction in transmission for every step in
# the NPI intervention scale.
beta_effect <- user(0.2)

# increase in the intervention level for every
stringency <- user(3)

# maximum intervention level
max_intervention_level <- user(5)

# surveillance delay
surv_delay <- user(5)

# initial_conditions ------------------------------------------------------

## initial states
initial(S) <- S_ini
initial(I) <- I_ini
initial(R) <- 0
initial(NPI) <- 0
initial(Time) <- 0
initial(TimeLastNPIChange) <- 0


# equations ---------------------------------------------------------------

## Total population size
N <- S + I + R

# odin doesn't seem to provide a time step variable,
# so I'm creating a stock variable
update(Time) <- Time + 1


# nonpharmaceutical interventions -----------------------------------------

# lagged epidemiological outcome to use for controlling NPIs.
Ilag <- delay(I, surv_delay)

# target intervention level depends on prevalence
# this might be modified to better represent alternative surveillance methods
# for instance, tests take longer to obtain test information
# and there is also case ascertainment bias from tests
# we might want to incorporate that information here.

# NPI level is a stock variable from 0 to max_intervention_level (5)
# 0 means business as usual, and 5 should mean something close to a lockdown.
# Actual intervention updates with a delay

target_NPI <- min((Ilag/N) * stringency * 100, max_intervention_level)
output(target_NPI) <- TRUE

# We can updated NPis every a days
can_update_NPI <- if(Time >= TimeLastNPIChange + a) 1 else 0

new_NPI <- if(can_update_NPI) target_NPI else NPI

# This implies continuous, smooth adjustment:
update(NPI) <- NPI + (target_NPI - NPI) / a

# this implies discrete-time periodic adjustment:
#update(NPI) <- new_NPI

# NPI effects:
# Use this to debug the model
# print("target_NPI: {target_NPI}")
# beta parameter is influenced by policy dynamically
beta_policy <- max(beta * (1-(round(NPI, 0) * beta_effect)), 0)

# transition probabilities ------------------------------------------------

## Individual probabilities of transition:
p_SI <- 1 - exp(-beta_policy * I / N) # S to I
p_IR <- 1 - exp(-gamma) # I to R
p_RS <- 1 - exp(-theta) # R to S (waning of immunity)

# draw from binomial distributions for numbers changing between compartments:
n_SI <- rbinom(S, p_SI)
n_IR <- rbinom(I, p_IR)
n_RS <- rbinom(R, p_RS)

# The new case importation follows a poisson distribution
n_re_seeding <- rpois(d_reseeding)


# epi difference equations ------------------------------------------------
update(S) <- S - n_SI + n_RS
update(I) <- I + n_SI - n_IR + n_re_seeding
update(R) <- R + n_IR - n_RS
update(TimeLastNPIChange) <- if(new_NPI == NPI) TimeLastNPIChange else Time
