


#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Stochastic SIR metapopulation model with adaptive NPIs
#
# This file specifies a stochastic metapopulation odin model including NPIs.
# This is *not* an R script. The R extension is used for syntax highlighting.
# For information on syntax, see https://mrc-ide.github.io/odin/index.html
#------------------------------------------------------------------------------#

# parameters --------------------------------------------------------------

nr_patches <- user()
n <- nr_patches # the number of regions is equal to the length of most inputs

## Params
# Assignments in arrays are translated by odin to for loops in C
# See https://mrc-ide.github.io/odin/articles/functions.html

## parameters
beta[,] <- user()     # effective contact rate (S to e)
sigma   <- user()     # progression rate from Exposed to Pre-symptomatic
delta   <- user()     # rate of progression from pre-symptomatic to Infected
gamma   <- user()     # rate of progression from active disease to Removed
tau     <- user()     # policy marginal effectiveness
c       <- user()     # policy stringency
npi_duration <- user(1000) # Maximum days to use interventions
obs_lag <- user()          # case Observation lag (in days)
days_to_adjust_NPI <- user() # time to adjust NPIs
output(tau) <- TRUE
L_max <- 5 # max intervention level
trans_mult <- user(1) # transmissibility multiplier (used for scenario analysis)

# initial conditions ------------------------------------------------------

initial(S[]) <- 10000000
initial(E[]) <- 0.0
initial(P[]) <- 0.0
initial(I[]) <- 10
initial(R[]) <- 0.0
initial(L[]) <- 0

# dimensions --------------------------------------------------------------

dim(beta)        <- c(n, n) # contact matrix
dim(lambda_prod) <- c(n, n) # Force of infection matrix
dim(lambda)      <- n       # Force of infection vector
dim(S)           <- n
dim(E)           <- n
dim(P)           <- n
dim(I)           <- n
dim(R)           <- n
dim(N)           <- n
dim(S_E)         <- n
dim(E_P)         <- n
dim(P_I)         <- n
dim(I_R)         <- n
dim(p_SE)        <- n
dim(L)           <- n
dim(L_star)      <- n
dim(I_lag)       <- n

# additional outputs ------------------------------------------------------

output(N[]) <- TRUE
output(lambda_prod[]) <- TRUE
output(lambda[]) <- TRUE
output(S_E[]) <- TRUE

# nonpharmaceutical interventions -----------------------------------------

# NPIs use delayed information
I_lag[] <- delay(I[i], obs_lag)
output(I_lag) <- TRUE

# Effective NPI strigency: only active temporarily
eff_c <- if(step <= npi_duration) c else 0

# need to use min(L_star, l_max), but we need to verify it's a parallel minimum.
# L_star is target NPI level?
L_star[] <- min(1000 * eff_c * I_lag[i] / N[i], L_max) # implies same stringency for everyone

output(L_star) <- TRUE

update(L[]) <- L[i] + (L_star[i] - L[i]) / days_to_adjust_NPI

# Disease transmission equation
lambda_prod[ , ] <- trans_mult * (1-L[i]*tau) * beta[i, j] * (I[j] + P[j])
lambda[] <- sum(lambda_prod[i, ]) # rowSums

# This is the probability of infection | susceptible
p_SE[] <- 1 - exp(-lambda[i]/N[i])

N[] <- S[i] + E[i] + P[i] + I[i] + R[i]

# difference equations ----------------------------------------------------

## Epidemiological Flows
S_E[] <- rbinom(S[i], p_SE[i]) # S[i] * lambda[i]
E_P[] <- rbinom(E[i], 1-exp(-sigma))
P_I[] <- rbinom(P[i], 1-exp(-delta))
I_R[] <- rbinom(I[i], 1-exp(-gamma))

## Derivatives
update(S[]) <- S[i] - S_E[i]
update(E[]) <- E[i] + S_E[i] - E_P[i]
update(P[]) <- P[i] + E_P[i] - P_I[i]
update(I[]) <- I[i] + P_I[i] - I_R[i]
update(R[]) <- R[i] + I_R[i]
