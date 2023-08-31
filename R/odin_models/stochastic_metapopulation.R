


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
npi_coord[,] <- user()
npi_coord_max <- user(1) # Whether to use NPI coordination by the maximum NPI. If F, uses the weighted average NPIs following the weights found in the mixing matrix.

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
dim(I_lag)       <- n
dim(npi_coord) <- c(n,n)
dim(L_star_matrix) <- c(n,n) # Target NPI matrix.
dim(L_star_f)      <- n # Jurisdiction's final target NPI level
dim(L_star_ind) <- n # Jurisdiction's own L_star without considering other's
dim(L_star_max) <- c(n,n) # Target NPI considering maximum NPI level of coordinating jurisdictions.
dim(L_star_avg) <- n # Target NPI considering the weighted average target NPI level of coordinating jurisdictions.

# additional outputs ------------------------------------------------------

#output(N[]) <- TRUE
#output(lambda_prod[]) <- TRUE
#output(lambda[]) <- TRUE
#output(S_E[]) <- TRUE

# nonpharmaceutical interventions -----------------------------------------

# NPIs use delayed information
I_lag[] <- delay(I[i], obs_lag)
output(I_lag) <- TRUE

# Effective NPI strigency: only active temporarily
eff_c <- if(step <= npi_duration) c else 0

# need to use min(L_star, l_max), but we need to verify it's a parallel minimum.
# L_star is target NPI level?
print("npi_coord: {npi_coord}")

# Use npi_coord variable here to compute target npi

# Jurisdiction level target NPI looking only at local prevalence:
L_star_ind[] <- min(1000 * eff_c * I_lag[i] / N[i], L_max)

# Target NPI considering other jurisdictions:
L_star_matrix[,] <-  npi_coord[i,j] * L_star_ind[j]

# NPI target using weighted averages of other's NPI targets.
# Unclear if i needs to be the in the column or the rows.
L_star_avg[] <- sum(L_star_matrix[,i]) / n

# Stores the maximum target level at the last column for each row:
L_star_max[,] <- L_star_matrix[i,j]

L_star_max[,2:n] <- if (L_star_max[i,j] > L_star_max[i,j-1]) L_star_max[i,j] else L_star_max[i,j-1]

# Final target NPI:
L_star_f[] <- if(npi_coord_max) L_star_max[i,n] else L_star_avg[i]

output(npi_coord[,]) <- TRUE

output(L_star_ind[]) <- TRUE

output(L_star_avg[]) <- TRUE

output(L_star_max[,]) <- TRUE

output(L_star_matrix[,]) <- TRUE

output(L_star_f[]) <- TRUE

update(L[]) <- L[i] + (L_star_f[i] - L[i]) / days_to_adjust_NPI

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
