


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
t_o <- user()         # Time of "opening" (i.e., where NPIs are no longer used)
surv_lag <- user()    # case Observation lag (in days)
a_up <- user()           # NPI *a*djustment time
a_down <- user()           # a decrease
L_max <- user()       # max intervention level
beta_mult <- user()   # transmissibility multiplier (used for scenario analysis)
A[,] <- user()        # *A*: NPI coordination matrix. 1 if jurisdiction i follows NPI of jurisdiction j if jurisdiction's j NPI is more stringent.
#npi_coord_max <- user() # Whether to use NPI coordination by the maximum NPI. If F, uses the weighted average NPIs following the weights found in the mixing matrix.
L_c <- user() # 1 if NPIs are coordinated across jurisdictions, 0 otherwise.
p <- user() # case ascertainment proportion.


# initial conditions ------------------------------------------------------

# TODO: population must be set from inputs.
initial(S[]) <- 100000
initial(E[]) <- 0.0
initial(P[]) <- 0.0
initial(I[]) <- 10
initial(R[]) <- 0.0
initial(L[]) <- 0
# initial(I_past[,]) <- 0

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
#dim(I_past)      <- c(n,surv_lag)
dim(I_lag)       <- n
dim(A) <- c(n,n)
dim(L_star_matrix) <- c(n,n) # Target NPI matrix.
dim(L_star_f)      <- n # Jurisdiction's final target NPI level
dim(L_star_ind) <- n # Jurisdiction's own L_star without considering other's
dim(L_star_max) <- c(n,n) # Target NPI considering maximum NPI level of coordinating jurisdictions.
dim(Cases_lag) <- n

#dim(L_star_avg) <- n # Target NPI considering the weighted average target NPI level of coordinating jurisdictions.

# additional outputs ------------------------------------------------------

# Set those to TRUE for debugging purposes

output(S_E[]) <- TRUE

#output(N[]) <- TRUE
#output(lambda_prod[]) <- TRUE
#output(lambda[]) <- TRUE
#output(S_E[]) <- TRUE
#output(A[,]) <- TRUE
#output(L_star_ind[]) <- TRUE
#output(L_star_avg[]) <- TRUE
#output(L_star_max[,]) <- TRUE
#output(L_star_matrix[,]) <- TRUE
#output(L_star_f[]) <- TRUE

# NPIS & NPI Coordination

# NPIs use delayed information
# Here, we may instead use a matrix and assign

# update(I_past[,]) <- if(I_past[,]) I[i]
# option using the delay function
I_lag[] <- delay(I[i], surv_lag)

# Assuming a 50% case ascertainment proportion.
# Cases_lag is the epidemiological signal used to introduce interventions.
Cases_lag[] <- delay(rbinom(S_E[i], p), surv_lag)

output(I_lag) <- TRUE

# Effective NPI strigency: only active temporarily
eff_c <- if(step <= t_o) c else 100000

# Jurisdiction level target NPI looking only at local prevalence:
# The equation below determines the intervention level, and adjusts the case threshold

# Note that L_star can be > L_max so that the target intervention can remain at the lockdown level.
L_star_ind[] <- min(100000 * (Cases_lag[i] / N[i]) / (eff_c * p), L_max + 0.999)

# Target NPI considering other jurisdictions:
L_star_matrix[,] <-  A[i,j] * L_star_ind[j]

# NPI target using weighted averages of other's NPI targets.
# Unclear if i needs to be the in the column or the rows.
#L_star_avg[] <- sum(L_star_matrix[,i]) / n

# Stores the maximum target level at the last column for each row:
L_star_max[,] <- L_star_matrix[i,j]

L_star_max[,2:n] <- if (L_star_max[i,j] > L_star_max[i,j-1]) L_star_max[i,j] else L_star_max[i,j-1]

# Final target NPI:
L_star_f[] <- if(L_c) L_star_max[i,n] else L_star_ind[i]

# Update Non-pharmaceutical intervention level updates with a lag:
# Equal adjustment rate for increasing or backing off interventions:

# Equal
# update(L[]) <- L[i] + (L_star_f[i] - L[i]) / a

# Differential rate for increasing and backing off: maybe needed for elimination:

update(L[]) <- if(L_star_f[i] > L[i]) L[i] + (L_star_f[i] - L[i]) / a_up else L[i] + (L_star_f[i] - L[i]) / a_down

# Disease transmission

# Disease transmission equation
lambda_prod[ , ] <- beta_mult * (1-floor(L[i])*tau) * beta[i, j] * (I[j] + P[j])
lambda[] <- sum(lambda_prod[i, ]) # rowSums

# This is the probability of infection | susceptible
p_SE[] <- 1 - exp(-lambda[i]/N[i])

N[] <- S[i] + E[i] + P[i] + I[i] + R[i]

# difference equations ----------------------------------------------------

## Epidemiological transitions
S_E[] <- rbinom(S[i], p_SE[i])
E_P[] <- rbinom(E[i], 1-exp(-sigma))
P_I[] <- rbinom(P[i], 1-exp(-delta))
I_R[] <- rbinom(I[i], 1-exp(-gamma))

## Difference equations
update(S[]) <- S[i] - S_E[i]
update(E[]) <- E[i] + S_E[i] - E_P[i]
update(P[]) <- P[i] + E_P[i] - P_I[i]
update(I[]) <- I[i] + P_I[i] - I_R[i]
update(R[]) <- R[i] + I_R[i]
