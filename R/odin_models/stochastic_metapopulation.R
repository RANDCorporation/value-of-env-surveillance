
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
rho   <- user()       # prop asymptomatic
tau     <- user()     # policy marginal effectiveness
c       <- user()     # policy stringency
t_o <- user()         # Time of "opening" (i.e., where NPIs are no longer used)
total_surv_lag <- user()    # case Observation lag (in days)
a_up <- user()           # NPI *a*djustment time
a_down <- user()           # a decrease
L_max <- user()       # max intervention level
C[,] <- user()        # *C*: NPI coordination matrix. 1 if jurisdiction i follows NPI of jurisdiction j if jurisdiction's j NPI is more stringent.
L_c <- user() # 1 if NPIs are coordinated across jurisdictions, 0 otherwise.
p <- user() # case ascertainment proportion.
S0[] <- user()
I0[] <- user()
variant_beta_rr <- user()
variant_t <- user()

# initial conditions ------------------------------------------------------

initial(S[]) <- S0[i]
initial(E[]) <- 0
initial(P[]) <- 0
initial(I[]) <- I0[i]
initial(A[]) <- 0
initial(R[]) <- 0
initial(L[]) <- 0
initial(NPI[]) <- 0
initial(inc_hist[,]) <- 0

# dimensions --------------------------------------------------------------

dim(beta)        <- c(n, n) # contact matrix
dim(lambda_prod) <- c(n, n) # Force of infection matrix
dim(lambda)      <- n       # Force of infection vector
dim(I0)          <- n
dim(S0)          <- n
dim(S)           <- n
dim(E)           <- n
dim(P)           <- n
dim(I)           <- n
dim(A)           <- n
dim(R)           <- n
dim(NPI)           <- n
dim(S_E)         <- n
dim(E_P)         <- n
dim(P_I)         <- n
dim(P_IA)         <- n
dim(P_A)         <- n
dim(A_R)         <- n
dim(I_R)         <- n
dim(p_SE)        <- n
dim(L)           <- n
dim(N)       <- n
dim(C) <- c(n,n)
dim(L_star_matrix) <- c(n,n) # Target NPI matrix.
dim(L_star_f)      <- n # Jurisdiction's final target NPI level
dim(L_star_ind) <- n # Jurisdiction's own L_star without considering other's
dim(L_star_max) <- c(n,n) # Target NPI considering maximum NPI level of coordinating jurisdictions.
dim(lagged_incidence) <- n
#dim(lagged_incidence_mov_avg) <- n
dim(change_NPI_now) <- n
dim(inc_hist) <- c(n,15) # incidence history: up to 15 days of history of jurisdiction i (row) and time t-j days.
#dim(variant_introduced) <- 1

# additional outputs

# Set to TRUE for debugging
#output(lagged_incidence[]) <- TRUE
#output(lagged_incidence_mov_avg[]) <- TRUE
#output(L_star_f[]) <- TRUE


# NPIS & NPI Coordination

# lagged_incidence is the epidemiological signal used to introduce interventions.
# Lagged incidence was causing issues with the use of the delay function.
#lagged_incidence[] <- delay(rbinom(S_E[i], p), total_surv_lag)
update(inc_hist[,]) <- if(j==1) rbinom(S_E[i],p) else inc_hist[i,j-1]

# Original
lagged_incidence[] <- inc_hist[i,as.integer(total_surv_lag+1)]

# Lagged incidence at the 7-day-moving-average
#lagged_incidence_mov_avg[] <- sum(inc_hist[i,as.integer(total_surv_lag+1):as.integer(total_surv_lag+7)]) / 7

# Effective NPI strigency: only active temporarily
eff_c <- if(step <= t_o) c else 100000

# Jurisdiction level target NPI looking only at local prevalence:
# The equation below determines the intervention level, and adjusts the case threshold

# Note that L_star can be > L_max so that the target intervention can remain at the lockdown level.
L_star_ind[] <- min(100000 * (lagged_incidence[i] / N[i]) / (eff_c * p), L_max + 0.01)

# Target NPI considering other jurisdictions:
L_star_matrix[,] <-  C[i,j] * L_star_ind[j]

# Stores the maximum target level at the last column for each row:
L_star_max[,] <- L_star_matrix[i,j]

L_star_max[,2:n] <- if (L_star_max[i,j] > L_star_max[i,j-1]) L_star_max[i,j] else L_star_max[i,j-1]

# Final target NPI:
L_star_f[] <- if(L_c) L_star_max[i,n] else L_star_ind[i]

# Update Non-pharmaceutical intervention level updates with a lag
# using a ddifferent rate for increasing and backing off

# Target (continuous intervention level, with 7-day updating lag):
# Continuous intervention level dependent on direction of L_star:
#update(L[]) <- if(L_star_f[i] > L[i]) L[i] + (L_star_f[i] - L[i]) / a_up else L[i] + (L_star_f[i] - L[i]) / a_down
update(L[]) <- L[i] + (L_star_f[i] - L[i]) / 2

# Decide whether to update the NPI level
change_NPI_now[] <- if(L[i] > NPI[i]) (step %% a_up) == 0 else (step %% a_down) == 0

update(NPI[]) <- if (change_NPI_now[i]) floor(L[i]) else NPI[i]

# Variant strain modified transmissibility

variant_rr <- if(step >= variant_t) variant_beta_rr else 1

# Disease transmission
# Disease transmission equation
lambda_prod[ , ] <- variant_rr * (1-NPI[i]*tau) * beta[i, j] * ((P[j] + I[j] + A[j])/N[j])
lambda[] <- sum(lambda_prod[i, ]) # rowSums

# This is the probability of infection | susceptible
p_SE[] <- 1 - exp(-lambda[i])

N[] <- S[i] + E[i] + P[i] + I[i] + A[i] + R[i]

# difference equations ----------------------------------------------------

## Epidemiological transitions
S_E[] <- rbinom(S[i], p_SE[i])
E_P[] <- rbinom(E[i], 1-exp(-sigma))

# Asymptompatic progression:
# first draw outflows from P:
P_IA[] <- rbinom(P[i], 1-exp(-delta))
# draw symptomatic:
P_I[] <- rbinom(P_IA[i], 1-rho)
# Asymptomatic transitions are the remainder (It is easier to do than a multinomial):
P_A[] <-P_IA[i] - P_I[i]

I_R[] <- rbinom(I[i], 1-exp(-gamma))
A_R[] <- rbinom(A[i], 1-exp(-gamma))

## Difference equations
update(S[]) <- S[i] - S_E[i]
update(E[]) <- E[i] + S_E[i] - E_P[i]
update(P[]) <- P[i] + E_P[i] - P_IA[i]
update(I[]) <- I[i] + P_I[i] - I_R[i]
update(A[]) <- A[i] + P_A[i] - A_R[i]
update(R[]) <- R[i] + I_R[i] + A_R[i]
