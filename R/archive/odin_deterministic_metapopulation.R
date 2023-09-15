


#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Deterministic metapopulation model
#
# Sample deterministic metapopulation model.
# Inspired in http://epirecip.es/epicookbook/chapters/lloydjansen2004/r_odin
# This is *not* an R script. The R extension is used for syntax highlighting.
# For information on syntax, see https://mrc-ide.github.io/odin/index.html
#------------------------------------------------------------------------------#


# parameters --------------------------------------------------------------

nr_patches <- user(2)
n <- nr_patches
beta[,] <- user()   # effective contact rate (S to e)
sigma   <- 1/6      # progression rate from Exposed to Pre-symptomatic
delta   <- 1/6      # rate of progression from pre-symptomatic to Infected
gamma   <- 1/3      # rate of recovery from active disease
C[,]    <- user()   # origin-destination matrix of prop of pop that travels
mp[]     <- user()  # relative migration propensity by disease status

# initial conditions ------------------------------------------------------
initial(S[]) <- 1.0 - 1E-6
initial(E[]) <- 0.0
initial(P[]) <- 0.0
initial(I[]) <- 1E-6
initial(R[]) <- 0.0


# dimensions --------------------------------------------------------------

dim(beta)        <- c(n, n) # contact matrix
dim(C)           <- c(n, n) # Mobility matrix
dim(mp)           <- 5      # Relative Mobility vector
dim(lambda_prod) <- c(n, n) # Force of infection matrix
dim(lambda)      <- n       # Force of infection vector
dim(mob_prod)    <- c(n, n)
dim(mob_S)       <- n
dim(mob_E)       <- n
dim(mob_P)       <- n
dim(mob_I)       <- n
dim(mob_R)       <- n
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


# equations ---------------------------------------------------------------

# assignments in arrays are translated by odin to for loops in C
# See https://mrc-ide.github.io/odin/articles/functions.html

# disease transmission equation
lambda_prod[ , ] <- beta[i, j] * (I[j] + P[j])
lambda[] <- sum(lambda_prod[i, ]) # rowSums

# mobility equations
# mob_prod object is re-used for each disease status.
# Calculate the number of people leaving each state
mob_prod[ , ] <- S[i] * C[i, j]

# and compute net flow of mobility to S
mob_S[] <- sum(mob_prod[, i])     # colSums

mob_prod[ , ] <- E[i] * C[i, j]
mob_E[] <- sum(mob_prod[, i])

mob_prod[ , ] <- P[i] * C[i, j]
mob_P[] <- sum(mob_prod[, i])

mob_prod[ , ] <- I[i] * C[i, j]
mob_I[] <- sum(mob_prod[, i])

mob_prod[ , ] <- R[i] * C[i, j]
mob_R[] <- sum(mob_prod[, i])

N[] <- S[i] + E[i] + P[i] + I[i] + R[i]

# output desired variables for debugging
output(N[]) <- TRUE
output(lambda_prod[]) <- TRUE
output(lambda[]) <- TRUE
output(S_E[]) <- TRUE
output(mob_prod[]) <- TRUE

## Epidemiological Flows
S_E[] <- S[i] * lambda[i]
E_P[] <- sigma * E[i]
P_I[] <- delta * P[i]
I_R[] <- gamma * I[i]

# derivatives -------------------------------------------------------------

deriv(S[]) <- - S_E[i]         + mp[1] * mob_S[i]
deriv(E[]) <- S_E[i] - E_P[i]  + mp[2] * mob_E[i]
deriv(P[]) <- E_P[i] - P_I[i]  + mp[3] * mob_P[i]
deriv(I[]) <- P_I[i] - I_R[i]  + mp[4] * mob_I[i]
deriv(R[]) <- I_R[i]           + mp[5] * mob_R[i]

