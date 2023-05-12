

# Metapopulation model implemented using the odin package.
# See https://mrc-ide.github.io/odin/
# for documentation on using odin
# And
# for examples of models implemented using odin
# This model is inspired in http://epirecip.es/epicookbook/chapters/lloydjansen2004/r_odin
# Which provides an implementation of a metapopulation model
# with coupling and/or migration between patches
# https://www.sciencedirect.com/science/article/abs/pii/S0025556403001809?via%3Dihub

library(odin)

# Helper functions:

SEIR_cont <- odin::odin({
  nr_patches <- user(2)
  n <- nr_patches

  ## Params
  # Assignments in arrays are translated by odin to for loops in C
  # See https://mrc-ide.github.io/odin/articles/functions.html


  # Disease transmission equation
  lambda_prod[ , ] <- beta[i, j] * (I[j] + P[j])
  lambda[] <- sum(lambda_prod[i, ]) # rowSums

  # This is the probability of infection | susceptible
  p_SE[] <- 1 - exp(-lambda[i]/N[i])



  # Mobility equations
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
  S_E[] <- rbinom(S[i], p_SE[i]) # S[i] * lambda[i]
  E_P[] <- rbinom(E[i], 1-exp(-sigma))
  P_I[] <- rbinom(P[i], 1-exp(-delta))
  I_R[] <- rbinom(I[i], 1-exp(-gamma))

  ## Derivatives                         # turning off mobility for the moment. This will also have to be stochastic.
  update(S[]) <- S[i] - S_E[i]           #+ mp[1] * mob_S[i]
  update(E[]) <- E[i] + S_E[i] - E_P[i]  #+ mp[2] * mob_E[i]
  update(P[]) <- P[i] + E_P[i] - P_I[i]  #+ mp[3] * mob_P[i]
  update(I[]) <- I[i] + P_I[i] - I_R[i]  #+ mp[4] * mob_I[i]
  update(R[]) <- R[i] + I_R[i]           #+ mp[5] * mob_R[i]

  ## Initial conditions
  initial(S[]) <- 10000000000
  initial(E[]) <- 0.0
  initial(P[]) <- 0.0
  initial(I[]) <- 1
  initial(R[]) <- 0.0

  ## parameters
  beta[,] <- user()   # effective contact rate (S to e)
  sigma   <- 1/6      # progression rate from Exposed to Pre-symptomatic
  delta   <- 1/6      # rate of progression from pre-symptomatic to Infected
  gamma   <- 1/3      # rate of recovery from active disease
  C[,]    <- user()   # origin-destination matrix of proportion of population that travels
  mp[]     <- user()  # relative migration propensity by disease status

  ## dimensions
  dim(beta)        <- c(n, n) # contact matrix
  dim(C)           <- c(n, n) # Mobility matrix
  dim(mp)           <- 5       # Relative Mobility vector
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
  dim(p_SE)        <- n
})


# Running the model
# set seed
set.seed(1)

# SEIR free parameters --------------------------------------------------------#
## total number of patches in the model
nr_patches = 2
## relative migration propensity by disease status (S, P, E, I, R)
mp_S = 1
mp_P = 1
mp_E = 0.5
mp_I = 1
mp_R = 1

mp <- c(1, 1, 0.5, 1, 1)
## matrix of effective contact rates
# using the function above
#beta <- beta.mat(nr_patches)
# Or assigning the beta matrix directly
beta <- diag(2, nrow = nr_patches, ncol = nr_patches)

write.csv(beta, file = "mixing.csv")


## mobility matrix using the random function

#C <- mob.mat(nr_patches)

## A symetric mobility matrix:
# 1 % of the population travels every day in every jurisdiction
# It represents people leaving from each row, entering each
daily_travel <- 0.01

# Divide that by the number of destinations
travel_per_destination <- daily_travel / (nr_patches-1)

# Create the mobility matrix
mob <- matrix(data = daily_travel, nrow = nr_patches, ncol = nr_patches)

# The diagonal of the mobility matrix must be the number of people leaving
# So we first set it to zero:
diag(mob) <- 0

# Assign it to the Column sums
# row sums would also work because everything is symetric in this example.
# But the sum
diag(mob) <- -rowSums(mob)

# Is the mobility matrix balanced?
sum(mob) == 0

write.csv(mob, file = "mobility.csv")

# run SEIR model --------------------------------------------------------------#
mod <- SEIR_cont$new(nr_patches=nr_patches, beta=beta, C=mob, mp=mp)
t <- seq(0, 60, 1)

out <-mod$run(t)

View(out)
out <- mod$transform_variables(out)


#This model might not work very well in discrete time

