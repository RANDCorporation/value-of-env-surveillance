

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
  S_E[] <- S[i] * lambda[i]
  E_P[] <- sigma * E[i]
  P_I[] <- delta * P[i]
  I_R[] <- gamma * I[i]

  ## Derivatives
  deriv(S[]) <- - S_E[i]         + mp[1] * mob_S[i]
  deriv(E[]) <- S_E[i] - E_P[i]  + mp[2] * mob_E[i]
  deriv(P[]) <- E_P[i] - P_I[i]  + mp[3] * mob_P[i]
  deriv(I[]) <- P_I[i] - I_R[i]  + mp[4] * mob_I[i]
  deriv(R[]) <- I_R[i]           + mp[5] * mob_R[i]

  ## Initial conditions
  initial(S[]) <- 1.0 - 1E-6
  initial(E[]) <- 0.0
  initial(P[]) <- 0.0
  initial(I[]) <- 1E-6
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
})


# Running the model
# set seed
set.seed(1)

# SEIR free parameters --------------------------------------------------------#
## total number of patches in the model
nr_patches = 6
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
beta <- diag(1, nrow = nr_patches, ncol = nr_patches)

write.csv(beta, file = "mixing.csv")


## mobility matrix using the random function

C <- mob.mat(nr_patches)

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
t <- seq(0, 100, 1)
out <- mod$run(t)

View(out)
out <- mod$transform_variables(out)


#This model might not work very well in discrete time

# error check -----------------------------------------------------------------#
if ( ! all( abs(rowSums(out$N) - nr_patches) < 1E-10 ) )
  warning("Something went wrong, density is increasing/decreasing!\n")




# plotting --------------------------------------------------------------------#
plot.pretty <- function(out, nr_patches, what) {

  # plot total densities by disease status --------------------------------------#
  if (what == "total") {
    ## compute total densities by disease status
    S_tot <- rowSums(out$S) / nr_patches
    E_tot <- rowSums(out$E) / nr_patches
    P_tot <- rowSums(out$P) / nr_patches
    I_tot <- rowSums(out$I) / nr_patches
    R_tot <- rowSums(out$R) / nr_patches

    ## plot total densities by disease status
    par(mfrow=c(1, 1), las=1, omi=c(1,0,0,0), xpd=NA)
    plot( t, S_tot, col="green",
          type="l", xlab="Days", ylab="Densities",
          main="Total densities by disease status")
    lines(t, E_tot, col="yellow")
    lines(t, P_tot, col="orange")
    lines(t, I_tot, col="red")
    lines(t, R_tot, col="blue")
    legend(-8.5, -0.3, title="Disease statuses", horiz=TRUE,
           legend=c("Susceptible", "Exposed", "Pre-Symptomatic", "Infectious", "Recovered"),
           col=c("green", "yellow", "orange", "red", "blue"), lty=1)
  }

  # plot densities of some patches by disease status ----------------------------#
  if (what == "panels") {
    ## define the plot panels
    if (nr_patches >= 6) {
      mfrow=c(2, 3)
      panels <- as.integer( seq(1, nr_patches, length.out=6))
    } else if (nr_patches >= 4) {
      mfrow=c(2, 2)
      panels <- as.integer( seq(1, nr_patches, length.out=4))
    } else {
      mfrow=c(1, nr_patches)
      panels <- 1:nr_patches
    }
    par(mfrow=mfrow, las=1, omi=c(1,0,0.3,0), xpd=NA)

    ## plot the disease statuses of some patches
    ymax <- max(out$N[, panels])
    for (i in panels) {
      plot (t, out$S[, i], col="green", ylim=c(0, ymax),
            type="l", xlab="Days", ylab="Densities",
            main=paste("Patch ", i))
      lines(t, out$E[, i], col="orange")
      lines(t, out$I[, i], col="red")
      lines(t, out$R[, i], col="blue")
    }
    title("Densities by disease status", outer=TRUE)
    legend(-130, -1.2, title="Disease statuses", horiz=TRUE,
           legend=c("Susceptible", "Exposed", "Infectious", "Recovered"),
           col=c("green", "orange", "red", "blue"), lty=1)
  }

}



# plot total densities by disease status --------------------------------------#
plot.pretty(out, nr_patches, "total")

# plot densities of some patches by disease status ----------------------------#
#plot.pretty(out, nr_patches, "panels")

