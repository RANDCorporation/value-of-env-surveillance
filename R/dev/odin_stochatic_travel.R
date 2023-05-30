
library(ggplot2)
library(dplyr)

# The purpose of this script is to demonstrate and test the implementation
# of stochastic travel with subsequent epidemiological transitions.
# The goal is to provide an implementation in odin that
# can reliably work with multiple jurisdictions and matches the behavior
# of travel implemented outside of the model.

# at any day

# mobility matrix:
loc_names <- c("US", "EU", "Other")

# population order of magnitude
mag <- 3*10^3
# p: population vector
P <- c(mag, 2*mag, 3*mag)
death_rate <- 0.001
t_final <- 500


# travel matrix
# contains travel flows from i to j as a proportion of the population
# rows add to 1.
# Diagonal (proportion who don't travel is set to zero).
# m_ij: proportion of population that travels from row i to column j
# everyone has a daily chance of traveling ~ 1%
M <- matrix(data = 0.001,
            byrow = T, nrow = length(P), ncol = length(P), dimnames = list(loc_names, loc_names))

diag(M) <- 0
diag(M) <- 1- rowSums(M)

# This is the mobility matrix to be used in these tests:
M

# Draw multinomial distributions for each state:

P_post_trav_m <- matrix(data = 0, nrow = length(loc_names), ncol = length(loc_names), dimnames = list(loc_names, loc_names))

run_test_model = function(rep) {
  res <- list()
  # for each time-step:
  for(t in 1:t_final) {
    res[[t]] <- data.frame(rep = rep, t = t, US =P[1], EU = P[2], Other = P[3])
    # Draw multinomial for travel:
    # P_post_trav is the matrix with population from location i that will be in place j
    for (i in 1:nrow(M)) {
      P_post_trav_m[i,] <-  t(rmultinom(n = 1, size = P[i], prob = M[i,]))
    }
    # This is the population vector after travel but before other transitions
    P_post_trav <- colSums(P_post_trav_m)
    # after the multinomial, some people die:
    death_p <- 1-exp(-death_rate)
    # Death transition:
    death_flow <- rbinom(n = 3, size = P_post_trav, prob = death_p)
    # this is the change in population, only from travel:
    p_change <- colSums(P_post_trav_m) - P
    # or equivalently,
    P <- P + p_change - death_flow
  }
  res <- do.call(rbind, res)
}


# demonstrate stochastic travel with epidemiological transitions.

# Trying to implement the same behavior with odin:
travel_odin_model <- odin::odin(x = {
  n <- user(3)
  mag <- user(3*10^3)
  M[,] <- user()
  initial(P[]) <- i * mag
  dim(P) <- n
  dim(M) <- c(n, n)
  dim(P_post_trav) <- n
  dim(P_post_trav_m) <- c(n, n)
  death_rate <- user(0.01)
  dim(deaths) <- n
  dim(P_change) <- n

  #travel loop emulating a multinomial distribution from sequential binomial draws
  # see https://github.com/mrc-ide/odin/issues/213
  P_post_trav_m[,1] <- rbinom(P[i],M[i,1])
  # computing the post-travel matrix      # This inner sum is a *row* sum   # the second bit accounts for the fact that that probability is conditional on not having traveled to the other places.
  P_post_trav_m[,2:n] <- rbinom(P[i] - sum(P_post_trav_m[i,1:(j-1)]), M[i,j]/sum(M[i,j:n]))

  # We might want to use this P_post_trav_m object to count the total
  # number of people who are going *to* or from a location
  # they might need to be taken into account in the transmission equation
  # even if the net travel rate is 0.

  output(P_post_trav_m) <- TRUE
  output(P_change) <- TRUE

  # do a col sum: Sum *for each* row:
  P_post_trav[] <- sum(P_post_trav_m[,i])
  P_change[] <- P_post_trav[i] - P[i]

  # Compute deaths
  deaths[] <- rbinom(P_post_trav[i], 1-exp(-death_rate))

  # difference equation:
  update(P[]) <- P[i] + P_change[i] - deaths[i]

})

model <- travel_odin_model$new(mag = mag, M = M, death_rate = death_rate)

res <- model$run(1:t_final) %>% as.data.frame()

# only conserves mass if deaths is set to 0.
# rowSums(res[,2:ncol(res)])


# Compare both implementations:

# R Implementation:
R_reps_results <- purrr::map_dfr(1, .f = run_test_model)

R_reps_results %>%
  ggplot(aes(x = t, group = rep)) +
  geom_line(mapping = aes(y = US), color = "blue") +
  geom_line(mapping = aes(y = EU), color = "red") +
  geom_line(mapping = aes(y = Other), color = "green") +
  geom_hline(yintercept = 0)


# odin implementation

res %>%
  ggplot(aes(x = step)) +
  geom_line(mapping = aes(y = `P[1]`), color = "blue") +
  geom_line(mapping = aes(y = `P[2]`), color = "red") +
  geom_line(mapping = aes(y = `P[3]`), color = "green") +
  geom_hline(yintercept = 0)



