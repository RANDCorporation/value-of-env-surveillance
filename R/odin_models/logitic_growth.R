
# -------------------------------------------------------------------------
# This is an Odin script
# Consult the odin documentation to understand the valid syntax.
# https://mrc-ide.github.io/odin/
# Do not assume this is a script
# -------------------------------------------------------------------------

# Parameters --------------------------------------------------------------
N0 <- user(1)
K <- user(100)
r <- user()


# Initial Conditions ------------------------------------------------------

initial(N) <- N0

# Flows and Derivatives ---------------------------------------------------

deriv(N) <- r * N * (1 - N / K)


