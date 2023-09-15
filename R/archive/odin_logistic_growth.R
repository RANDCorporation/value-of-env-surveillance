


#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Sample logistic growth model
#
# Sample logistic growth model from the odin package.
# This is *not* an R script. The R extension is used for syntax highlighting.
# For information on syntax, see https://mrc-ide.github.io/odin/index.html
#------------------------------------------------------------------------------#


# Parameters --------------------------------------------------------------
N0 <- user(1)
K <- user(100)
r <- user(1)

# Initial Conditions ------------------------------------------------------

initial(N) <- N0

# Flows and Derivatives ---------------------------------------------------

deriv(N) <- r * N * (1 - N / K)


