


#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# odinpbm metapopulation class
#
# An R6 class to represents odin models
#------------------------------------------------------------------------------#

odinmetapop <- R6::R6Class(
  classname = "odinmetapop",
  inherit = odinpbm,
  public = list(
    collect_default_inputs = function() {
      # start from the inputs collected by the parent class
      inputs <- super$collect_default_inputs()
      # add in inputs that are unique to this model.
      # TODO: Jing, set additional inputs here.
      inputs$beta <- structure(c(1, 0, 0, 1), dim = c(2L, 2L))
      inputs$C <- structure(c(-0.01, 0.01, 0.01, -0.01), dim = c(2L, 2L))
      inputs$nr_patches <- 2
      inputs$mp <- c(1, 1, 0.5, 1, 1)
      return(inputs)
    }
  )
)
