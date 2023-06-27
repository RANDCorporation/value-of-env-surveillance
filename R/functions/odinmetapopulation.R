


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
      print(inputs)
      # add in inputs that are unique to this model.
      # TODO: Jing, set additional inputs here.
      nr_patches <- as.integer(inputs$nr_patches)
      inputs$beta <- structure(diag(nr_patches), dim = c(nr_patches,  nr_patches))
      #inputs$C <- structure(c(-0.01, 0.01, 0.01, -0.01), dim = c(2L, 2L))
      #inputs$mp <- c(1, 1, 0.5, 1, 1)
      return(inputs)
    },

    #' Computes replication-level summaries based on model results
    #'
    #' @export
    post_process = function() {

      # hard-coded for first jurisdiction
      # Sarah to generalize
      # Compute time-varying costs:
      self$res <- self$res %>%
        # This is a linear function from now, it can be non-linear
        mutate(CNPI = `L[1]` * self$oi$tau * self$inputs$jurisdiction$cost.npi[1])

      # This is where we summarize costs:
      self$summary <- self$res %>%
        group_by(rep) %>%
        summarise(CNPI = sum(CNPI),
                  R = max(`R[1]`)) %>%
        # Compute other costs:
        mutate(CH = R * (self$oi$r*self$oi$w + self$oi$o),
               CSURV = self$oi$C_surv,
               C = CH + CSURV + CNPI)

    }


  )
)
