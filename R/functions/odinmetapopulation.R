


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
      inputs$nr_patches <- as.integer((self$inputs$settings %>% filter(setting=="nr_patches"))$value)
      #print(self$inputs$settings["nr_patches",2])
      inputs$beta <- structure(diag(inputs$nr_patches), dim = c(inputs$nr_patches,  inputs$nr_patches))
      #inputs$C <- structure(c(-0.01, 0.01, 0.01, -0.01), dim = c(2L, 2L))
      #inputs$mp <- c(1, 1, 0.5, 1, 1)
      return(inputs)
    },

    #' Computes replication-level summaries based on model results
    #'
    #' @export
    post_process = function() {

      # select only the variables we want for the summary variable

      required_jurisdiction_variables = c("rep", "step", "L", "R")


      # This is an example only for L:



      # res_long <- self$res %>%
      #   select(starts_with(required_jurisdiction_variables, ignore.case = FALSE)) %>%
      #   select(-starts_with("L_star")) %>% # not a clean thing to do
      #   pivot_longer(cols = starts_with("L"),values_to = "L", names_to = "jurisdiction", names_pattern = "L.*([0-9]+).*")

      # convert data.frame to long format adding a jurisdiction column

      self$res_long <- self$res %>%
        select(starts_with(required_jurisdiction_variables, ignore.case = FALSE)) %>%
        select(-starts_with("L_star")) %>%
        tidyr::pivot_longer(cols = -c(rep, step), names_to = "variable", values_to = "value") %>%
        as.data.frame() %>%
        tidyr::extract(col = variable,into = c("variable", "jurisdiction.id"), regex = "([A-Z]+)\\[([0-9]+)") %>%
        tidyr::pivot_wider(id_cols = c(rep, step, jurisdiction.id), names_from = "variable", values_from = "value")

      # We pulled the number from the variable names (e.g. L[1] == 43.42 -> jurisdiction == "1" L == 43.42)
      # However, we need to convert to numeric i.e. jurisdiction == 1 instead of jurisdiction == "1"
      self$res_long$jurisdiction.id <- as.numeric(self$res_long$jurisdiction.id)

      # Do a left join on the long table and the jurisdiction information
      self$res_long <- left_join(self$res_long, self$inputs$jurisdiction, self$inputs$jurisdiction, by = "jurisdiction.id")


      # hard-coded for first jurisdiction
      # Sarah to generalize
      # Compute time-varying costs:
      self$res_long <- self$res_long %>%
        # merge jurisdiction-level costs here with dplyr.
        # This is a linear function from now, it can be non-linear
        mutate(CNPI = L * self$oi$tau * self$res_long$cost.npi)


      # let's imagine we define the probability of the outbreak event as p in the model
      p = 0.1

      # This is where we summarize costs by jurisdiction:
      self$summary_jurisdiction <- self$res_long %>%
        group_by(rep, jurisdiction.id) %>%
        summarise(CNPI = sum(CNPI),
                  R_final = max(R)) %>%
        # Compute other costs:
        mutate(CH = R_final * (self$oi$r*self$oi$w + self$oi$o),
               CSURV = self$oi$C_surv,
               C = p * (CH + CSURV) + CNPI)


      # This is where we summarize costs overall:
      self$summary_all <- self$summary_jurisdiction %>%
        group_by(rep) %>%
        select(-jurisdiction.id) %>%
        summarise(across(everything(),.fns = ~sum(.x)))

    }


  )
)
