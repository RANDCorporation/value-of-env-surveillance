


#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# odinpbm metapopulation class
#
# This is the model class, and self-contains the model.
#------------------------------------------------------------------------------#

odinmetapop <- R6::R6Class(
  classname = "odinmetapop",
  inherit = odinpbm,
  public = list(

    #' Collect model-specific default inputs.
    #'
    #' @export
    collect_default_inputs = function() {

      # start from the inputs collected by the parent class
      inputs <- super$collect_default_inputs()


      # add in inputs that are unique to this model.
      inputs$nr_patches <- as.integer((self$inputs$settings %>% filter(setting=="nr_patches"))$value)


      # Scale Mixing matrix to the desired R0:

      # Here, we scale the mixing matrix such that
      # R0 = tau.eff * k * beta_pop
      # beta_pop = p' beta_matrix p.

      # Population density vector:
      pop <- self$inputs$jurisdiction$population[1:inputs$nr_patches] / sum(self$inputs$jurisdiction$population[1:inputs$nr_patches])

      # Original beta matrix:
      beta_input <- as.matrix(self$inputs$beta[1:inputs$nr_patches,1:inputs$nr_patches+1])

      inputs$npi_coord  <- as.matrix(self$inputs$coordination[1:inputs$nr_patches,1:inputs$nr_patches+1])

      beta_pop_input <- t(pop) %*% beta_input %*% pop

      # Effective infectious period
      tau.eff <- (1/inputs$delta + 1/inputs$gamma)

      # k factor aligns overall mixing matrix and infectious periods to a desired R0:
      k <- as.numeric(inputs$R0 / (tau.eff * beta_pop_input))

      # Hence, overall beta is fixed here:
      inputs$beta <- k * beta_input


      # Calculate cost of illness
      healthcosts <- self$inputs$healthcosts

      # The cost of being ill for each stage

      healthcosts$cost_unwellness_for_given_stage <- healthcosts$DALY_weight * healthcosts$disease_duration * inputs$VSLY
      # Severe and critical illness started as mild, so we must get that cost
      # And add
      mild_cost = filter(healthcosts, severity=="mild")$cost_unwellness_for_given_stage
      # if the disease stage is severe or critical, we started off as mild. We must add
      # the cost of being ill during the
      # mild period to the total cost of being ill
      # Then we add in the hospital cost
      # PNL note: Can we avoid using with, and use dplyr::mutate instead.
      # This is more of a style opinion rather than a hard-and-fast rule.
      healthcosts <- healthcosts %>%mutate(cost_with_mild_included_and_hospitalization = cost_unwellness_for_given_stage
                       + ifelse(severity %in% c("severe", "critical"), mild_cost, 0)
                       + hospital_cost)

      cost_per_new_infection_by_severity <- with(healthcosts, cost_with_mild_included_and_hospitalization*disease_state_prevalence)
      self$inputs$average_health_cost_per_infection <- sum(cost_per_new_infection_by_severity)


      # Calibrate parameters of logistic functions here:

      if(as.logical(inputs$time_varying_IFR)) {
        # Scale parameter of the RR risk function
        inputs$r_scale_factor <- calib_logistic_fn(y_max = inputs$r_terminal_RR, x_mid_point = inputs$t_mid_IFR, x_trans = inputs$t_trans_IFR, x_vector = seq.default(from = 0, to = 365, by = 0.01))
      }

      if(as.logical(inputs$prevalence_varying_IFR)) {
        # Scale parameter of the RR risk function
        # Recall the logistic function needs to start at zero, so we need to add 1 to the RR later.
        inputs$H_overload_scale_factor <- calib_logistic_fn(y_max = inputs$H_overload_IFR_RR - 1, x_mid_point = inputs$I_mid_IFR, x_trans = (inputs$I_max_IFR - inputs$I_mid_IFR)*2, x_vector = seq.default(from = 0, to = inputs$I_max_IFR * 1.5, by = 0.0001))
      }

      return(inputs)
    },

    #' Computes summaries based on model results across replications
    #'
    #' @export
    post_process = function() {

      # select only the variables we want for the summary variable

      required_jurisdiction_variables = c("rep", "step", "L", "R", "I")

      # save results into long format
      self$res_long <- self$res %>%
        select(starts_with(required_jurisdiction_variables, ignore.case = FALSE)) %>%
        select(-starts_with( c("L_star", "I_lag"))) %>%
        tidyr::pivot_longer(cols = -c(rep, step), names_to = "variable", values_to = "value") %>%
        as.data.frame() %>%
        tidyr::extract(col = variable,into = c("variable", "jurisdiction.id"), regex = "([A-Z]+)\\[([0-9]+)") %>%
        tidyr::pivot_wider(id_cols = c(rep, step, jurisdiction.id), names_from = "variable", values_from = "value")



      self$res_long$jurisdiction.id <- as.numeric(self$res_long$jurisdiction.id)
      # Compute deaths:
      self$res_long <- left_join(self$res_long, self$inputs$jurisdiction, by = "jurisdiction.id") %>%
        # Cost might also be formulated as dependent on effectiveness (tau):
        mutate(CNPI = L * cost.npi) %>%
        mutate(prevalence = I / population) %>%
        mutate(IFR = self$oi$r) %>%
        # Change IFR based on time:
        {if(as.logical(self$oi$time_varying_IFR)) {
          mutate(.,IFR_time_mult =  (1 - logistic_fn(y_max = self$oi$r_terminal_RR, x_mid_point = self$oi$t_mid_IFR, x_trans = self$oi$t_trans_IFR,x = .$step, scale_factor = self$oi$r_scale_factor))) %>%
            mutate(.,IFR = IFR * IFR_time_mult)
        } else . } %>%
        # Change IFR based on prevalence (i.e., hospital overload):
        {if(as.logical(self$oi$prevalence_varying_IFR)) {
          mutate(.,IFR_hosp_mult = (1 + logistic_fn(y_max = self$oi$H_overload_IFR_RR - 1 , x_mid_point = self$oi$I_mid_IFR, x_trans = (self$oi$I_max_IFR - self$oi$I_mid_IFR)*2,x = .$prevalence, scale_factor = self$oi$H_overload_scale_factor))) %>%
            mutate(.,IFR = IFR * IFR_hosp_mult)
        } else . } %>%
        arrange(rep, jurisdiction.id, step) %>%
        group_by(rep, jurisdiction.id) %>%
        mutate(new_recoveries = c(0, diff(R))) %>%
        # Compute deaths:
        mutate(Deaths.per.100k = round((new_recoveries * IFR / population) * 100000))

      # The new_recoveries is the difference in the recovered count at each step, which should
      # reflect the infection count, albeit offset in time.
      # We pre-pend a 0 to capture the first set of differences (first entry will be the
      # num of people in the recovered compartiment at time 1, second will be the number at time 2
      # minus the number with recovered status at time 1, etc)
      # TODO: THIS ONLY WORKS IF R IS ABSORBING AND NO ONE LEAVES THE STATUS R.
      # WE WILL HAVE TO CHANGE THIS IF WE ADD VARIANTS OR PEOPLE BECOME SUSCEPTIBLE
      # AGAIN

      #PNL note: why do that again?
      self$res_long <- self$res_long %>%
        mutate(new_recoveries = c(0, diff(R)))

      # PNL note: average_health_cost_
      # The health cost is the number of infected this round times the avg cost per infection
      self$res_long <- self$res_long %>%
        mutate(health_cost_of_illness = new_recoveries * self$inputs$average_health_cost_per_infection) %>%
        mutate(per_capita_health_cost_of_illness = health_cost_of_illness / population)



      # Summarize costs by jurisdiction

      self$summary_jurisdiction <- self$res_long %>%
        group_by(rep, jurisdiction.id) %>%
        summarise(CNPI = self$oi$p_disease_event * sum(CNPI),
                  Deaths.per.100k = self$oi$p_disease_event * sum(Deaths.per.100k),
                  population = mean(population),
                  health_cost_of_illness = self$oi$p_disease_event * sum(health_cost_of_illness),
                  per_capita_health_cost_of_illness = sum(health_cost_of_illness)/population,
                  R_final = max(R), .groups = "keep") %>%
        mutate(CH = self$oi$p_disease_event * R_final * (self$oi$r*self$oi$w + self$oi$o),
               CSURV = self$oi$C_surv,
               C = CSURV + CH + CNPI )

      # Summarize overall costs
      self$summary_all <- self$summary_jurisdiction %>%
        group_by(rep) %>%
        select(-jurisdiction.id) %>%
        summarise(across(everything(),.fns = ~sum(.x))) %>%
        # Assumes equal population sizes - we may change this.
        mutate(CNPI = CNPI / self$oi$nr_patches) %>%
        mutate(Deaths.per.100k = Deaths.per.100k / self$oi$nr_patches) %>%
        mutate(per_capita_health_cost_of_illness = per_capita_health_cost_of_illness / self$oi$nr_patches)
      # summarize across replications:
      self$summary <- self$summary_all %>%
        group_by() %>%
        summarise(across(everything(),.fns = ~mean(.x)))

    }


  )
)







































































































































































