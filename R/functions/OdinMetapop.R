


#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# OdinSim metapopulation class
#
# This is the specific model class.
#------------------------------------------------------------------------------#

OdinMetapop <- R6::R6Class(
  classname = "OdinMetapop",
  inherit = OdinSim,
  public = list(

    # Pre-processes model inputs before running the simulation.
    pre_process_inputs = function() {

      # Create Mixing matrix and scales it as to align wit the desired R0.

      # Population density vector:
      pop <- self$inputs$jurisdiction$population[1:self$inputs$nr_patches] / sum(self$inputs$jurisdiction$population[1:self$inputs$nr_patches])


      # First, compute a normalized mixing matrix M, where all *rows* sum to 1.
      # This mixing matrix describes the distribution of contacts from an infected individual in place i to other individuals in place j.

      names.jurisdictions <- self$inputs$jurisdiction$jurisdiction.name
      n.jurisdictions <- length(names.jurisdictions)

      names.modes <- c("Home", "Work", "Other")
      n.modes <- length(names.modes)

      # Following the same notation, k represents normalized contact rates across modes, by jurisdictions
      # k can be thought of the intensity of contacts across these mixing modes for each jurisdiction

      # For this version, the jurisdictions spend the same time across mixing modes.
      # This can be changed by making k.mat an input itself
      k.mat <- matrix(data = c(self$inputs$k_home,self$inputs$k_work_travel,self$inputs$k_other), byrow = T, nrow = n.jurisdictions, ncol = n.modes, dimnames = list(names.jurisdictions, names.modes))

      # stop if rows are not normalized to 1
      stopifnot(!all((rowSums(k.mat) - 1)^2 > 1e-10))

      # now, for each mode, we have a mode-specific mixing matrix:
      home_mixing <-  diag(nrow = n.jurisdictions, ncol = n.jurisdictions, names = names.jurisdictions)

      other_mixing <-  diag(nrow = n.jurisdictions, ncol = n.jurisdictions, names = names.jurisdictions)

      # Construct work commuting based on "intra" and "inter" commuting rates
      # "Intra commuting" rate is the rate of commuting within commuting areas, as defined in the jurisdiction table.
      # "Inter commuting" is the rate for the other jurisdictions
      # The diagonal then is the rest of 1-row sums.
      # This makes it so we can define the mixing matrices a minimum set of inputs.

      # Set coordination following the jurisdiction block

      commuting_areas <- unique(self$inputs$jurisdiction$commuting.area.id)

      work_travel_mixing <- matrix(data = 0, nrow = self$inputs$nr_patches, ncol = self$inputs$nr_patches, dimnames = list(self$inputs$jurisdiction$jurisdiction.name, self$inputs$jurisdiction$jurisdiction.name))

      for (i in names.jurisdictions) {

        intra.rate <- self$inputs$commuting_area$intra.commuting.rate[self$inputs$jurisdiction$commuting.area.id[self$inputs$jurisdiction$jurisdiction.name==i]]
        inter.rate <- self$inputs$commuting_area$inter.commuting.rate[self$inputs$jurisdiction$commuting.area.id[self$inputs$jurisdiction$jurisdiction.name==i]]
        for (j in names.jurisdictions) {

          is_same_commuting_area <- self$inputs$jurisdiction$commuting.area.id[self$inputs$jurisdiction$jurisdiction.name==i] == self$inputs$jurisdiction$commuting.area.id[self$inputs$jurisdiction$jurisdiction.name==j]

          # Only assign here if this is not the diagonal
          if(i!=j) {
            work_travel_mixing[i,j] <- ifelse(is_same_commuting_area,intra.rate, inter.rate)
          }

        }

        # Then, assign the diagonal with the remainder - i.e., people who work in their home counties or do not travel.
        work_travel_mixing[i,i] <- 1-sum(work_travel_mixing[i,])

      }

      # Concatenate all mixing mdoes into one array (order matters!):
      M <- array(c(home_mixing, work_travel_mixing, other_mixing),dim = c(n.jurisdictions, n.jurisdictions, n.modes), dimnames = list(names.jurisdictions, names.jurisdictions, names.modes))

      # This is the same as Mij.

      # Overall mixing matrix is the sum of element-wise multiplications:
      mixing_matrix <- matrix(data = 0, nrow = n.jurisdictions, ncol = n.jurisdictions,dimnames = list(names.jurisdictions, names.jurisdictions))

      for(m in names.modes) {

        mixing_matrix <- mixing_matrix + k.mat[,m] * M[,,m]

      }

      # Rowsums should be 1:
      stopifnot(all(abs(rowSums(mixing_matrix) - 1) < 1e-3))


      # Now that we have Mij, we need to align it with a specified R0.

      # This approach does so by assuming that cbeta is uniform across juridictions.

      # Simplistic approach: Results match, but R0 may not match when population sizes

      # are different.

      tau.eff <- (1/self$inputs$delta + 1/self$inputs$gamma)

      # Here, I calculate cbeta as the population average:
      cbeta <- self$inputs$R0 / tau.eff


      # C beta risk ratios are inputs
      c_rr <- self$inputs$jurisdiction$mixing.risk.ratio


      # This assumes that the first jurisdiction is the reference jurisdiction
      # From c = sum_i(c_i*pop_i), we can derive c1
      c_1 = cbeta / (pop[1] + sum(c_rr[2:self$inputs$nr_patches] * pop[2:self$inputs$nr_patches]))

      cbeta_i <- c_1 * c_rr

      k_i <- cbeta_i * pop / cbeta

      # k_i also happens to be this:
      #k_i <- self$inputs$jurisdiction$relative.mixing * pop / sum(pop)

      # As opposed to cbeta * mixing_matrix
      # We don't need to multiply by the population because S is a prop of the population.
      #final_mixing_matrix <- cbeta * t(pop * t(mixing_matrix))

      # This is an element-wise multiplication, row by row:
      final_mixing_matrix <- t(cbeta_i * t(mixing_matrix))

      # If the mixing is homogenous, this should be the same as:
      # final_mixing_matrix <- cbeta * mixing_matrix

      self$set_input("beta", final_mixing_matrix)

      # R0 verification:
      # R0 should be this:
      R0_verif <- sum(self$inputs$beta %*% pop * tau.eff)

      stopifnot(abs(R0_verif - self$inputs$R0)/self$inputs$R0 < 1e-2)

      # NPI Coordination:
      # fist, create a matrix assuming no coordination at all

      coordination <- matrix(data = 0, nrow = self$inputs$nr_patches, ncol = self$inputs$nr_patches, dimnames = list(self$inputs$jurisdiction$jurisdiction.name, self$inputs$jurisdiction$jurisdiction.name))

      # Set coordination according to the selected option

      # No coordination - each jurisdiction uses their own
      if(self$inputs$coordination_mode == 0) {

      diag(coordination) <- 1

      # Full coordination mode
      } else if (self$inputs$coordination_mode == 1) {
        coordination[,] <- 1
      # Blocked coordination mode
      } else if(self$inputs$coordination_mode == 2) {

      # Set coordination following the jurisdiction block

      npi_blocks <- unique(self$inputs$jurisdiction$npi.coordination.block)

      # Set coordination values for each block:

      for(i in npi_blocks) {

        jurisdictions <- self$inputs$jurisdiction$jurisdiction.name[self$inputs$jurisdiction$npi.coordination.block == i]

        coordination[jurisdictions,jurisdictions] <- 1

      }

      # Inputs from coordination are taken from the spreadsheet:
      } else if(self$inputs$coordination_mode == 3) {

        coordination <- as.matrix(self$inputs$coordination[1:self$inputs$nr_patches,1:self$inputs$nr_patches+1])

      }

      # Set coordination inputs:
      self$set_input("C", coordination)

      # Population sizes
      self$set_input("S0", self$inputs$jurisdiction$S0[1:self$inputs$nr_patches])

      # Initial Infected:
      self$set_input("I0", self$inputs$jurisdiction$I0[1:self$inputs$nr_patches])

      # Calculate cost of illness
      healthcosts <- self$inputs$healthcosts

      # The cost of being ill for each stage
      healthcosts$cost_unwellness_for_given_stage <- healthcosts$DALY_weight * healthcosts$disease_duration * self$inputs$VSLY
      # Severe and critical illness started as mild, so we must get that cost
      # And add
      mild_cost <- filter(healthcosts, severity=="mild")$cost_unwellness_for_given_stage
      # if the disease stage is severe or critical, we started off as mild. We must add
      # the cost of being ill during the
      # mild period to the total cost of being ill
      # Then we add in the hospital cost
      healthcosts <- healthcosts %>% mutate(cost_with_mild_included_and_hospitalization = cost_unwellness_for_given_stage
                                           + ifelse(severity %in% c("severe", "critical"), mild_cost, 0)
                                           + hospital_cost)

      cost_per_new_infection_by_severity <- with(healthcosts, cost_with_mild_included_and_hospitalization*disease_state_prevalence)


      self$set_input("average_health_cost_per_infection",
                     sum(cost_per_new_infection_by_severity))


      # Set NPI Cost per day for each intervention level
      self$set_input("jurisdiction",
                     self$inputs$jurisdiction %>%
                       mutate(cost.npi = self$inputs$cost_max_npi * self$inputs$gdp_per_capita / 365 / self$inputs$L_max)
                     )

      # Calibrate parameters of logistic functions for varying IFR

      # Based on time (i.e., due to improved standard of care)
      if(as.logical(self$inputs$time_varying_IFR)) {
        # Scale parameter of the RR risk function
        self$set_input("r_scale_factor", calib_logistic_fn(y_max = self$inputs$r_terminal_RR, x_mid_point = self$inputs$t_mid_IFR, x_trans = self$inputs$t_trans_IFR, x_vector = seq.default(from = 0, to = 365, by = 0.01)))
      }

      # Based on prevalence (i.e., hospital utilization)
      if(as.logical(self$inputs$prevalence_varying_IFR)) {
        # Scale parameter of the RR risk function
        # Recall the logistic function needs to start at zero, so we need to add 1 to the RR later.
        self$set_input("H_overload_scale_factor", calib_logistic_fn(y_max = self$inputs$H_overload_IFR_RR - 1, x_mid_point = self$inputs$I_mid_IFR, x_trans = (self$inputs$I_max_IFR - self$inputs$I_mid_IFR)*2, x_vector = seq.default(from = 0, to = self$inputs$I_max_IFR * 1.5, by = 0.0001)))
      }

      return(invisible(self))

    },

    #' Computes summaries based on model results across replications
    #'
    #' @export
    post_process = function() {

      # select only the variables we want for the summary variable
      required_jurisdiction_variables = c("rep", "step", "L", "NPI", "S", "E", "P", "I", "R")

      # save results into long format
      self$res_long <- self$res %>%
        select(starts_with(required_jurisdiction_variables, ignore.case = FALSE)) %>%
        select(-starts_with( c("L_star", "I_lag", "S_E"))) %>%
        tidyr::pivot_longer(cols = -c(rep, step), names_to = "variable", values_to = "value") %>%
        as.data.frame() %>%
        tidyr::extract(col = variable,into = c("variable", "jurisdiction.id"), regex = "([A-Z]+)\\[([0-9]+)") %>%
        tidyr::pivot_wider(id_cols = c(rep, step, jurisdiction.id), names_from = "variable", values_from = "value")



      self$res_long$jurisdiction.id <- as.numeric(self$res_long$jurisdiction.id)


      # Compute deaths allowing for time-varying IFR:
      self$res_long <- left_join(self$res_long, self$inputs$jurisdiction, by = "jurisdiction.id") %>%
        # Cost might also be formulated as dependent on effectiveness (tau):
        mutate(CNPI = floor(L) * cost.npi) %>%
        mutate(prevalence = I / population) %>%
        mutate(IFR = self$inputs$r) %>%
        # Change IFR based on time:
        {if(as.logical(self$inputs$time_varying_IFR)) {
          mutate(.,IFR_time_mult =  (1 - logistic_fn(y_max = self$inputs$r_terminal_RR, x_mid_point = self$inputs$t_mid_IFR, x_trans = self$inputs$t_trans_IFR,x = .$step, scale_factor = self$inputs$r_scale_factor))) %>%
            mutate(.,IFR = IFR * IFR_time_mult)
        } else . } %>%
        # Change IFR based on prevalence (i.e., hospital overload):
        {if(as.logical(self$inputs$prevalence_varying_IFR)) {
          mutate(.,IFR_hosp_mult = (1 + logistic_fn(y_max = self$inputs$H_overload_IFR_RR - 1 , x_mid_point = self$inputs$I_mid_IFR, x_trans = (self$inputs$I_max_IFR - self$inputs$I_mid_IFR)*2,x = .$prevalence, scale_factor = self$inputs$H_overload_scale_factor))) %>%
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
      # This set up assumes R is absorbing, if there is loss of immunity,
      # all the above would have to be computed within the model.
      self$res_long <- self$res_long %>%
        mutate(new_recoveries = c(0, diff(R)))

      # PNL note: average_health_cost_
      # The health cost is the number of infected * the avg cost per infection
      self$res_long <- self$res_long %>%
        mutate(health_cost_of_illness = new_recoveries * self$inputs$average_health_cost_per_infection) %>%
        mutate(per_capita_health_cost_of_illness = health_cost_of_illness / population)

      # Summarize costs by jurisdiction
      self$summary_jurisdiction <- self$res_long %>%
        group_by(rep, jurisdiction.id) %>%
        summarise(CNPI = self$inputs$p_disease_event * sum(CNPI),
                  Deaths.per.100k = self$inputs$p_disease_event * sum(Deaths.per.100k),
                  population = mean(population),
                  health_cost_of_illness = self$inputs$p_disease_event * sum(health_cost_of_illness),
                  per_capita_health_cost_of_illness = sum(health_cost_of_illness)/population,
                  R_final = max(R), .groups = "keep") %>%
        mutate(CH_deaths = Deaths.per.100k * self$inputs$ly_lost_death * self$inputs$VSLY / 10^5,
               CH_illness = self$inputs$p_disease_event * per_capita_health_cost_of_illness) %>%
        mutate(CH = CH_deaths + CH_illness,
               CSURV = self$inputs$C_surv,
               C = CSURV + CH + CNPI) %>%
        relocate(C, CNPI, CH, CH_deaths, CH_illness)

      # Summarize overall costs
      self$summary_all <- self$summary_jurisdiction %>%
        group_by(rep) %>%
        select(-jurisdiction.id) %>%
        summarise(across(everything(),.fns = ~sum(.x))) %>%
        # Assumes equal population sizes - we may change this.
        mutate(CNPI = CNPI / self$inputs$nr_patches) %>%
        mutate(CH = CH / self$inputs$nr_patches) %>%
        mutate(CH_deaths = CH_deaths / self$inputs$nr_patches) %>%
        mutate(CH_illness = CH_illness / self$inputs$nr_patches) %>%
        mutate(C = C / self$inputs$nr_patches) %>%
        mutate(Deaths.per.100k = Deaths.per.100k / self$inputs$nr_patches) %>%
        mutate(per_capita_health_cost_of_illness = per_capita_health_cost_of_illness / self$inputs$nr_patches)
      # summarize across replications:
      self$summary <- self$summary_all %>%
        group_by() %>%
        summarise(across(everything(),.fns = ~mean(.x)))

      return(invisible(self))
    },

    # Simulate function
    simulate = function(step = 0:365, y = NULL, use_names = TRUE, reps = 100){

      self$pre_process_inputs()

      super$simulate(step = step, y = y, use_names= use_names, reps = reps)

      self$post_process()

      return(self$summary)

    }

  )
)
