#------------------------------------------------------------------------------#
# Code for "The value of environmental surveillance for pandemic response"
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# OdinSim model class
#
# An R6 class to represents odin models
#------------------------------------------------------------------------------#

OdinSim <- R6::R6Class(
  classname = "OdinSim",
  inherit = R6Sim,
  public = list(

    #' @field o odin model object
    o = NULL,

    #' @field odin_parms vector of odin user parameter values
    odin_parms = NULL,

    #' @field res day-level results
    res = NULL,

    #' @field res_long day-level results in long format
    res_long = NULL,

    #' @field summary_jurisdiction jurisdiction-replication -level summary results
    summary_jurisdiction = NULL,

    #' @field summary_all replication-level summary results
    summary_all = NULL,

    #' @field summary aggregate summary results
    summary = NULL,

    #' @description
    #' Create a new `OdinSim` object.
    #' @param odin_file odin model file under R/odin_models. Should have an .R extension.
    #' @param inputs_file spreadsheet with model inputs
    #' @param model_name optional model name (useful when using the same odin model file)
    #' @param ... additional inputs to te odin model.
    #' @return s new `OdinSim` object.
    initialize = function(odin_file, inputs_file, model_name = "", ...) {
      super$initialize(name = odin_file)

      model_path <- paste0("./R/odin_models/", odin_file)

      odin_workdir <- paste0("./cpp/", paste0(substr(odin_file,
        start = 1,
        stop = nchar(odin_file) - 2
      ), "_", model_name), "/")

      # Get (and set) inputs from spreadsheet:
      self$get_inputs(inputs_file)

      # set default parameters as inputs
      self$set_default_params()

      # Pre-process inputs (which will also assign any new parameters to the odin model)
      self$pre_process_inputs()

      # Create odin constructor (this is an R6 class):
      odin_constructor <- odin::odin(model_path, workdir = odin_workdir)

      # Create list of odin inputs
      self$odin_parms <- odin_constructor$private_fields$user

      odin_inputs <- self$inputs[self$odin_parms]

      odin_inputs_missing <- self$odin_parms[!self$odin_parms %in% names(self$inputs)]

      if (!length(odin_inputs_missing) == 0) {
        stop(paste0("odin inputs missing: ", paste0(odin_inputs_missing, collapse = ", ")))
      }

      # Build odin model:
      self$o <- do.call(odin_constructor$new, odin_inputs)

      # The model now should be ready to be simulated.
      return(invisible(self))
    },
    pre_process_inputs = function() {
      stop("Function must be implemented in class that inherits this model")
    },

    #' @description
    #' Set Input
    #'
    #' @details
    #' Use this function to add a new input to the model object.
    #'
    #' @param name character string defining the input name
    #' @param value input value. Can be a single value, a list or a vector.
    #' @param type optional character string defining the type of input. Useful when one wants to only write inputs of a certain type to json.
    set_input = function(name, value, type = NA_character_) {
      # set.seed(1234)

      # If the model has been instantiated, and the input type is odin, set odin inptut
      if (!is.null(self$o)) {
        # check that this is an input the odin model needs:

        if (name %in% self$odin_parms) {
          input <- list()
          input[[name]] <- value

          do.call(self$o$set_user, input)
        }
      }

      super$set_input(name = name, value = value, type = type)

      return(invisible(self))
    },

    # simulates the model for a set of replications
    simulate = function(step, y = NULL, use_names = TRUE, reps = 1, seed = 1234) {
      # set.seed(seed)

      # result comes as an array of matrices
      res <- replicate(n = reps, expr = self$o$run(step, y = y, use_names = use_names))

      # convert to a nice list
      res_list <- lapply(seq(dim(res)[3]), function(x) {
        res[, , x] %>%
          as.data.frame(.) %>%
          mutate(rep = x)
      })

      # return as a data.frame
      self$res <- do.call(rbind, res_list)

      return(invisible(self))
    },

    # set default parameters from model inputs:
    set_default_params = function() {
      if (!is.null(self$inputs$parameters)) {
        for (i in 1:nrow(self$inputs$parameters)) {
          self$set_input(name = self$inputs$parameters$parameter[i], value = self$inputs$parameters$baseline[i])
        }
      }
      return(invisible(self))
    }
  )
)
